module Domain.Action.UI.Pass
  ( getMultimodalPassAvailablePasses,
    postMultimodalPassSelect,
    getMultimodalPassList,
    postMultimodalPassVerify,
    passOrderStatusHandler,
    postMultimodalPassSwitchDeviceId,
    getMultimodalPassTransactions,
    createPassReconEntry,
  )
where

import qualified API.Types.UI.Pass as PassAPI
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.FRFSRecon as Recon
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PassCategory as DPassCategory
import qualified Domain.Types.PassType as DPassType
import qualified Domain.Types.PassVerifyTransaction as DPassVerifyTransaction
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.PurchasedPassPayment as DPurchasedPassPayment
import qualified Environment
import qualified EulerHS.Prelude as EHS
import qualified JsonLogic
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps.Types
import qualified Kernel.External.Payment.Interface as PaymentInterface
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Id as Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JLU
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.PaymentVendorSplits as PaymentVendorSplits
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.Translations as QT
import qualified Storage.Queries.FRFSRecon as QRecon
import qualified Storage.Queries.PassCategoryExtra as QPassCategory
import qualified Storage.Queries.PassExtra as QPass
import qualified Storage.Queries.PassTypeExtra as QPassType
import qualified Storage.Queries.PassVerifyTransaction as QPassVerifyTransaction
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment
import Tools.Error
import qualified Tools.Payment as TPayment

getMultimodalPassAvailablePasses ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Kernel.Prelude.Maybe Lang.Language ->
    Environment.Flow [PassAPI.PassInfoAPIEntity]
  )
getMultimodalPassAvailablePasses (mbPersonId, _merchantId) mbLanguage = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  -- Get all pass categories for the merchant operating city
  passCategories <- B.runInReplica $ QPassCategory.findAllByMerchantOperatingCityId person.merchantOperatingCityId

  -- For each category, get pass types and passes
  forM passCategories $ \category -> do
    passTypes <- B.runInReplica $ QPassType.findAllByPassCategoryId category.id

    -- Get all passes for these pass types
    allPasses <- forM passTypes $ \passType -> do
      passes <- B.runInReplica $ QPass.findAllByPassTypeIdAndEnabled passType.id True
      return (passType, passes)

    -- Flatten passes and build API entities
    let flatPasses = concatMap snd allPasses
    passAPIEntities <- mapM (buildPassAPIEntity mbLanguage personId) flatPasses

    return $
      PassAPI.PassInfoAPIEntity
        { passCategory = buildPassCategoryAPIEntity category,
          passTypes = map (buildPassTypeAPIEntity . fst) allPasses,
          passes = passAPIEntities
        }

postMultimodalPassSelect ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DPass.Pass ->
    Maybe HighPrecMoney ->
    Maybe DT.Day ->
    Text ->
    Environment.Flow PassAPI.PassSelectionAPIEntity
  )
postMultimodalPassSelect (mbPersonId, merchantId) passId mbAmount mbStartDay deviceId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  pass <- B.runInReplica $ QPass.findById passId >>= fromMaybeM (PassNotFound passId.getId)

  unless pass.enable $ throwError (InvalidRequest "Pass is not enabled")

  -- Check if user has all required documents
  validateRequiredDocuments person pass.documentsRequired

  -- Use Redis lock to prevent race condition when purchasing pass
  let lockKey = mkPassPurchaseLockKey personId pass.passTypeId
  Redis.whenWithLockRedisAndReturnValue lockKey 60 (purchasePassWithPayment person pass merchantId personId mbAmount mbStartDay deviceId) >>= \case
    Left _ -> do
      logError $ "Pass purchase already in progress for personId: " <> personId.getId <> " and passTypeId: " <> pass.passTypeId.getId
      throwError (InvalidRequest "Pass purchase already in progress, please try again")
    Right result -> return result

purchasePassWithPayment ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    EventStreamFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    EsqDBReplicaFlow m r
  ) =>
  DP.Person ->
  DPass.Pass ->
  Id.Id DM.Merchant ->
  Id.Id DP.Person ->
  Maybe HighPrecMoney ->
  Maybe DT.Day ->
  Text ->
  m PassAPI.PassSelectionAPIEntity
purchasePassWithPayment person pass merchantId personId mbAmount mbStartDay deviceId = do
  -- Check if pass is already purchased and active
  now <- getCurrentTime
  purchasedPassPaymentId <- generateGUID
  paymentOrderId <- generateGUID
  paymentOrderShortId <- generateShortId

  istTime <- getLocalCurrentTime (19800 :: Seconds)
  passNumber <- getNextPassNumber
  let startDate = fromMaybe (DT.utctDay istTime) mbStartDay
      endDate =
        case pass.maxValidDays of
          Just 30 ->
            -- Special handling for 30 days: next month, previous day
            let (y, m, d) = DT.toGregorian startDate
                -- Helper to handle wrap-around at December
                nextMonthYear
                  | d == 1 = (y, m)
                  | m == 12 = (y + 1, 1)
                  | otherwise = (y, m + 1)
                (nextY, nextM) = nextMonthYear
                daysInNextMonth = DT.gregorianMonthLength nextY nextM
                endDay = if d > daysInNextMonth || d == 1 then daysInNextMonth else d - 1
                candidateEndDate = DT.fromGregorian nextY nextM endDay
             in candidateEndDate
          Just days -> DT.addDays (fromIntegral days) startDate
          Nothing -> startDate

  mbSamePass <- QPurchasedPass.findPassByPersonIdAndPassTypeIdAndDeviceId personId merchantId pass.passTypeId deviceId

  passType <- QPassType.findById pass.passTypeId >>= fromMaybeM (PassTypeNotFound pass.passTypeId.getId)

  -- let initialStatus = if pass.amount == 0 then DPurchasedPass.Active else DPurchasedPass.Pending
  (purchasedPassId, balance, amount) <-
    case passType.passClass of
      DPassType.Unlimited ->
        handleUnlimitedPass mbSamePass startDate endDate passNumber
      DPassType.StoredValuePass ->
        handleStoredValuePass mbSamePass startDate endDate passNumber now

  let purchasedPassPayment =
        DPurchasedPassPayment.PurchasedPassPayment
          { id = purchasedPassPaymentId,
            orderId = paymentOrderId,
            purchasedPassId = purchasedPassId,
            personId = personId,
            startDate,
            endDate,
            balance = balance,
            status = DPurchasedPass.Pending,
            amount = amount,
            passCode = pass.code,
            passName = pass.name,
            merchantId = pass.merchantId,
            merchantOperatingCityId = pass.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  -- update purchasedPassPayment

  orderAmount <-
    case passType.passClass of
      DPassType.Unlimited -> pure pass.amount
      DPassType.StoredValuePass -> fromMaybeM (InvalidRequest "Top up amount not provided") mbAmount

  mbPaymentOrder <-
    if orderAmount > 0
      then do
        customerEmail <- fromMaybe "noreply@nammayatri.in" <$> mapM decrypt person.email
        customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt

        -- Get split settlement details
        let itemDetails =
              [ PaymentVendorSplits.ItemDetail
                  { itemId = pass.id.getId,
                    itemTransactionId = purchasedPassPaymentId.getId,
                    amount = orderAmount
                  }
              ]
        vendorSplitList <- PaymentVendorSplits.createVendorSplit merchantId person.merchantOperatingCityId TPayment.FRFSPassPurchase itemDetails
        isSplitEnabled <- TPayment.getIsSplitEnabled merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase
        isPercentageSplitEnabled <- TPayment.getIsPercentageSplit merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase
        splitSettlementDetails <- TPayment.mkUnaggregatedSplitSettlementDetails isSplitEnabled orderAmount vendorSplitList isPercentageSplitEnabled
        let createOrderReq =
              Payment.CreateOrderReq
                { orderId = paymentOrderId.getId,
                  orderShortId = paymentOrderShortId.getShortId,
                  amount = orderAmount,
                  customerId = personId.getId,
                  customerEmail,
                  customerPhone,
                  customerFirstName = person.firstName,
                  customerLastName = person.lastName,
                  createMandate = Nothing,
                  mandateMaxAmount = Nothing,
                  mandateFrequency = Nothing,
                  mandateStartDate = Nothing,
                  mandateEndDate = Nothing,
                  optionsGetUpiDeepLinks = Nothing,
                  metadataExpiryInMins = Nothing,
                  metadataGatewayReferenceId = Nothing,
                  splitSettlementDetails,
                  basket = Nothing
                }

        let commonMerchantId = Id.cast @DM.Merchant @DPayment.Merchant merchantId
            commonPersonId = Id.cast @DP.Person @DPayment.Person personId
            createOrderCall = TPayment.createOrder merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase (Just personId.getId) person.clientSdkVersion
        mbPaymentOrderValidity <- TPayment.getPaymentOrderValidity merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase
        DPayment.createOrderService commonMerchantId (Just $ Id.cast person.merchantOperatingCityId) commonPersonId mbPaymentOrderValidity Nothing TPayment.FRFSPassPurchase createOrderReq createOrderCall
      else return Nothing
  QPurchasedPassPayment.create purchasedPassPayment
  return $
    PassAPI.PassSelectionAPIEntity
      { purchasedPassId = purchasedPassId,
        paymentOrder = mbPaymentOrder
      }
  where
    buildPurchasedPass newPurchasedPassId benefitType benefitValue passAmount passNumber startDate endDate = do
      now <- getCurrentTime
      return $
        DPurchasedPass.PurchasedPass
          { id = newPurchasedPassId,
            passNumber = fromIntegral passNumber,
            personId = personId,
            startDate = startDate,
            endDate = endDate,
            passTypeId = pass.passTypeId,
            passCode = pass.code,
            passName = pass.name,
            passDescription = pass.description,
            passAmount = passAmount,
            benefitDescription = pass.benefitDescription,
            benefitType = benefitType,
            benefitValue = benefitValue,
            applicableVehicleServiceTiers = pass.applicableVehicleServiceTiers,
            maxValidTrips = pass.maxValidTrips,
            maxValidDays = pass.maxValidDays,
            deviceSwitchCount = 0,
            deviceId,
            status = DPurchasedPass.Pending,
            merchantId = pass.merchantId,
            usedTripCount = Just 0,
            verificationValidity = pass.verificationValidity,
            merchantOperatingCityId = pass.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            minAmount = pass.minAmount,
            maxAmount = pass.maxAmount,
            passTypeCode = pass.passTypeCode
          }

    getNextPassNumber :: (CacheFlow m r, EsqDBFlow m r) => m Integer
    getNextPassNumber =
      Hedis.safeGet makeLastPassNumberKey >>= \case
        Just (_ :: Integer) -> do
          Hedis.incr makeLastPassNumberKey
        Nothing -> do
          lastPassNumber <- QPurchasedPass.getLastPassNumber
          cacheLastPassNumber (fromIntegral lastPassNumber)
          Hedis.incr makeLastPassNumberKey

    cacheLastPassNumber :: (CacheFlow m r) => Integer -> m ()
    cacheLastPassNumber lastPassNumber = do
      Hedis.set makeLastPassNumberKey lastPassNumber

    makeLastPassNumberKey :: Text
    makeLastPassNumberKey = "CachedQueries:Pass:NextPassNumber"

    sumHighPrecMoney :: HighPrecMoney -> HighPrecMoney -> HighPrecMoney
    sumHighPrecMoney (HighPrecMoney amount1) (HighPrecMoney amount2) = HighPrecMoney (amount1 + amount2)

    isOutOfRange :: HighPrecMoney -> DPurchasedPass.PurchasedPass -> Bool
    isOutOfRange amount samePass =
      amount > samePass.maxAmount || amount < samePass.minAmount

    handleUnlimitedPass mbSamePass startDate endDate passNumber =
      case mbSamePass of
        Just samePass -> do
          let overlaps (aStart, aEnd) (bStart, bEnd) = not (aEnd < bStart || bEnd < aStart)
              hasDateOverlap activePass =
                overlaps (activePass.startDate, activePass.endDate) (startDate, endDate)
          when (samePass.status `elem` [DPurchasedPass.Active, DPurchasedPass.PreBooked] && hasDateOverlap samePass) $
            throwError (InvalidRequest "You already have an active pass of this type in the selected dates")
          return (samePass.id, samePass.passAmount, samePass.passAmount)
        Nothing -> do
          newPurchasedPassId <- generateGUID
          let (benefitType, benefitValue) = case pass.benefit of
                Nothing -> (Nothing, Nothing)
                Just DPass.FullSaving -> (Just DPurchasedPass.FullSaving, Nothing)
                Just (DPass.FixedSaving amount) -> (Just DPurchasedPass.FixedSaving, Just amount)
                Just (DPass.PercentageSaving percentage) -> (Just DPurchasedPass.PercentageSaving, Just percentage)
          purchasedPass <- buildPurchasedPass newPurchasedPassId benefitType benefitValue pass.amount passNumber startDate endDate
          QPurchasedPass.create purchasedPass
          return (newPurchasedPassId, pass.amount, pass.amount)

    handleStoredValuePass mbSamePass startDate endDate passNumber now =
      case mbSamePass of
        Just samePass ->
          if samePass.status `elem` [DPurchasedPass.Active, DPurchasedPass.PreBooked]
            then do
              topUpAmount <- fromMaybeM (InvalidRequest "Top up amount not provided") mbAmount
              let newAmount = sumHighPrecMoney samePass.passAmount topUpAmount
              when (isOutOfRange newAmount samePass) $
                throwError (InvalidRequest "Top up amount is out of range")
              QPurchasedPass.updateByPrimaryKey
                samePass
                  { DPurchasedPass.passAmount = newAmount,
                    DPurchasedPass.updatedAt = now
                  }
              previousPurchasedPassPayments <- QPurchasedPassPayment.findAllByPurchasedPassIdAndStatus (Just 1) (Just 0) samePass.id DPurchasedPass.Active startDate
              forM_ previousPurchasedPassPayments $ \purchasedPassPayment ->
                QPurchasedPassPayment.updateByPrimaryKey
                  purchasedPassPayment
                    { DPurchasedPassPayment.status = DPurchasedPass.Invalidated,
                      DPurchasedPassPayment.updatedAt = now
                    }
              return (samePass.id, newAmount, topUpAmount)
            else do
              newPurchasedPassId <- generateGUID
              passAmount <- fromMaybeM (InvalidRequest "Pass amount not found") mbAmount
              purchasedPass <- buildPurchasedPass newPurchasedPassId Nothing Nothing passAmount passNumber startDate endDate
              QPurchasedPass.create purchasedPass
              return (newPurchasedPassId, passAmount, passAmount)
        Nothing -> do
          newPurchasedPassId <- generateGUID
          passAmount <- fromMaybeM (InvalidRequest "Pass amount not found") mbAmount
          purchasedPass <- buildPurchasedPass newPurchasedPassId Nothing Nothing passAmount passNumber startDate endDate
          QPurchasedPass.create purchasedPass
          return (newPurchasedPassId, passAmount, passAmount)

-- Generate Redis lock key for pass purchase
mkPassPurchaseLockKey :: Id.Id DP.Person -> Id.Id DPassType.PassType -> Text
mkPassPurchaseLockKey personId passTypeId =
  "PassPurchase:PersonId:" <> personId.getId <> ":PassTypeId:" <> passTypeId.getId

-- Validate that the person has all required documents for the pass
validateRequiredDocuments :: (MonadFlow m) => DP.Person -> [DPass.PassDocumentType] -> m ()
validateRequiredDocuments person requiredDocs = do
  let missingDocs = filter (not . hasDocument person) requiredDocs
  unless (null missingDocs) $ do
    let missingDocNames = show missingDocs
    throwError $ InvalidRequest $ "Missing required documents: " <> missingDocNames

-- Check if person has a specific document
hasDocument :: DP.Person -> DPass.PassDocumentType -> Bool
hasDocument person docType = case docType of
  DPass.ProfilePicture -> isJust person.profilePicture
  DPass.Aadhaar -> person.aadhaarVerified

-- Construct message keys for Pass fields
mkPassMessageKey :: Id.Id DPass.Pass -> Text -> Text
mkPassMessageKey passId name = passId.getId <> "-" <> name

buildPassCategoryAPIEntity :: DPassCategory.PassCategory -> PassAPI.PassCategoryAPIEntity
buildPassCategoryAPIEntity category =
  PassAPI.PassCategoryAPIEntity
    { id = category.id,
      name = category.name,
      description = category.description
    }

buildPassTypeAPIEntity :: DPassType.PassType -> PassAPI.PassTypeAPIEntity
buildPassTypeAPIEntity passType =
  PassAPI.PassTypeAPIEntity
    { id = passType.id,
      name = passType.name,
      catchline = passType.catchline,
      title = passType.title,
      description = passType.description
    }

buildPassAPIEntity ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Maybe Lang.Language ->
  Id.Id DP.Person ->
  DPass.Pass ->
  m PassAPI.PassAPIEntity
buildPassAPIEntity mbLanguage personId pass = do
  -- Get person details for eligibility check
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  -- Create person data for eligibility check with relevant fields
  let personData =
        A.object
          [ "id" A..= (person.id.getId :: Text),
            "gender" A..= (show person.gender :: Text),
            "dateOfBirth" A..= person.dateOfBirth,
            "city" A..= (show person.currentCity :: Text),
            "merchantOperatingCityId" A..= (person.merchantOperatingCityId.getId :: Text),
            "hasDisability" A..= person.hasDisability,
            "customerNammaTags" A..= person.customerNammaTags,
            "aadhaarVerified" A..= (person.aadhaarVerified :: Bool),
            "enabled" A..= (person.enabled :: Bool),
            "blocked" A..= (person.blocked :: Bool)
          ]

  -- Check purchase eligibility using JSON logic
  eligibility <- checkEligibility pass.purchaseEligibilityJsonLogic personData

  let language = fromMaybe Lang.ENGLISH mbLanguage
  let moid = person.merchantOperatingCityId
  nameTranslation <- QT.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid (mkPassMessageKey pass.id "name") language
  benefitTranslation <- QT.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid (mkPassMessageKey pass.id "benefitDescription") language
  descriptionTranslation <- QT.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid (mkPassMessageKey pass.id "description") language
  let name = maybe pass.name (Just . (.message)) nameTranslation
  let benefitDescription = maybe pass.benefitDescription (.message) benefitTranslation
  let description = maybe pass.description (Just . (.message)) descriptionTranslation
  return $
    PassAPI.PassAPIEntity
      { id = pass.id,
        amount = pass.amount,
        savings = Nothing, -- TODO: Calculate based on benefit
        benefit = pass.benefit,
        benefitDescription = benefitDescription,
        vehicleServiceTierType = pass.applicableVehicleServiceTiers,
        maxTrips = pass.maxValidTrips,
        maxDays = pass.maxValidDays,
        documentsRequired = pass.documentsRequired,
        eligibility = eligibility,
        name = name,
        description = description,
        code = pass.code,
        autoApply = pass.autoApply
      }

-- Build Pass API Entity from PurchasedPass snapshot (for viewing purchased passes)
buildPassAPIEntityFromPurchasedPass ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Maybe Lang.Language ->
  Id.Id DP.Person ->
  DPurchasedPass.PurchasedPass ->
  m PassAPI.PassAPIEntity
buildPassAPIEntityFromPurchasedPass mbLanguage _personId purchasedPass = do
  -- Reconstruct Pass.Benefit from flattened benefitType and benefitValue
  let benefit = case (purchasedPass.benefitType, purchasedPass.benefitValue) of
        (Nothing, _) -> Nothing
        (Just DPurchasedPass.FullSaving, _) -> Just DPass.FullSaving
        (Just DPurchasedPass.FixedSaving, Just value) -> Just (DPass.FixedSaving value)
        (Just DPurchasedPass.PercentageSaving, Just value) -> Just (DPass.PercentageSaving value)
        _ -> Nothing

  let language = fromMaybe Lang.ENGLISH mbLanguage
  let passId = Id.cast purchasedPass.id :: Id.Id DPass.Pass
  let moid = purchasedPass.merchantOperatingCityId
  nameTranslation <- QT.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid (mkPassMessageKey passId "name") language
  benefitTranslation <- QT.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid (mkPassMessageKey passId "benefitDescription") language
  descriptionTranslation <- QT.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid (mkPassMessageKey passId "description") language

  let name = maybe purchasedPass.passName (Just . (.message)) nameTranslation
  let benefitDescription = maybe purchasedPass.benefitDescription (.message) benefitTranslation
  let description = maybe purchasedPass.passDescription (Just . (.message)) descriptionTranslation
  return $
    PassAPI.PassAPIEntity
      { id = Id.cast purchasedPass.id,
        amount = purchasedPass.passAmount,
        savings = Nothing,
        benefit = benefit,
        benefitDescription = benefitDescription,
        vehicleServiceTierType = purchasedPass.applicableVehicleServiceTiers,
        maxTrips = purchasedPass.maxValidTrips,
        maxDays = purchasedPass.maxValidDays,
        description = description,
        documentsRequired = [],
        eligibility = True,
        name = name,
        code = purchasedPass.passCode,
        autoApply = False -- Auto-apply not relevant for already purchased passes
      }

-- Check eligibility using JSON logic rules
checkEligibility :: (MonadFlow m) => [A.Value] -> A.Value -> m Bool
checkEligibility logics personData = do
  if null logics
    then return True -- If no rules defined, consider eligible
    else do
      -- Evaluate all JSON logic rules against person data
      results <- forM logics $ \logic -> do
        let result = JsonLogic.jsonLogicEither logic personData
        case result of
          Right (A.Bool b) -> return b
          Right _ -> return False -- Non-boolean results considered as False
          Left err -> do
            logError $ "JSON logic evaluation error: " <> show err
            return False -- Errors considered as not eligible
            -- All rules must be True for eligibility
      return $ and results

-- Build PurchasedPass API Entity
buildPurchasedPassAPIEntity ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Maybe Lang.Language ->
  Id.Id DP.Person ->
  Text ->
  DT.Day ->
  DPurchasedPass.PurchasedPass ->
  m PassAPI.PurchasedPassAPIEntity
buildPurchasedPassAPIEntity mbLanguage personId deviceId today purchasedPass = do
  let deviceMismatch = purchasedPass.deviceId /= deviceId
  passType <- B.runInReplica $ QPassType.findById purchasedPass.passTypeId >>= fromMaybeM (PassTypeNotFound purchasedPass.passTypeId.getId)
  passCategory <- B.runInReplica $ QPassCategory.findById passType.passCategoryId >>= fromMaybeM (PassCategoryNotFound passType.passCategoryId.getId)
  futureRenewals <- QPurchasedPassPayment.findAllByPurchasedPassIdAndStatusStartDateGreaterThan Nothing Nothing purchasedPass.id DPurchasedPass.PreBooked purchasedPass.endDate

  let tripsLeft = case purchasedPass.maxValidTrips of
        Just maxTrips -> Just $ max 0 (maxTrips - fromMaybe 0 purchasedPass.usedTripCount)
        Nothing -> Nothing

  passAPIEntity <- buildPassAPIEntityFromPurchasedPass mbLanguage personId purchasedPass
  let passDetailsEntity =
        PassAPI.PassDetailsAPIEntity
          { category = buildPassCategoryAPIEntity passCategory,
            passType = buildPassTypeAPIEntity passType,
            passDetails = passAPIEntity
          }

  let daysToExpire = fromIntegral $ DT.diffDays purchasedPass.endDate today

  lastVerifiedVehicleNumber <- QPassVerifyTransaction.findLastVerifiedVehicleNumberByPurchasePassId purchasedPass.id
  return $
    PassAPI.PurchasedPassAPIEntity
      { id = purchasedPass.id,
        passNumber = let s = show purchasedPass.passNumber in T.pack $ replicate (8 - length s) '0' ++ s,
        passEntity = passDetailsEntity,
        tripsLeft = tripsLeft,
        lastVerifiedVehicleNumber,
        status = purchasedPass.status,
        startDate = purchasedPass.startDate,
        deviceMismatch,
        deviceSwitchAllowed = purchasedPass.deviceSwitchCount == 0,
        daysToExpire = daysToExpire,
        purchaseDate = DT.utctDay purchasedPass.createdAt,
        expiryDate = purchasedPass.endDate,
        futureRenewals = buildPurchasedPassPaymentAPIEntity <$> futureRenewals
      }

-- Webhook Handler for Pass Payment Status Updates
passOrderStatusHandler ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Id.Id DOrder.PaymentOrder ->
  Id.Id DM.Merchant ->
  Payment.TransactionStatus ->
  m (DPayment.PaymentFulfillmentStatus, Maybe Text, Maybe Text)
passOrderStatusHandler paymentOrderId _merchantId status = do
  logInfo $ "Pass payment webhook handler called for paymentOrderId: " <> paymentOrderId.getId
  mbPurchasedPassPayment <- QPurchasedPassPayment.findOneByPaymentOrderId paymentOrderId
  mbPurchasedPass <- maybe (pure Nothing) (QPurchasedPass.findById . (.purchasedPassId)) mbPurchasedPassPayment
  case (mbPurchasedPassPayment, mbPurchasedPass) of
    (Just purchasedPassPayment, Just purchasedPass) -> do
      istTime <- getLocalCurrentTime (19800 :: Seconds)
      let mbPassStatus = convertPaymentStatusToPurchasedPassStatus (purchasedPassPayment.startDate > DT.utctDay istTime) status
      whenJust mbPassStatus $ \passStatus -> do
        QPurchasedPassPayment.updateStatusByOrderId passStatus paymentOrderId
        when (purchasedPass.status `notElem` [DPurchasedPass.Active, DPurchasedPass.PreBooked]) $ do
          QPurchasedPass.updatePurchaseData purchasedPass.id purchasedPassPayment.startDate purchasedPassPayment.endDate passStatus
      case mbPassStatus of
        Just DPurchasedPass.Active -> return (DPayment.FulfillmentSucceeded, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
        Just DPurchasedPass.PreBooked -> return (DPayment.FulfillmentSucceeded, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
        Just DPurchasedPass.Expired -> return (DPayment.FulfillmentSucceeded, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
        Just DPurchasedPass.Failed -> return (DPayment.FulfillmentFailed, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
        Just DPurchasedPass.RefundPending -> return (DPayment.FulfillmentRefundPending, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
        Just DPurchasedPass.RefundInitiated -> return (DPayment.FulfillmentRefundInitiated, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
        Just DPurchasedPass.RefundFailed -> return (DPayment.FulfillmentRefundFailed, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
        Just DPurchasedPass.Refunded -> return (DPayment.FulfillmentRefunded, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
        _ -> return (DPayment.FulfillmentPending, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
    _ -> do
      logError $ "Purchased pass not found for paymentOrderId: " <> paymentOrderId.getId
      return (DPayment.FulfillmentPending, Nothing, Nothing)
  where
    convertPaymentStatusToPurchasedPassStatus futureDatePass = \case
      Payment.CHARGED -> if futureDatePass then Just DPurchasedPass.PreBooked else Just DPurchasedPass.Active
      Payment.AUTHENTICATION_FAILED -> Just DPurchasedPass.Failed
      Payment.AUTHORIZATION_FAILED -> Just DPurchasedPass.Failed
      Payment.JUSPAY_DECLINED -> Just DPurchasedPass.Failed
      Payment.CANCELLED -> Just DPurchasedPass.Failed
      Payment.AUTO_REFUNDED -> Just DPurchasedPass.Refunded
      _ -> Nothing

createPassReconEntry ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r
  ) =>
  DPayment.PaymentStatusResp ->
  Text ->
  m ()
createPassReconEntry paymentStatusResponse transactionId = do
  case paymentStatusResponse.status of
    Payment.CHARGED -> do
      purchasedPassPayment <- QPurchasedPassPayment.findByPrimaryKey (Id.Id transactionId) >>= fromMaybeM (InvalidRequest $ "Purchase pass payment not found for id: " <> transactionId)
      bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback purchasedPassPayment.merchantOperatingCityId purchasedPassPayment.merchantId (show Spec.FRFS) (Utils.frfsVehicleCategoryToBecknVehicleCategory Spec.BUS) >>= fromMaybeM (InternalError "Beckn Config not found")
      mkPassReconEntry bapConfig purchasedPassPayment
    _ -> return ()
  where
    mkPassReconEntry bapConfig purchasedPassPayment = do
      let finderFee :: Price = mkPrice Nothing $ fromMaybe 0 $ (readMaybe . T.unpack) =<< bapConfig.buyerFinderFee
      now <- getCurrentTime
      reconId <- generateGUID
      let reconEntry =
            Recon.FRFSRecon
              { Recon.id = reconId,
                Recon.frfsTicketBookingId = Id.Id purchasedPassPayment.id.getId,
                Recon.networkOrderId = purchasedPassPayment.id.getId,
                Recon.collectorSubscriberId = bapConfig.subscriberId,
                Recon.receiverSubscriberId = "MTC bus pass",
                Recon.date = show now,
                Recon.time = show now,
                Recon.mobileNumber = Nothing,
                Recon.sourceStationCode = Nothing,
                Recon.destinationStationCode = Nothing,
                Recon.ticketQty = Nothing,
                Recon.ticketNumber = Nothing,
                Recon.transactionRefNumber = Nothing,
                Recon.transactionUUID = paymentStatusResponse.txnUUID,
                Recon.txnId = paymentStatusResponse.txnId,
                Recon.fare = mkPrice Nothing purchasedPassPayment.amount,
                Recon.buyerFinderFee = finderFee,
                Recon.totalOrderValue = mkPrice Nothing purchasedPassPayment.amount,
                Recon.settlementAmount = mkPrice Nothing purchasedPassPayment.amount,
                Recon.beneficiaryIFSC = Nothing,
                Recon.beneficiaryBankAccount = Nothing,
                Recon.collectorIFSC = bapConfig.bapIFSC,
                Recon.settlementReferenceNumber = Nothing,
                Recon.settlementDate = Nothing,
                Recon.differenceAmount = Nothing,
                Recon.message = Nothing,
                Recon.ticketStatus = Nothing,
                Recon.providerId = "MTC Bus Pass",
                Recon.providerName = "MTC Bus Pass Provider",
                Recon.entityType = Just Recon.BUS_PASS,
                Recon.reconStatus = Just Recon.PENDING,
                Recon.paymentGateway = Nothing,
                Recon.merchantId = Just purchasedPassPayment.merchantId,
                Recon.merchantOperatingCityId = Just purchasedPassPayment.merchantOperatingCityId,
                Recon.createdAt = now,
                Recon.updatedAt = now
              }
      void $ QRecon.create reconEntry

getMultimodalPassList ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Kernel.Prelude.Maybe Lang.Language ->
    Maybe Int ->
    Maybe Int ->
    Maybe DPurchasedPass.StatusType ->
    Text ->
    Environment.Flow [PassAPI.PurchasedPassAPIEntity]
  )
getMultimodalPassList (mbCallerPersonId, merchantId) mbLanguage mbLimitParam mbOffsetParam mbStatusParam deviceId = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")

  let mbStatus = case mbStatusParam of
        Just DPurchasedPass.Active -> Just [DPurchasedPass.Active, DPurchasedPass.PreBooked, DPurchasedPass.Expired]
        Just s -> Just [s]
        Nothing -> Nothing

  passEntities <- QPurchasedPass.findAllByPersonIdWithFilters personId merchantId mbStatus mbLimitParam mbOffsetParam
  istTime <- getLocalCurrentTime (19800 :: Seconds)
  let today = DT.utctDay istTime
  forM_ passEntities $ \purchasedPass -> do
    when (purchasedPass.status == DPurchasedPass.PreBooked && purchasedPass.startDate <= today) $ do
      QPurchasedPass.updateStatusById DPurchasedPass.Active purchasedPass.id purchasedPass.startDate purchasedPass.endDate

    when (purchasedPass.status `elem` [DPurchasedPass.Active, DPurchasedPass.PreBooked] && purchasedPass.endDate < today) $ do
      -- check if user has already renewed the pass
      allPreBookedPayments <- QPurchasedPassPayment.findAllByPurchasedPassIdAndStatus (Just 1) (Just 0) purchasedPass.id DPurchasedPass.PreBooked today
      let mbFirstPreBookedPayment = listToMaybe allPreBookedPayments
      case mbFirstPreBookedPayment of
        Just firstPreBookedPayment -> do
          let newStatus = if firstPreBookedPayment.startDate <= today then DPurchasedPass.Active else DPurchasedPass.PreBooked
          QPurchasedPass.updatePurchaseData purchasedPass.id firstPreBookedPayment.startDate firstPreBookedPayment.endDate newStatus
        Nothing -> do
          QPurchasedPass.updateStatusById DPurchasedPass.Expired purchasedPass.id purchasedPass.startDate purchasedPass.endDate

  allActivePurchasedPasses <- QPurchasedPass.findAllByPersonIdWithFilters personId merchantId mbStatus mbLimitParam mbOffsetParam
  let passWithSameDevice = filter (\pass -> pass.deviceId == deviceId) allActivePurchasedPasses
  let purchasedPasses = if null passWithSameDevice then allActivePurchasedPasses else passWithSameDevice

  mapM (buildPurchasedPassAPIEntity mbLanguage personId deviceId today) purchasedPasses

postMultimodalPassVerify ::
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DPurchasedPass.PurchasedPass ->
    PassAPI.PassVerifyReq ->
    Environment.Flow APISuccess.APISuccess
  )
postMultimodalPassVerify (mbCallerPersonId, merchantId) purchasedPassId passVerifyReq = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  purchasedPass <- QPurchasedPass.findById purchasedPassId >>= fromMaybeM (PurchasedPassNotFound purchasedPassId.getId)
  unless (purchasedPass.personId == personId) $ throwError AccessDenied
  istTime <- getLocalCurrentTime (19800 :: Seconds)
  unless (purchasedPass.startDate <= DT.utctDay istTime) $ throwError (InvalidRequest $ "Pass will be active from " <> show purchasedPass.startDate)
  passType <- QPassType.findById purchasedPass.passTypeId >>= fromMaybeM (PassTypeNotFound purchasedPass.passTypeId.getId)

  case passType.passClass of
    DPassType.Unlimited -> do
      integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig person.merchantOperatingCityId Enums.BUS DIBC.MULTIMODAL
      (integratedBPPConfig, vehicleLiveRouteInfo) <- JLU.getVehicleLiveRouteInfo integratedBPPConfigs passVerifyReq.vehicleNumber >>= fromMaybeM (InvalidRequest $ "Entered Bus OTP: " <> passVerifyReq.vehicleNumber <> " is invalid. Please check again.")
      unless (vehicleLiveRouteInfo.serviceType `elem` purchasedPass.applicableVehicleServiceTiers) $
        throwError $ InvalidRequest ("This pass is only " <> purchasedPass.benefitDescription)
      routeStopMapping <-
        case vehicleLiveRouteInfo.routeCode of
          Just routeCode ->
            withTryCatch
              "passVerify:getRouteStopMappingByRouteCodeInMem"
              (OTPRest.getRouteStopMappingByRouteCodeInMem routeCode integratedBPPConfig)
              >>= \case
                Left _ -> return []
                Right rsm -> return rsm
          Nothing -> return []
      let sourceStop =
            getNearestStop routeStopMapping passVerifyReq.currentLat passVerifyReq.currentLon
              <|> ((listToMaybe routeStopMapping) <&> (.stopCode))
          destinationStop = (safeTail routeStopMapping) <&> (.stopCode)
      passVerifyTransaction <- buildPassVerifyTransaction purchasedPass sourceStop destinationStop (Just purchasedPass.passAmount) Nothing person
      QPassVerifyTransaction.create passVerifyTransaction
    DPassType.StoredValuePass -> do
      case passVerifyReq.isEntry of
        Just True -> do
          when (purchasedPass.passAmount < purchasedPass.minAmount) $ throwError (InvalidRequest $ "Pass amount is less than minimum amount")
          stationCode <- passVerifyReq.stationCode & fromMaybeM (InvalidRequest $ "Station code not provided")
          passVerifyTransaction <- buildPassVerifyTransaction purchasedPass (Just stationCode) Nothing (Just purchasedPass.passAmount) passVerifyReq.gateId person
          QPassVerifyTransaction.create passVerifyTransaction
        Just False -> do
          -- can add min amount validation here if required
          now <- getCurrentTime
          deductableAmount <- passVerifyReq.amount & fromMaybeM (InvalidRequest $ "Amount not provided")
          let newAmount = subtractHighPrecMoney purchasedPass.passAmount deductableAmount
          QPassVerifyTransaction.updateOngoingPassVerifyTransaction
            passVerifyReq.stationCode
            passVerifyReq.gateId
            (Just newAmount)
            purchasedPassId
            passVerifyReq.vehicleNumber
            now
        Nothing -> do
          throwError $ (InvalidRequest "isEntry not provided")

  return APISuccess.Success
  where
    safeTail :: [a] -> Maybe a
    safeTail [] = Nothing
    safeTail [_] = Nothing
    safeTail xs = Just (last xs)

    getNearestStop _ Nothing _ = Nothing
    getNearestStop _ _ Nothing = Nothing
    getNearestStop [] _ _ = Nothing
    getNearestStop routeStopMapping (Just currentLat) (Just currentLon) =
      let nearestStop =
            minimumBy
              ( EHS.comparing
                  ( \rsm ->
                      distanceBetweenInMeters (LatLong currentLat currentLon) rsm.stopPoint
                  )
              )
              routeStopMapping
       in Just nearestStop.stopCode

    subtractHighPrecMoney :: HighPrecMoney -> HighPrecMoney -> HighPrecMoney
    subtractHighPrecMoney (HighPrecMoney amount1) (HighPrecMoney amount2) = HighPrecMoney (amount1 - amount2)

    buildPassVerifyTransaction purchasedPass sourceStop destinationStop openingAmount entryGateId person = do
      now <- getCurrentTime
      passVerifyTransactionId <- generateGUID
      let validTill =
            addUTCTime
              (intToNominalDiffTime (fromIntegral purchasedPass.verificationValidity))
              now
      return
        DPassVerifyTransaction.PassVerifyTransaction
          { id = passVerifyTransactionId,
            purchasePassId = purchasedPassId,
            validTill = validTill,
            verifiedAt = now,
            fleetId = passVerifyReq.vehicleNumber,
            sourceStopCode = sourceStop,
            destinationStopCode = destinationStop,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just person.merchantOperatingCityId,
            openingAmount = openingAmount,
            entryGateId = entryGateId,
            closingAmount = openingAmount,
            exitGateId = Nothing,
            purchasePassPaymentId = Nothing
          }

postMultimodalPassSwitchDeviceId ::
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    PassAPI.PassSwitchDeviceIdReq ->
    Environment.Flow APISuccess.APISuccess
  )
postMultimodalPassSwitchDeviceId (mbCallerPersonId, merchantId) req = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
  allActivePurchasedPasses <- QPurchasedPass.findAllByPersonIdWithFilters personId merchantId (Just [DPurchasedPass.Active, DPurchasedPass.PreBooked]) Nothing Nothing
  forM_ allActivePurchasedPasses $ \purchasedPass -> do
    when (purchasedPass.deviceId /= req.deviceId) $ do
      QPurchasedPass.updateDeviceIdById req.deviceId (purchasedPass.deviceSwitchCount + 1) purchasedPass.id
  return APISuccess.Success

getMultimodalPassTransactions ::
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow [PassAPI.PurchasedPassTransactionAPIEntity]
  )
getMultimodalPassTransactions (mbCallerPersonId, _) mbLimitParam mbOffsetParam = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
  let limit = fromMaybe 10 mbLimitParam
  let offset = fromMaybe 0 mbOffsetParam
  allPurchasedPassTransactions <- QPurchasedPassPayment.findAllWithPersonId (Just limit) (Just offset) personId
  return $ map buildPurchasedPassPaymentAPIEntity allPurchasedPassTransactions

buildPurchasedPassPaymentAPIEntity :: DPurchasedPassPayment.PurchasedPassPayment -> PassAPI.PurchasedPassTransactionAPIEntity
buildPurchasedPassPaymentAPIEntity purchasedPassPayment =
  PassAPI.PurchasedPassTransactionAPIEntity
    { startDate = purchasedPassPayment.startDate,
      endDate = purchasedPassPayment.endDate,
      status = purchasedPassPayment.status,
      amount = purchasedPassPayment.amount,
      passName = purchasedPassPayment.passName,
      passCode = purchasedPassPayment.passCode,
      createdAt = purchasedPassPayment.createdAt
    }
