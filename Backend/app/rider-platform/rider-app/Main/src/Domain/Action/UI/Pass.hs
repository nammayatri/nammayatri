module Domain.Action.UI.Pass
  ( getMultimodalPassAvailablePasses,
    postMultimodalPassSelect,
    getMultimodalPassStatus,
    getMultimodalPassList,
    webhookHandlerPass,
    postMultimodalPassVerify,
  )
where

import qualified API.Types.UI.Pass as PassAPI
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PassCategory as DPassCategory
import qualified Domain.Types.PassType as DPassType
import qualified Domain.Types.PassVerifyTransaction as DPassVerifyTransaction
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.PurchasedPassPayment as DPurchasedPassPayment
import qualified Environment
import qualified JsonLogic
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface as PaymentInterface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import Storage.Beam.Payment ()
import qualified Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import qualified Storage.Queries.PassCategoryExtra as QPassCategory
import qualified Storage.Queries.PassExtra as QPass
import qualified Storage.Queries.PassTypeExtra as QPassType
import qualified Storage.Queries.PassVerifyTransaction as QPassVerifyTransaction
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassExtra as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment
import Tools.Error
import qualified Tools.Payment as TPayment

getMultimodalPassAvailablePasses ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Environment.Flow [PassAPI.PassInfoAPIEntity]
  )
getMultimodalPassAvailablePasses (mbPersonId, _merchantId) = do
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
    passAPIEntities <- mapM (buildPassAPIEntity personId) flatPasses

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
    Maybe DT.Day ->
    Environment.Flow PassAPI.PassSelectionAPIEntity
  )
postMultimodalPassSelect (mbPersonId, merchantId) passId mbStartDay = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  pass <- B.runInReplica $ QPass.findById passId >>= fromMaybeM (PassNotFound passId.getId)

  unless pass.enable $ throwError (InvalidRequest "Pass is not enabled")

  -- Check if user has all required documents
  validateRequiredDocuments person pass.documentsRequired

  -- Use Redis lock to prevent race condition when purchasing pass
  let lockKey = mkPassPurchaseLockKey personId pass.passTypeId
  Redis.whenWithLockRedisAndReturnValue lockKey 60 (purchasePassWithPayment person pass merchantId personId mbStartDay) >>= \case
    Left _ -> do
      logError $ "Pass purchase already in progress for personId: " <> personId.getId <> " and passTypeId: " <> pass.passTypeId.getId
      throwError (InvalidRequest "Pass purchase already in progress, please try again")
    Right result -> return result

purchasePassWithPayment ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    EventStreamFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  DP.Person ->
  DPass.Pass ->
  Id.Id DM.Merchant ->
  Id.Id DP.Person ->
  Maybe DT.Day ->
  m PassAPI.PassSelectionAPIEntity
purchasePassWithPayment person pass merchantId personId mbStartDay = do
  -- Check if pass is already purchased and active
  existingActivePasses <- QPurchasedPass.findActivePassByPersonIdAndPassTypeId personId pass.passTypeId
  whenJust existingActivePasses $ \_ -> throwError (InvalidRequest "You already have an active pass of this type")

  now <- getCurrentTime
  purchasedPassId <- generateGUID
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

  let initialStatus = if pass.amount == 0 then DPurchasedPass.Active else DPurchasedPass.Pending
      -- Convert Pass.Benefit to flattened benefitType and benefitValue
      (benefitType, benefitValue) = case pass.benefit of
        Nothing -> (Nothing, Nothing)
        Just DPass.FullSaving -> (Just DPurchasedPass.FullSaving, Nothing)
        Just (DPass.FixedSaving amount) -> (Just DPurchasedPass.FixedSaving, Just amount)
        Just (DPass.PercentageSaving percentage) -> (Just DPurchasedPass.PercentageSaving, Just percentage)

      purchasedPass =
        DPurchasedPass.PurchasedPass
          { id = purchasedPassId,
            passNumber = fromIntegral passNumber,
            personId = personId,
            startDate,
            endDate,
            passTypeId = pass.passTypeId,
            passCode = pass.code,
            passName = pass.name,
            passAmount = pass.amount,
            benefitDescription = pass.benefitDescription,
            benefitType = benefitType,
            benefitValue = benefitValue,
            applicableVehicleServiceTiers = pass.applicableVehicleServiceTiers,
            maxValidTrips = pass.maxValidTrips,
            maxValidDays = pass.maxValidDays,
            status = initialStatus,
            merchantId = pass.merchantId,
            usedTripCount = Just 0,
            merchantOperatingCityId = pass.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

      purchasedPassPayment =
        DPurchasedPassPayment.PurchasedPassPayment
          { id = purchasedPassPaymentId,
            orderId = paymentOrderId,
            purchasedPassId = purchasedPassId,
            startDate,
            endDate,
            merchantId = Just pass.merchantId,
            merchantOperatingCityId = Just pass.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

  mbPaymentOrder <-
    if pass.amount > 0
      then do
        customerEmail <- fromMaybe "noreply@nammayatri.in" <$> mapM decrypt person.email
        customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt

        -- Get split settlement details
        isSplitEnabled <- TPayment.getIsSplitEnabled merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase
        isPercentageSplitEnabled <- TPayment.getIsPercentageSplit merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase
        splitSettlementDetails <- TPayment.mkSplitSettlementDetails isSplitEnabled pass.amount [] isPercentageSplitEnabled

        let createOrderReq =
              Payment.CreateOrderReq
                { orderId = paymentOrderId.getId,
                  orderShortId = paymentOrderShortId.getShortId,
                  amount = pass.amount,
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

        DPayment.createOrderService commonMerchantId (Just $ Id.cast person.merchantOperatingCityId) commonPersonId Nothing (show TPayment.FRFSPassPurchase) createOrderReq createOrderCall
      else return Nothing

  QPurchasedPass.create purchasedPass
  QPurchasedPassPayment.create purchasedPassPayment
  return $
    PassAPI.PassSelectionAPIEntity
      { purchasedPassId = purchasedPassId,
        paymentOrder = mbPaymentOrder
      }
  where
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

getMultimodalPassStatus ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DPurchasedPass.PurchasedPass ->
    Environment.Flow PassAPI.PurchasedPassAPIEntity
  )
getMultimodalPassStatus (mbPersonId, _merchantId) purchasedPassId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  purchasedPass <- B.runInReplica $ QPurchasedPass.findById purchasedPassId >>= fromMaybeM (PurchasedPassNotFound purchasedPassId.getId)

  unless (purchasedPass.personId == personId) $ throwError AccessDenied

  buildPurchasedPassAPIEntity personId purchasedPass

-- Helper functions

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

buildPassAPIEntity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id DP.Person -> DPass.Pass -> m PassAPI.PassAPIEntity
buildPassAPIEntity personId pass = do
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

  -- Extract service tier types from applicable vehicle service tiers
  vehicleServiceTiers <- catMaybes <$> mapM (\tierId -> B.runInReplica $ QFRFSVehicleServiceTier.findById tierId) pass.applicableVehicleServiceTiers
  let vehicleServiceTierTypes = map (._type) vehicleServiceTiers

  return $
    PassAPI.PassAPIEntity
      { id = pass.id,
        amount = pass.amount,
        savings = Nothing, -- TODO: Calculate based on benefit
        benefit = pass.benefit,
        benefitDescription = pass.benefitDescription,
        vehicleServiceTierType = vehicleServiceTierTypes,
        maxTrips = pass.maxValidTrips,
        maxDays = pass.maxValidDays,
        documentsRequired = pass.documentsRequired,
        eligibility = eligibility,
        name = pass.name,
        code = pass.code,
        autoApply = pass.autoApply
      }

-- Build Pass API Entity from PurchasedPass snapshot (for viewing purchased passes)
buildPassAPIEntityFromPurchasedPass :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id DP.Person -> DPurchasedPass.PurchasedPass -> m PassAPI.PassAPIEntity
buildPassAPIEntityFromPurchasedPass _personId purchasedPass = do
  -- Reconstruct Pass.Benefit from flattened benefitType and benefitValue
  let benefit = case (purchasedPass.benefitType, purchasedPass.benefitValue) of
        (Nothing, _) -> Nothing
        (Just DPurchasedPass.FullSaving, _) -> Just DPass.FullSaving
        (Just DPurchasedPass.FixedSaving, Just value) -> Just (DPass.FixedSaving value)
        (Just DPurchasedPass.PercentageSaving, Just value) -> Just (DPass.PercentageSaving value)
        _ -> Nothing

  -- Extract service tier types from applicable vehicle service tiers
  vehicleServiceTiers <- catMaybes <$> mapM (\tierId -> B.runInReplica $ QFRFSVehicleServiceTier.findById tierId) purchasedPass.applicableVehicleServiceTiers
  let vehicleServiceTierTypes = map (._type) vehicleServiceTiers

  return $
    PassAPI.PassAPIEntity
      { id = Id.cast purchasedPass.id,
        amount = purchasedPass.passAmount,
        savings = Nothing,
        benefit = benefit,
        benefitDescription = purchasedPass.benefitDescription,
        vehicleServiceTierType = vehicleServiceTierTypes,
        maxTrips = purchasedPass.maxValidTrips,
        maxDays = purchasedPass.maxValidDays,
        documentsRequired = [],
        eligibility = True,
        name = purchasedPass.passName,
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
buildPurchasedPassAPIEntity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id DP.Person -> DPurchasedPass.PurchasedPass -> m PassAPI.PurchasedPassAPIEntity
buildPurchasedPassAPIEntity personId purchasedPass = do
  passType <- B.runInReplica $ QPassType.findById purchasedPass.passTypeId >>= fromMaybeM (PassTypeNotFound purchasedPass.passTypeId.getId)
  passCategory <- B.runInReplica $ QPassCategory.findById passType.passCategoryId >>= fromMaybeM (PassCategoryNotFound passType.passCategoryId.getId)

  let tripsLeft = case purchasedPass.maxValidTrips of
        Just maxTrips -> Just $ max 0 (maxTrips - fromMaybe 0 purchasedPass.usedTripCount)
        Nothing -> Nothing

  passAPIEntity <- buildPassAPIEntityFromPurchasedPass personId purchasedPass
  let passDetailsEntity =
        PassAPI.PassDetailsAPIEntity
          { category = buildPassCategoryAPIEntity passCategory,
            passType = buildPassTypeAPIEntity passType,
            passDetails = passAPIEntity
          }

  istTime <- getLocalCurrentTime (19800 :: Seconds)
  let daysToExpire = fromIntegral $ DT.diffDays purchasedPass.endDate (DT.utctDay istTime)

  lastVerifiedVehicleNumber <- QPassVerifyTransaction.findLastVerifiedVehicleNumberByPurchasePassId purchasedPass.id
  return $
    PassAPI.PurchasedPassAPIEntity
      { id = purchasedPass.id,
        passNumber = let s = show purchasedPass.passNumber in T.pack $ (replicate (8 - length s) '0') ++ s,
        passEntity = passDetailsEntity,
        tripsLeft = tripsLeft,
        lastVerifiedVehicleNumber,
        status = purchasedPass.status,
        startDate = purchasedPass.startDate,
        daysToExpire = daysToExpire,
        purchaseDate = DT.utctDay purchasedPass.createdAt,
        expiryDate = purchasedPass.endDate
      }

-- Webhook Handler for Pass Payment Status Updates
webhookHandlerPass ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Id.Id DOrder.PaymentOrder ->
  Id.Id DM.Merchant ->
  Payment.TransactionStatus ->
  m ()
webhookHandlerPass paymentOrderId _merchantId status = do
  logInfo $ "Pass payment webhook handler called for paymentOrderId: " <> paymentOrderId.getId
  mbPurchasedPassPayment <- QPurchasedPassPayment.findOneByPaymentOrderId paymentOrderId
  mbPurchasedPass <- maybe (pure Nothing) (QPurchasedPass.findById . (.purchasedPassId)) mbPurchasedPassPayment
  case mbPurchasedPass of
    Nothing -> do
      logError $ "Purchased pass not found for paymentOrderId: " <> paymentOrderId.getId
      pure ()
    Just purchasedPass -> do
      istTime <- getLocalCurrentTime (19800 :: Seconds)
      let mbPassStatus = convertPaymentStatusToPurchasedPassStatus (purchasedPass.startDate > DT.utctDay istTime) status
      whenJust mbPassStatus $ \passStatus -> QPurchasedPass.updateStatusById passStatus purchasedPass.id
  where
    convertPaymentStatusToPurchasedPassStatus futureDatePass = \case
      Payment.CHARGED -> if futureDatePass then Just DPurchasedPass.PreBooked else Just DPurchasedPass.Active
      Payment.AUTHENTICATION_FAILED -> Just DPurchasedPass.Failed
      Payment.AUTHORIZATION_FAILED -> Just DPurchasedPass.Failed
      Payment.JUSPAY_DECLINED -> Just DPurchasedPass.Failed
      Payment.CANCELLED -> Just DPurchasedPass.Failed
      Payment.AUTO_REFUNDED -> Just DPurchasedPass.Refunded
      _ -> Nothing

getMultimodalPassList ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Maybe Int ->
    Maybe Int ->
    Maybe DPurchasedPass.StatusType ->
    Environment.Flow [PassAPI.PurchasedPassAPIEntity]
  )
getMultimodalPassList (mbCallerPersonId, merchantId) mbLimitParam mbOffsetParam mbStatusParam = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")

  let mbStatus = case mbStatusParam of
        Just DPurchasedPass.Active -> Just [DPurchasedPass.Active, DPurchasedPass.PreBooked]
        Just s -> Just [s]
        Nothing -> Nothing
  purchasedPasses <- QPurchasedPass.findAllByPersonIdWithFilters personId merchantId mbStatus mbLimitParam mbOffsetParam

  mapM (buildPurchasedPassAPIEntity personId) purchasedPasses

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
  id <- generateGUID
  now <- getCurrentTime
  let passVerifyTransaction =
        DPassVerifyTransaction.PassVerifyTransaction
          { id = id,
            purchasePassId = purchasedPassId,
            validTill = addUTCTime (intToNominalDiffTime 7200) now,
            verifiedAt = now,
            fleetId = passVerifyReq.vehicleNumber,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just person.merchantOperatingCityId
          }
  QPassVerifyTransaction.create passVerifyTransaction
  return APISuccess.Success
