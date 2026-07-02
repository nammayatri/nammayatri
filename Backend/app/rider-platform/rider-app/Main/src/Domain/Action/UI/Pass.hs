module Domain.Action.UI.Pass
  ( getMultimodalPassAvailablePasses,
    postMultimodalPassSelect,
    postMultimodalPassV2Select,
    getMultimodalPassList,
    getMultimodalPassListUtil,
    postMultimodalPassVerify,
    passOrderStatusHandler,
    postMultimodalPassSwitchDeviceId,
    postMultimodalPassResetDeviceSwitchCount,
    getMultimodalPassTransactions,
    postMultimodalPassActivateToday,
    postMultimodalPassActivateTodayUtil,
    postMultimodalPassSelectUtil,
    postMultimodalPassUploadProfilePicture,
    getMultimodalPassPhoto,
    fetchPassPhotoFromS3,
    postMultimodalPassUpdateProfilePictureUtil,
    buildPurchasedPassAPIEntity,
    postMultimodalPassSetPrefSrcAndDest,
  )
where

import qualified API.Types.UI.Pass as PassAPI
import qualified AWS.S3 as S3
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative ((<|>))
import Control.Monad.Extra (mapMaybeM)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKeyMap
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PassCategory as DPassCategory
import qualified Domain.Types.PassDetails as DPassDetails
import qualified Domain.Types.PassType as DPassType
import qualified Domain.Types.PassVerifyTransaction as DPassVerifyTransaction
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.PurchasedPassPayment as DPurchasedPassPayment
import qualified Domain.Types.RiderConfig
import qualified Environment
import qualified EulerHS.Prelude as EHS
import qualified IssueManagement.Common.UI.Issue as IssueCommon
import qualified IssueManagement.Domain.Action.UI.Issue as IssueAction
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.MediaFile as QMediaFile
import qualified JsonLogic
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps.Types
import qualified Kernel.External.Payment.Interface as PaymentInterface
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Sms.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Id as Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.JourneyLeg.Common.FRFSJourneyUtils as FRFSJourneyUtils
import qualified Lib.JourneyModule.Utils as JLU
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified SharedLogic.External.Nandi.Types
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Offer as SOffer
import qualified SharedLogic.PaymentVendorSplits as PaymentVendorSplits
import qualified SharedLogic.Utils as SLUtils
import Storage.Beam.IssueManagement ()
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.Pass as CQPass
import qualified Storage.CachedQueries.PassCategory as CQPassCategory
import qualified Storage.CachedQueries.PassType as CQPassType
import qualified Storage.CachedQueries.Translations as QT
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.PassCategoryExtra as QPassCategory
import qualified Storage.Queries.PassDetails as QPassDetails
import qualified Storage.Queries.PassExtra as QPass
import qualified Storage.Queries.PassTypeExtra as QPassType
import qualified Storage.Queries.PassVerifyTransaction as QPassVerifyTransaction
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment
import Tools.Error
import qualified Tools.Payment as TPayment
import Tools.SMS as Sms hiding (Success)
import qualified Tools.Wallet as TWallet

defaultDashboardDeviceId :: Text
defaultDashboardDeviceId = "dashboard-default-device-id"

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

  passCategories <- CQPassCategory.findAllByMerchantOperatingCityId person.merchantOperatingCityId
  when (null passCategories) $
    logError $ "getMultimodalPassAvailablePasses: no pass categories for mocId " <> person.merchantOperatingCityId.getId

  forM passCategories $ \category -> do
    passTypes <- CQPassType.findAllByPassCategoryId category.id
    when (null passTypes) $
      logError $ "getMultimodalPassAvailablePasses: no pass types for categoryId " <> category.id.getId

    allPasses <- forM passTypes $ \passType -> do
      passes <- CQPass.findAllByPassTypeIdAndEnabled passType.id True
      when (null passes) $
        logError $ "getMultimodalPassAvailablePasses: no enabled passes for passTypeId " <> passType.id.getId
      return (passType, passes)

    let flatPasses = concatMap snd allPasses
    -- Isolate per-pass failures so one bad pass cannot fail the whole response.
    passAPIEntities <- flip mapMaybeM flatPasses $ \pass ->
      withTryCatch ("getMultimodalPassAvailablePasses:buildPassAPIEntity:" <> pass.id.getId) (buildPassAPIEntity mbLanguage person pass)
        >>= either (const (pure Nothing)) (pure . Just)

    return $
      PassAPI.PassInfoAPIEntity
        { passCategory = buildPassCategoryAPIEntity category,
          passTypes = map (buildPassTypeAPIEntity . fst) allPasses,
          passes = passAPIEntities
        }

postMultimodalPassSelectUtil ::
  Bool ->
  ( Kernel.Prelude.Maybe (Id.Id DP.Person),
    Id.Id DM.Merchant
  ) ->
  Id.Id DPass.Pass ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Id.Id DMF.MediaFile) ->
  Maybe DT.Day ->
  Environment.Flow PassAPI.PassSelectionAPIEntity
postMultimodalPassSelectUtil isDashboard (mbPersonId, merchantId) passId mbDeviceIdParam mbImeiParam mbProfilePicture mbPassPhotoMediaId mbStartDay = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  pass <- B.runInReplica $ QPass.findById passId >>= fromMaybeM (PassNotFound passId.getId)

  unless pass.enable $ throwError (InvalidRequest "Pass is not enabled")

  -- Check if user has all required documents
  unless isDashboard $ do
    validateRequiredDocuments mbProfilePicture mbPassPhotoMediaId person pass.documentsRequired
  mbDeviceId <- if isDashboard then return Nothing else Just <$> getDeviceId person mbDeviceIdParam mbImeiParam

  -- Use Redis lock to prevent race condition when purchasing pass
  let lockKey = mkPassPurchaseLockKey personId pass.passTypeId
  Redis.whenWithLockRedisAndReturnValue lockKey 60 (purchasePassWithPayment isDashboard person pass merchantId personId mbStartDay mbDeviceId mbProfilePicture mbPassPhotoMediaId) >>= \case
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
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    EsqDBReplicaFlow m r,
    HasField "isMetroTestTransaction" r Bool
  ) =>
  Bool ->
  DP.Person ->
  DPass.Pass ->
  Id.Id DM.Merchant ->
  Id.Id DP.Person ->
  Maybe DT.Day ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Id.Id DMF.MediaFile) ->
  m PassAPI.PassSelectionAPIEntity
purchasePassWithPayment isDashboard person pass merchantId personId mbStartDay mbDeviceId mbProfilePicture mbPassPhotoMediaId = do
  -- Check if pass is already purchased and active
  now <- getCurrentTime
  purchasedPassPaymentId <- generateGUID
  paymentOrderId <- generateGUID
  paymentOrderShortId <- generateShortId
  let deviceId = fromMaybe defaultDashboardDeviceId mbDeviceId

  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId person.merchantOperatingCityId))
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
  istTime <- getLocalCurrentTime timeDiffFromUtc
  passNumber <- getNextPassNumber
  let startDate = fromMaybe (DT.utctDay istTime) mbStartDay
      endDate = calculatePassEndDate startDate pass.maxValidDays

  let (benefitType, benefitValue) = case pass.benefit of
        Nothing -> (Nothing, Nothing)
        Just DPass.FullSaving -> (Just DPurchasedPass.FullSaving, Nothing)
        Just (DPass.FixedSaving amount) -> (Just DPurchasedPass.FixedSaving, Just amount)
        Just (DPass.PercentageSaving percentage) -> (Just DPurchasedPass.PercentageSaving, Just percentage)
  mbSamePassDevice <- QPurchasedPass.findPassByPersonIdAndPassTypeIdAndDeviceId personId merchantId pass.passTypeId deviceId
  -- If there is no pass for the same device, try to find an existing Pending pass to reuse across devices and set it as mbSamePass
  mbSamePass <-
    case mbSamePassDevice of
      Just s -> return $ Just s
      Nothing -> do
        mbPending <- QPurchasedPass.findPendingPassByPersonIdAndPassTypeId personId merchantId pass.passTypeId
        case mbPending of
          Just pendingPass -> do
            -- don't update device id if existing different device id pass is active or prebooked
            QPurchasedPass.updateDeviceIdById deviceId 0 pendingPass.id
            logInfo $ "Reusing existing purchased pass " <> pendingPass.id.getId <> " and updated deviceId to " <> deviceId
            return $ Just pendingPass
          Nothing -> return Nothing

  -- restricting user from buying new pass in switched device before swtiching..
  unless isDashboard $ do
    otherTypePasses <- QPurchasedPass.findAllByPersonIdAndPassTypeIdAndStatus personId merchantId pass.passTypeId [DPurchasedPass.Active, DPurchasedPass.PreBooked]
    let isOtherActivePass p = maybe True (\samePass -> p.id /= samePass.id) mbSamePass
    whenJust (listToMaybe (filter isOtherActivePass otherTypePasses)) $ \otherPass -> do
      passes <- CQPass.findAllByPassTypeIdAndEnabled pass.passTypeId True
      let maxSwitchCount = case listToMaybe passes >>= (.passConfig) of
            Just pc -> pc.maxSwitchCount
            Nothing -> 1
      if otherPass.deviceSwitchCount < maxSwitchCount
        then throwError (InvalidRequest "You already have an active or pre-booked pass of this type on another device. Please switch it to this device instead of buying a new pass")
        else throwError (InvalidRequest "You already have an active or pre-booked pass of this type on another device and the device switch limit has been reached")

  passType <- CQPassType.findById pass.passTypeId
  let initialStatus = if pass.amount == 0 then DPurchasedPass.Active else DPurchasedPass.Pending
  purchasedPassId <-
    case mbSamePass of
      Just samePass -> do
        let passOverlaps = hasDateOverlap (samePass.startDate, samePass.endDate) (startDate, endDate)
        when (samePass.status `elem` [DPurchasedPass.Active, DPurchasedPass.PreBooked] && passOverlaps) $
          throwError (InvalidRequest "You already have an active pass of this type in the selected dates")
        futureRenewals <- QPurchasedPassPayment.findAllByPurchasedPassIdAndStatus Nothing Nothing samePass.id [DPurchasedPass.PreBooked, DPurchasedPass.Active] startDate
        let futureRenewalsOverlap = any (\futurePass -> hasDateOverlap (futurePass.startDate, futurePass.endDate) (startDate, endDate)) futureRenewals
        when futureRenewalsOverlap $
          throwError (InvalidRequest "You already have a future renewal of this pass in the selected dates")
        return samePass.id
      Nothing -> do
        newPurchasedPassId <- generateGUID
        let purchasedPass =
              DPurchasedPass.PurchasedPass
                { id = newPurchasedPassId,
                  passNumber = fromIntegral passNumber,
                  personId = personId,
                  startDate,
                  endDate,
                  passTypeId = pass.passTypeId,
                  passCode = pass.code,
                  passName = pass.name,
                  passDescription = pass.description,
                  passAmount = pass.amount,
                  benefitDescription = pass.benefitDescription,
                  benefitType = benefitType,
                  benefitValue = benefitValue,
                  applicableVehicleServiceTiers = pass.applicableVehicleServiceTiers,
                  maxValidTrips = pass.maxValidTrips,
                  maxValidDays = pass.maxValidDays,
                  deviceSwitchCount = 0,
                  profilePicture = mbProfilePicture <|> person.profilePicture,
                  passPhotoMediaId = mbPassPhotoMediaId,
                  deviceId = deviceId,
                  status = initialStatus,
                  merchantId = pass.merchantId,
                  usedTripCount = Just 0,
                  verificationValidity = pass.verificationValidity,
                  merchantOperatingCityId = pass.merchantOperatingCityId,
                  preferredDestination = Nothing,
                  preferredSource = Nothing,
                  createdAt = now,
                  updatedAt = now
                }

        QPurchasedPass.create purchasedPass
        return newPurchasedPassId

  let purchasedPassPayment =
        DPurchasedPassPayment.PurchasedPassPayment
          { id = purchasedPassPaymentId,
            orderId = paymentOrderId,
            purchasedPassId = purchasedPassId,
            personId = personId,
            startDate,
            endDate,
            benefitDescription = pass.benefitDescription,
            isDashboard = Just isDashboard,
            benefitType = benefitType,
            benefitValue = benefitValue,
            status = initialStatus,
            amount = pass.amount,
            passCode = pass.code,
            passName = pass.name,
            merchantId = pass.merchantId,
            passEnum = passType >>= (.passEnum),
            merchantOperatingCityId = pass.merchantOperatingCityId,
            profilePicture = mbProfilePicture <|> person.profilePicture,
            passPhotoMediaId = mbPassPhotoMediaId,
            createdAt = now,
            updatedAt = now
          }

  mbPaymentOrder <-
    if pass.amount > 0
      then do
        customerEmail <- fromMaybe "noreply@nammayatri.in" <$> mapM decrypt person.email
        customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
        -- Get split settlement details
        let itemDetails =
              [ PaymentVendorSplits.ItemDetail
                  { itemId = pass.id.getId,
                    itemTransactionId = purchasedPassPaymentId.getId,
                    amount = pass.amount
                  }
              ]
        vendorSplitList <- PaymentVendorSplits.createVendorSplit merchantId person.merchantOperatingCityId TPayment.FRFSPassPurchase itemDetails
        isSplitEnabled <- TPayment.getIsSplitEnabled merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase
        isPercentageSplitEnabled <- TPayment.getIsPercentageSplit merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase
        splitSettlementDetails <- TPayment.mkUnaggregatedSplitSettlementDetails isSplitEnabled pass.amount vendorSplitList isPercentageSplitEnabled False
        basket <- TPayment.mkOfferBasket merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase pass.amount 1
        staticCustomerId <- SLUtils.getStaticCustomerId person customerPhone
        nwAddress <- asks (.nwAddress)
        udf1 <- SLUtils.getPersonUdf1 person
        let createOrderReq =
              Payment.CreateOrderReq
                { orderId = paymentOrderId.getId,
                  orderShortId = paymentOrderShortId.getShortId,
                  amount = pass.amount,
                  customerId = staticCustomerId,
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
                  webhookUrl = Just nwAddress,
                  splitSettlementDetails,
                  basket = Just basket,
                  paymentRules = Nothing,
                  autoRefundPostSuccess = Nothing,
                  paymentFilter = Nothing,
                  udf1 = udf1
                }

        let commonMerchantId = Id.cast @DM.Merchant @DPayment.Merchant merchantId
            commonPersonId = Id.cast @DP.Person @DPayment.Person personId
            createOrderCall = TPayment.createOrder merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase (Just staticCustomerId) person.clientSdkVersion Nothing
        mbPaymentOrderValidity <- TPayment.getPaymentOrderValidity merchantId person.merchantOperatingCityId Nothing TPayment.FRFSPassPurchase
        isMetroTestTransaction <- asks (.isMetroTestTransaction)
        let createWalletCall = TWallet.createWallet merchantId person.merchantOperatingCityId
        DPayment.createOrderService commonMerchantId (Just $ Id.cast person.merchantOperatingCityId) commonPersonId mbPaymentOrderValidity Nothing TPayment.FRFSPassPurchase isMetroTestTransaction createOrderReq createOrderCall (Just createWalletCall) False (Just purchasedPassId.getId)
      else return Nothing
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

postMultimodalPassSelect ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DPass.Pass ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe DT.Day ->
    Environment.Flow PassAPI.PassSelectionAPIEntity
  )
postMultimodalPassSelect ids passId mbDeviceIdParam mbImeiParam mbPassPhotoMediaIdParam mbProfilePicture mbStartDay =
  postMultimodalPassSelectUtil False ids passId mbDeviceIdParam mbImeiParam mbProfilePicture (Id.Id <$> mbPassPhotoMediaIdParam) mbStartDay

postMultimodalPassV2Select ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DPass.Pass ->
    PassAPI.PassSelectReq ->
    Environment.Flow PassAPI.PassSelectionAPIEntity
  )
postMultimodalPassV2Select (mbPersonId, merchantId) passId req =
  postMultimodalPassSelectUtil False (mbPersonId, merchantId) passId Nothing (Just req.imeiNumber) req.profilePicture req.passPhotoMediaId (Just req.startDate)

-- Generate Redis lock key for pass purchase
mkPassPurchaseLockKey :: Id.Id DP.Person -> Id.Id DPassType.PassType -> Text
mkPassPurchaseLockKey personId passTypeId =
  "PassPurchase:PersonId:" <> personId.getId <> ":PassTypeId:" <> passTypeId.getId

-- Validate that the person has all required documents for the pass.
-- Profile picture is intentionally NOT mandatory at select time: it's consumed
-- if the client sends it, but its absence does not fail the purchase. The pass
-- moves to PhotoPending after payment and the user can upload the photo later
-- (uploadProfilePicture). Other documents (e.g. Aadhaar) remain mandatory.
validateRequiredDocuments :: (MonadFlow m) => Maybe Text -> Maybe (Id.Id DMF.MediaFile) -> DP.Person -> [DPass.PassDocumentType] -> m ()
validateRequiredDocuments mbProfilePicture mbPassPhotoMediaId person requiredDocs = do
  let mandatoryDocs = filter (/= DPass.ProfilePicture) requiredDocs
      missingDocs = filter (not . hasDocument mbProfilePicture mbPassPhotoMediaId person) mandatoryDocs
  unless (null missingDocs) $ do
    let missingDocNames = show missingDocs
    throwError $ InvalidRequest $ "Missing required documents: " <> missingDocNames

-- Check if person has a specific document
hasDocument :: Maybe Text -> Maybe (Id.Id DMF.MediaFile) -> DP.Person -> DPass.PassDocumentType -> Bool
hasDocument mbProfilePicture mbPassPhotoMediaId person docType = case docType of
  DPass.ProfilePicture -> isJust mbPassPhotoMediaId || isJust (mbProfilePicture <|> person.profilePicture)
  DPass.Aadhaar -> person.aadhaarVerified

-- Check if two date ranges overlap
hasDateOverlap :: (DT.Day, DT.Day) -> (DT.Day, DT.Day) -> Bool
hasDateOverlap (aStart, aEnd) (bStart, bEnd) = not (aEnd < bStart || bEnd < aStart)

-- Calculate end date for a pass based on start date and maxValidDays
calculatePassEndDate :: DT.Day -> Maybe Int -> DT.Day
calculatePassEndDate startDate mbMaxValidDays =
  case mbMaxValidDays of
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
       in DT.fromGregorian nextY nextM endDay
    Just days -> DT.addDays (fromIntegral days) startDate
    Nothing -> startDate

-- Construct message keys for Pass fields
mkPassMessageKey :: Id.Id DPass.Pass -> Text -> Text
mkPassMessageKey passId name = passId.getId <> "-" <> name

findFloorTier :: Int -> AKeyMap.KeyMap A.Value -> Maybe A.Value
findFloorTier targetStages tiersObj =
  let parsedPairs :: [(Int, A.Value)]
      parsedPairs =
        mapMaybe
          ( \(k, v) -> case readMaybe (T.unpack (AKey.toText k)) of
              Just n | n <= targetStages -> Just (n, v)
              _ -> Nothing
          )
          (AKeyMap.toList tiersObj)
      sortedDesc = EHS.sortBy (\(a, _) (b, _) -> compare b a) parsedPairs
   in case sortedDesc of
        ((_, v) : _) -> Just v
        [] -> AKeyMap.lookup (AKey.fromText "default") tiersObj

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
      passEnum = passType.passEnum,
      description = passType.description
    }

buildPassAPIEntity ::
  Maybe Lang.Language ->
  DP.Person ->
  DPass.Pass ->
  Environment.Flow PassAPI.PassAPIEntity
buildPassAPIEntity mbLanguage person pass = do
  passType <- B.runInReplica $ QPassType.findById pass.passTypeId >>= fromMaybeM (PassTypeNotFound pass.passTypeId.getId)

  mbPassDetails <-
    case passType.passEnum of
      Just DPassType.StudentPass -> B.runInReplica $ QPassDetails.findByPersonId person.id DPassType.StudentPass
      _ -> return Nothing

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
            "blocked" A..= (person.blocked :: Bool),
            "verificationStatus" A..= ((.verificationStatus) <$> mbPassDetails),
            "numberOfStages" A..= ((.numberOfStages) =<< mbPassDetails)
          ]

  -- Check purchase eligibility using JSON logic
  eligibility <- checkEligibility pass.purchaseEligibilityJsonLogic personData

  -- Get pass amount: use pricing tiers if verified organization holder, else default pass amount
  let mbTierAmount = do
        pd <- mbPassDetails
        guard eligibility
        stages <- pd.numberOfStages
        tiers <- pass.pricingTiers
        case tiers of
          A.Object obj -> do
            val <- findFloorTier stages obj
            case val of
              A.Number n -> Just . HighPrecMoney $ toRational n
              _ -> Nothing
          _ -> Nothing

  let passAmount = fromMaybe pass.amount mbTierAmount

  let language = fromMaybe Lang.ENGLISH mbLanguage
  let moid = person.merchantOperatingCityId
  nameTranslation <- QT.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid (mkPassMessageKey pass.id "name") language
  benefitTranslation <- QT.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid (mkPassMessageKey pass.id "benefitDescription") language
  descriptionTranslation <- QT.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid (mkPassMessageKey pass.id "description") language
  let name = maybe pass.name (Just . (.message)) nameTranslation
  let benefitDescription = maybe pass.benefitDescription (.message) benefitTranslation
  let description = maybe pass.description (Just . (.message)) descriptionTranslation

  offer <-
    withTryCatch "getMultimodalPassAvailablePasses:offerListCache" (SOffer.offerListCache person.merchantId person.id person.merchantOperatingCityId DOrder.FRFSPassPurchase (mkPrice (Just INR) pass.amount) (case pass.applicableVehicleServiceTiers of [] -> Nothing; tiers -> Just $ T.intercalate "-" $ EHS.sort $ map show tiers))
      >>= \case
        Left _ -> return Nothing
        Right offersResp -> SOffer.mkCumulativeOfferResp person.merchantOperatingCityId offersResp [] Nothing

  let originalAmount = case pass.benefit of
        Just DPass.FullSaving -> passAmount
        Just (DPass.FixedSaving value) -> passAmount + value
        Just (DPass.PercentageSaving percentage) -> passAmount / (1 - (percentage / 100))
        Nothing -> passAmount

  return $
    PassAPI.PassAPIEntity
      { id = pass.id,
        amount = passAmount,
        originalAmount,
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
        offer,
        autoApply = pass.autoApply,
        minFare = pass.minFare,
        maxFare = pass.maxFare,
        verificationStatus = (.verificationStatus) <$> mbPassDetails,
        formVerificationConfig = pass.formVerificationConfig,
        referenceNumber = (.referenceNumber) =<< mbPassDetails
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
  let originalAmount = case benefit of
        Just DPass.FullSaving -> purchasedPass.passAmount
        Just (DPass.FixedSaving value) -> purchasedPass.passAmount + value
        Just (DPass.PercentageSaving percentage) -> purchasedPass.passAmount / (1 - (percentage / 100))
        Nothing -> purchasedPass.passAmount
  return $
    PassAPI.PassAPIEntity
      { id = Id.cast purchasedPass.id,
        amount = originalAmount, -- Doing this so that UI it shows the original amount instead of the discounted amount.
        originalAmount = originalAmount,
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
        offer = Nothing,
        autoApply = False, -- Auto-apply not relevant for already purchased passes
        minFare = Nothing,
        maxFare = Nothing,
        verificationStatus = Nothing,
        formVerificationConfig = Nothing,
        referenceNumber = Nothing
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
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, IssueManagement.Storage.BeamFlow.BeamFlow m r) =>
  Maybe Lang.Language ->
  DP.Person ->
  Maybe Text ->
  DT.Day ->
  DPurchasedPass.PurchasedPass ->
  m PassAPI.PurchasedPassAPIEntity
buildPurchasedPassAPIEntity mbLanguage person mbDeviceId today purchasedPass = do
  let deviceMismatch = maybe False (\deviceId -> purchasedPass.deviceId /= deviceId && purchasedPass.deviceId /= defaultDashboardDeviceId) mbDeviceId -- Nothing only for dashboard
  passType <- B.runInReplica $ QPassType.findById purchasedPass.passTypeId >>= fromMaybeM (PassTypeNotFound purchasedPass.passTypeId.getId)
  passCategory <- B.runInReplica $ QPassCategory.findById passType.passCategoryId >>= fromMaybeM (PassCategoryNotFound passType.passCategoryId.getId)
  futureRenewals <- QPurchasedPassPayment.findAllByPurchasedPassIdAndStatusStartDateGreaterThan Nothing Nothing purchasedPass.id DPurchasedPass.PreBooked purchasedPass.endDate

  let tripsLeft = case purchasedPass.maxValidTrips of
        Just maxTrips -> Just $ max 0 (maxTrips - fromMaybe 0 purchasedPass.usedTripCount)
        Nothing -> Nothing

  passAPIEntity <- buildPassAPIEntityFromPurchasedPass mbLanguage person.id purchasedPass
  let passDetailsEntity =
        PassAPI.PassDetailsAPIEntity
          { category = buildPassCategoryAPIEntity passCategory,
            passType = buildPassTypeAPIEntity passType,
            passDetails = passAPIEntity
          }

  let daysToExpire = fromIntegral $ DT.diffDays purchasedPass.endDate today

  passes <- CQPass.findAllByPassTypeIdAndEnabled purchasedPass.passTypeId True
  let maxSwitchCount = case listToMaybe passes >>= (.passConfig) of
        Just pc -> pc.maxSwitchCount
        Nothing -> 1
  mbLastVerified <- QPassVerifyTransaction.findLastVerifiedVehicleNumberByPurchasePassId purchasedPass.id
  let lastVerifiedVehicleNumber = fmap fst mbLastVerified
  let isAutoVerified = (mbLastVerified >>= snd) == Just True
  return $
    PassAPI.PurchasedPassAPIEntity
      { id = purchasedPass.id,
        passNumber = let s = show purchasedPass.passNumber in T.pack $ replicate (8 - length s) '0' ++ s,
        passEntity = passDetailsEntity,
        tripsLeft = tripsLeft,
        lastVerifiedVehicleNumber,
        isAutoVerified,
        status = purchasedPass.status,
        startDate = purchasedPass.startDate,
        deviceMismatch,
        deviceSwitchAllowed = purchasedPass.deviceSwitchCount < maxSwitchCount,
        profilePicture = purchasedPass.profilePicture <|> person.profilePicture,
        passPhotoMediaId = purchasedPass.passPhotoMediaId,
        daysToExpire = daysToExpire,
        purchaseDate = DT.utctDay purchasedPass.createdAt,
        expiryDate = purchasedPass.endDate,
        isPreferredSourceAndDestinationSet = isJust purchasedPass.preferredDestination && isJust purchasedPass.preferredSource,
        futureRenewals = buildPurchasedPassPaymentAPIEntity <$> futureRenewals
      }

-- Webhook Handler for Pass Payment Status Updates
passOrderStatusHandler ::
  (HasFlowEnv m r '["smsCfg" ::: SmsConfig, "kafkaProducerTools" ::: KafkaProducerTools], MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Id.Id DOrder.PaymentOrder ->
  Id.Id DM.Merchant ->
  Payment.TransactionStatus ->
  m (DPayment.PaymentFulfillmentStatus, Maybe Text, Maybe Text)
passOrderStatusHandler paymentOrderId _merchantId status = do
  logInfo $ "Pass payment webhook handler called for paymentOrderId: " <> paymentOrderId.getId
  mbPurchasedPassPayment <- QPurchasedPassPayment.findOneByPaymentOrderId paymentOrderId
  mbPurchasedPass <- maybe (pure Nothing) (QPurchasedPass.findById . (.purchasedPassId)) mbPurchasedPassPayment
  mbRiderConfig <- maybe (pure Nothing) (\purchasedPass -> getConfig (RiderConfigDimensions {merchantOperatingCityId = purchasedPass.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId purchasedPass.merchantOperatingCityId))) mbPurchasedPass
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
  istTime <- getLocalCurrentTime timeDiffFromUtc
  let today = DT.utctDay istTime
  case (mbPurchasedPassPayment, mbPurchasedPass) of
    (Just purchasedPassPayment, Just purchasedPass) -> do
      let isDashboard = fromMaybe False purchasedPassPayment.isDashboard
      let mbPassStatus = convertPaymentStatusToPurchasedPassStatus (isJust purchasedPass.profilePicture || isJust purchasedPass.passPhotoMediaId) (purchasedPassPayment.startDate > DT.utctDay istTime) status
      let activeLikeStatuses = [DPurchasedPass.Active, DPurchasedPass.PreBooked, DPurchasedPass.PhotoPending]
      let refundStatuses = [DPurchasedPass.RefundPending, DPurchasedPass.RefundInitiated, DPurchasedPass.RefundFailed, DPurchasedPass.Refunded]
      mbDuplicateActivePayment <-
        case mbPassStatus of
          Just s
            | s `elem` activeLikeStatuses
                && purchasedPassPayment.status `notElem` (activeLikeStatuses ++ refundStatuses) -> do
              otherActivePayments <-
                QPurchasedPassPayment.findAllByPurchasedPassIdAndStatus
                  Nothing
                  Nothing
                  purchasedPass.id
                  activeLikeStatuses
                  purchasedPassPayment.startDate
              pure $
                listToMaybe $
                  filter
                    ( \p ->
                        p.id /= purchasedPassPayment.id
                          && hasDateOverlap (p.startDate, p.endDate) (purchasedPassPayment.startDate, purchasedPassPayment.endDate)
                    )
                    otherActivePayments
          _ -> pure Nothing
      case mbDuplicateActivePayment of
        Just existingActive -> do
          logInfo $
            "Duplicate overlapping active pass payment detected for paymentOrderId: " <> paymentOrderId.getId
              <> ", purchasedPassId: "
              <> purchasedPass.id.getId
              <> ", existing active paymentId: "
              <> existingActive.id.getId
              <> ". Marking this payment as RefundPending for auto-refund."
          return (DPayment.FulfillmentRefundPending, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
        Nothing -> do
          -- Don't re-activate this payment if it's already in a refund flow — a late CHARGED
          -- replay would otherwise flip Refunded/RefundPending/etc. back to Active, silently
          -- diverging from the refund outcome. Guard is on the payment row's own status only:
          -- a pass-level refund state can come from a different payment (e.g., prior refund of
          -- Payment A), which shouldn't block a legitimate retry via Payment B.
          let isPaymentInRefundFlow = purchasedPassPayment.status `elem` refundStatuses
          whenJust mbPassStatus $ \passStatus -> unless isPaymentInRefundFlow $ do
            when (purchasedPassPayment.status `notElem` activeLikeStatuses) $ do
              QPurchasedPassPayment.updateStatusByOrderId passStatus paymentOrderId
              when (passStatus `elem` activeLikeStatuses && isDashboard) $ do
                void $ withTryCatch "sendPassPurchasedSuccessMessage" $ sendPassPurchasedSuccessMessage purchasedPass.personId purchasedPass.merchantId purchasedPass.merchantOperatingCityId (fromMaybe "" purchasedPass.passName)
            when (purchasedPass.status `notElem` activeLikeStatuses) $ do
              QPurchasedPass.updatePurchaseData purchasedPass.id purchasedPassPayment.startDate purchasedPassPayment.endDate passStatus purchasedPassPayment.benefitDescription purchasedPassPayment.benefitType purchasedPassPayment.benefitValue purchasedPassPayment.amount
            when (passStatus `elem` activeLikeStatuses) $ do
              QPurchasedPass.updateProfilePictureById purchasedPassPayment.profilePicture purchasedPass.id
              -- Don't touch passPhotoMediaId here: the async upload writes it to the payment row,
              -- and the pass-list reconcile (updatePurchasedPass) is the single owner that projects
              -- it onto the pass at each transition — so the webhook never races the upload for it.
              when (passStatus == DPurchasedPass.Active && purchasedPassPayment.startDate <= today && purchasedPassPayment.endDate >= today) $
                QPurchasedPass.updatePurchaseData purchasedPass.id purchasedPassPayment.startDate purchasedPassPayment.endDate passStatus purchasedPassPayment.benefitDescription purchasedPassPayment.benefitType purchasedPassPayment.benefitValue purchasedPassPayment.amount
          case purchasedPassPayment.status of
            DPurchasedPass.RefundPending -> return (DPayment.FulfillmentRefundPending, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
            DPurchasedPass.RefundInitiated -> return (DPayment.FulfillmentRefundInitiated, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
            DPurchasedPass.RefundFailed -> return (DPayment.FulfillmentRefundFailed, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
            DPurchasedPass.Refunded -> return (DPayment.FulfillmentRefunded, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
            DPurchasedPass.Failed -> return (DPayment.FulfillmentFailed, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
            _ -> do
              case mbPassStatus of
                Just DPurchasedPass.Active -> return (DPayment.FulfillmentSucceeded, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
                Just DPurchasedPass.PreBooked -> return (DPayment.FulfillmentSucceeded, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
                Just DPurchasedPass.PhotoPending -> return (DPayment.FulfillmentSucceeded, Just purchasedPass.id.getId, Just purchasedPassPayment.id.getId)
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
    convertPaymentStatusToPurchasedPassStatus hasProfilePicture futureDatePass = \case
      Payment.CHARGED -> if hasProfilePicture then if futureDatePass then Just DPurchasedPass.PreBooked else Just DPurchasedPass.Active else Just DPurchasedPass.PhotoPending
      -- There can be a CHARGED transaction for Same Order even on Failure, so we should not mark the Pass as FAILED.
      -- Payment.AUTHENTICATION_FAILED -> Just DPurchasedPass.Failed
      -- Payment.AUTHORIZATION_FAILED -> Just DPurchasedPass.Failed
      -- Payment.JUSPAY_DECLINED -> Just DPurchasedPass.Failed
      Payment.CANCELLED -> Just DPurchasedPass.Failed
      Payment.AUTO_REFUNDED -> Just DPurchasedPass.Refunded
      _ -> Nothing

    sendPassPurchasedSuccessMessage :: (HasFlowEnv m r '["smsCfg" ::: SmsConfig, "kafkaProducerTools" ::: KafkaProducerTools], MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id.Id DP.Person -> Id.Id DM.Merchant -> Id.Id DMOC.MerchantOperatingCity -> Text -> m ()
    sendPassPurchasedSuccessMessage personId merchantId merchantOpCityId passName = do
      person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      mbPhoneNumber <- decrypt `mapM` person.mobileNumber
      whenJust mbPhoneNumber $ \phoneNumber -> do
        let countryCode = fromMaybe "+91" person.mobileCountryCode
            phoneNumberWithCountryCode = countryCode <> phoneNumber
        withLogTag ("sending Pass Purchased Success SMS" <> Id.getId person.id) $ do
          buildSmsReq <- MessageBuilder.buildPassSuccessMessage merchantOpCityId $ MessageBuilder.BuildPassSuccessMessage {passName}
          Sms.sendSMS merchantId merchantOpCityId (buildSmsReq phoneNumberWithCountryCode)
            >>= Sms.checkSmsResult

updatePurchasedPass ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DPurchasedPass.PurchasedPass ->
  DT.Day ->
  UTCTime ->
  m (DPurchasedPass.PurchasedPass, Maybe DPurchasedPassPayment.PurchasedPassPayment, Bool)
-- Reconcile the pass against its latest payment term, lazily activating a PhotoPending pass once
-- its photo is attached. uploadProfilePicture is attach-only and writes the media id onto the
-- payment row (never the pass row), so this read path owns both the PhotoPending -> Active/PreBooked
-- transition and the normal term-based reconcile. Date-expired PhotoPending passes fall through to
-- the expiry fallback below.
updatePurchasedPass purchasedPass _today now
  | purchasedPass.status /= DPurchasedPass.PhotoPending && diffUTCTime now purchasedPass.updatedAt <= 300 =
    return (purchasedPass, Nothing, False)
updatePurchasedPass purchasedPass today now = do
  latestPayments <-
    QPurchasedPassPayment.findAllByPurchasedPassIdAndStatus
      (Just 1)
      (Just 0)
      purchasedPass.id
      [DPurchasedPass.PreBooked, DPurchasedPass.Active, DPurchasedPass.PhotoPending]
      today

  case listToMaybe latestPayments of
    Just firstPayment
      | firstPayment.status == DPurchasedPass.PhotoPending,
        Nothing <- firstPayment.passPhotoMediaId ->
        return (purchasedPass, Nothing, False)
    Just firstPayment ->
      let newStatus
            | firstPayment.endDate < today = DPurchasedPass.Expired
            | firstPayment.startDate <= today = DPurchasedPass.Active
            | otherwise = DPurchasedPass.PreBooked

          newPass =
            purchasedPass
              { DPurchasedPass.startDate = firstPayment.startDate,
                DPurchasedPass.endDate = firstPayment.endDate,
                DPurchasedPass.status = newStatus,
                DPurchasedPass.passPhotoMediaId = firstPayment.passPhotoMediaId <|> purchasedPass.passPhotoMediaId,
                DPurchasedPass.usedTripCount = Just 0,
                DPurchasedPass.deviceSwitchCount = 0,
                DPurchasedPass.updatedAt = now
              }

          newPassPayment =
            firstPayment
              { DPurchasedPassPayment.status = newStatus,
                DPurchasedPassPayment.updatedAt = now
              }

          hasChanged =
            purchasedPass.status /= newStatus
              || firstPayment.status /= newStatus
       in return (newPass, Just newPassPayment, hasChanged)
    Nothing ->
      let newPass =
            purchasedPass
              { DPurchasedPass.status = DPurchasedPass.Expired,
                DPurchasedPass.updatedAt = now
              }
       in return (newPass, Nothing, True)

getMultimodalPassList ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Maybe Text ->
    Maybe Text ->
    Kernel.Prelude.Maybe Lang.Language ->
    Maybe Int ->
    Maybe Int ->
    Maybe DPurchasedPass.StatusType ->
    Environment.Flow [PassAPI.PurchasedPassAPIEntity]
  )
getMultimodalPassList = getMultimodalPassListUtil False

getMultimodalPassListUtil ::
  Bool ->
  ( Kernel.Prelude.Maybe (Id.Id DP.Person),
    Id.Id DM.Merchant
  ) ->
  Maybe Text ->
  Maybe Text ->
  Kernel.Prelude.Maybe Lang.Language ->
  Maybe Int ->
  Maybe Int ->
  Maybe DPurchasedPass.StatusType ->
  Environment.Flow [PassAPI.PurchasedPassAPIEntity]
getMultimodalPassListUtil isDashboard (mbCallerPersonId, merchantId) mbDeviceIdParam mbImeiParam mbLanguage mbLimitParam mbOffsetParam mbStatusParam = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbDeviceId <- if isDashboard then return Nothing else Just <$> getDeviceId person mbDeviceIdParam mbImeiParam

  let mbStatus = case mbStatusParam of
        Just DPurchasedPass.Active -> Just [DPurchasedPass.Active, DPurchasedPass.PreBooked, DPurchasedPass.Expired, DPurchasedPass.PhotoPending]
        Just s -> Just [s]
        Nothing -> Nothing

  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId person.merchantOperatingCityId))
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
  istTime <- getLocalCurrentTime timeDiffFromUtc
  let today = DT.utctDay istTime
  now <- getCurrentTime

  passEntities <- QPurchasedPass.findAllByPersonIdWithFilters personId merchantId mbStatus mbLimitParam mbOffsetParam

  -- Process each pass and apply updates in-memory to ensure we return the latest state
  -- without relying on read replica synchronization
  updatedPassEntities <- forM passEntities $ \purchasedPass -> do
    (updatedPass, mbPayment, shouldUpdateDB) <- updatePurchasedPass purchasedPass today now

    when shouldUpdateDB $ do
      QPurchasedPassPayment.expireOlderPaymentsByPurchasedPassId purchasedPass.id today
      case (updatedPass.status, mbPayment) of
        (DPurchasedPass.Expired, _) ->
          QPurchasedPass.updateStatusById DPurchasedPass.Expired purchasedPass.id
        (_, Just firstPreBookedPayment) -> do
          QPurchasedPassPayment.updateStatusByOrderId updatedPass.status firstPreBookedPayment.orderId
          QPurchasedPass.updatePurchaseData purchasedPass.id updatedPass.startDate updatedPass.endDate updatedPass.status updatedPass.benefitDescription updatedPass.benefitType updatedPass.benefitValue updatedPass.passAmount
          -- Project the activating term's photo onto the pass. whenJust-guarded so a photo-less
          -- renewal payment can't null out an existing pass photo.
          whenJust firstPreBookedPayment.passPhotoMediaId $ \paymentPhotoMediaId ->
            QPurchasedPass.updatePassPhotoMediaIdById (Just paymentPhotoMediaId) purchasedPass.id
        _ -> return ()

    return updatedPass

  let allActivePurchasedPasses =
        HM.elems $
          foldr
            ( \el acc ->
                case HM.lookup el.passTypeId acc of
                  Just _ ->
                    case mbDeviceId of
                      Just deviceId | el.deviceId == deviceId -> HM.insert el.passTypeId el acc
                      _ -> acc
                  Nothing -> HM.insert el.passTypeId el acc
            )
            HM.empty
            updatedPassEntities

  -- Always show all passes regardless of device. The deviceMismatch flag in
  -- PurchasedPassAPIEntity will inform the UI which passes need device switching.
  -- Previously, passes from other devices were hidden if any pass (even Pending)
  -- existed for the current device, preventing users from switching older active passes.
  purchasedPassAPIEntities <- mapM (buildPurchasedPassAPIEntity mbLanguage person mbDeviceId today) allActivePurchasedPasses
  let hasActivePass = any (\p -> p.status == DPurchasedPass.Active) purchasedPassAPIEntities
      isInactiveTouristPass p = p.passEntity.passType.passEnum == Just DPassType.TouristPass && p.status /= DPurchasedPass.Active
      isExpiredPass p = p.status == DPurchasedPass.Expired
      shouldFilter p = isInactiveTouristPass p || (hasActivePass && isExpiredPass p)
  pure $ filter (not . shouldFilter) purchasedPassAPIEntities

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
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId person.merchantOperatingCityId))
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
  istTime <- getLocalCurrentTime timeDiffFromUtc
  unless (purchasedPass.startDate <= DT.utctDay istTime) $ throwError (PassActivationNotReady purchasedPassId.getId $ "Pass will be active from " <> show purchasedPass.startDate)

  mbPassType <- CQPassType.findById purchasedPass.passTypeId
  let mbPassEnum = mbPassType >>= (.passEnum)
  mbStudentPassDetails <- case mbPassEnum of
    Just DPassType.StudentPass -> QPassDetails.findByPersonId purchasedPass.personId DPassType.StudentPass
    _ -> pure Nothing
  let isStudentPass = mbPassEnum == Just DPassType.StudentPass
  when isStudentPass $
    reserveStudentPassActivationSlot purchasedPass mbStudentPassDetails istTime
  let throwAndReleaseSlot :: PassError -> Environment.Flow a
      throwAndReleaseSlot err = do
        when isStudentPass $ rollbackStudentPassActivation purchasedPass
        throwError err

  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig person.merchantOperatingCityId Enums.BUS DIBC.MULTIMODAL

  -- If autoActivated is requested, find the nearest fleet (vehicle number) from user location
  vehicleNumberToUse <-
    if fromMaybe False passVerifyReq.autoActivated
      then do
        case (passVerifyReq.currentLat, passVerifyReq.currentLon) of
          (Just lat, Just lon) -> do
            riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId person.merchantOperatingCityId)) >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
            case integratedBPPConfigs of
              [] -> throwAndReleaseSlot (PassVerificationFailed purchasedPassId.getId "No integrated BPP config available for auto activation")
              (nearbyConfig : _) -> do
                buses <- FRFSJourneyUtils.getNearbyBusesFRFS (LatLong lat lon) riderConfig nearbyConfig
                let busesWithVehicle = filter (isJust . (.vehicle_number)) buses
                when (null busesWithVehicle) $ throwAndReleaseSlot (PassNoBusesNearby purchasedPassId.getId)

                -- Filter by purchased pass applicable service tiers
                let applicableTiers = purchasedPass.applicableVehicleServiceTiers
                busesFilteredByTier <-
                  if null applicableTiers
                    then return busesWithVehicle
                    else
                      filterM
                        ( \b ->
                            case b.vehicle_number of
                              Just v -> do
                                mbVehicleMetadata <- OTPRest.getVehicleMetadata nearbyConfig v (Just True)
                                return $ case mbVehicleMetadata of
                                  Nothing -> False
                                  Just metadata ->
                                    fromMaybe True metadata.isActuallyValid
                                      && metadata.serviceType `elem` applicableTiers
                              Nothing -> return False
                        )
                        busesWithVehicle

                when (null busesFilteredByTier) $ throwAndReleaseSlot (PassNoBusesNearby purchasedPassId.getId)

                let nearest = minimumBy (EHS.comparing (\b -> distanceBetweenInMeters (LatLong lat lon) (LatLong b.latitude b.longitude))) busesFilteredByTier
                case nearest.vehicle_number of
                  Just v -> return v
                  Nothing -> throwAndReleaseSlot (PassNoBusesNearby purchasedPassId.getId)
          _ -> throwAndReleaseSlot (PassVerificationFailed purchasedPassId.getId "Location is required for auto activation")
      else return passVerifyReq.vehicleNumber

  (integratedBPPConfig, vehicleInfo) <-
    JLU.getVehicleLiveRouteInfo integratedBPPConfigs vehicleNumberToUse (Just True)
      >>= maybe (throwAndReleaseSlot (PassInvalidVehicle purchasedPassId.getId vehicleNumberToUse)) pure
  when (fromMaybe True vehicleInfo.isActuallyValid) $ do
    unless (vehicleInfo.serviceType `elem` purchasedPass.applicableVehicleServiceTiers) $
      throwAndReleaseSlot $ PassVerificationFailed purchasedPassId.getId ("This pass is only valid for " <> purchasedPass.benefitDescription)
  routeStopMapping <-
    case vehicleInfo.routeCode of
      Just routeCode ->
        withTryCatch
          "passVerify:getRouteStopMappingByRouteCodeInMem"
          (OTPRest.getRouteStopMappingByRouteCodeInMem routeCode integratedBPPConfig)
          >>= \case
            Left _ -> return []
            Right rsm -> return rsm
      Nothing -> return []

  mbVerifyStatus <- case mbPassEnum of
    Just DPassType.StudentPass ->
      case mbStudentPassDetails of
        Nothing -> pure $ Just DPassVerifyTransaction.NOT_VERIFIED
        Just passDetails -> do
          riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId person.merchantOperatingCityId)) >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
          pure $ Just $ verifyStudentPass passDetails vehicleInfo.routeCode routeStopMapping riderConfig
    _ -> pure Nothing

  let sourceStop =
        getNearestStop routeStopMapping passVerifyReq.currentLat passVerifyReq.currentLon
          <|> (listToMaybe routeStopMapping <&> (.stopCode))
      destinationStop = safeTail routeStopMapping <&> (.stopCode)
  id <- generateGUID
  now <- getCurrentTime
  let passVerifyTransaction =
        DPassVerifyTransaction.PassVerifyTransaction
          { id = id,
            purchasePassId = purchasedPassId,
            autoActivated = Just (fromMaybe False passVerifyReq.autoActivated),
            validTill = addUTCTime (intToNominalDiffTime (fromIntegral purchasedPass.verificationValidity)) now,
            verifiedAt = now,
            fleetId = vehicleNumberToUse,
            sourceStopCode = sourceStop,
            destinationStopCode = destinationStop,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just person.merchantOperatingCityId,
            isActuallyValid = Just $ fromMaybe True vehicleInfo.isActuallyValid,
            verificationStatus = mbVerifyStatus,
            passEnum = mbPassEnum
          }
  QPassVerifyTransaction.create passVerifyTransaction

  case mbVerifyStatus of
    Just DPassVerifyTransaction.NOT_VERIFIED ->
      throwAndReleaseSlot $ PassVerificationFailed purchasedPassId.getId "This pass is not valid for this bus route"
    Just _ -> pure ()
    Nothing -> pure ()

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

-- | Redis key for the per-person per-passType daily activation counter.
mkPassRouteActivationCountKey :: Id.Id DP.Person -> Id.Id DPassType.PassType -> Text
mkPassRouteActivationCountKey personId passTypeId =
  "PassRouteActivation:PersonId:" <> personId.getId <> ":PassTypeId:" <> passTypeId.getId

verifyStudentPass ::
  DPassDetails.PassDetails ->
  Maybe Text ->
  [SharedLogic.External.Nandi.Types.RouteStopMappingInMemoryServer] ->
  Domain.Types.RiderConfig.RiderConfig ->
  DPassVerifyTransaction.PassVerifiedStatus
verifyStudentPass passDetails mbBusRouteCode routeStopMapping riderConfig =
  let cfg = riderConfig.studentPassVerifyConfig
      thresholdMeters = maybe 2000 ((.distanceThresholdMeters) >>> getMeters) cfg
      minMatchingStops = maybe 0 (.minMatchingStops) cfg
      applicableRouteIds = filter (not . T.null) (map T.strip (fromMaybe [] passDetails.applicableRouteIds))
      primaryMatch = case T.strip <$> mbBusRouteCode of
        Just rc | not (T.null rc) -> rc `elem` applicableRouteIds
        _ -> False
   in if primaryMatch
        then DPassVerifyTransaction.FULLY_VERIFIED
        else
          let userStops = concatMap (\rp -> [rp.srcLatLong, rp.destLatLong]) passDetails.routePairs
              busStops = map (.stopPoint) routeStopMapping
              isWithinThreshold bus user =
                getMeters (highPrecMetersToMeters (distanceBetweenInMeters bus user)) <= thresholdMeters
              matchingStopCount = length [() | bus <- busStops, user <- userStops, isWithinThreshold bus user]
           in if matchingStopCount >= minMatchingStops
                then DPassVerifyTransaction.PARTIALLY_VERIFIED
                else DPassVerifyTransaction.NOT_VERIFIED

studentPassActivationLimit :: Maybe DPassDetails.PassDetails -> Int
studentPassActivationLimit mbPassDetails =
  let routePairCount = maybe 0 (length . (.routePairs)) mbPassDetails
   in if routePairCount > 0 then routePairCount else 6

reserveStudentPassActivationSlot ::
  DPurchasedPass.PurchasedPass ->
  Maybe DPassDetails.PassDetails ->
  DT.UTCTime ->
  Environment.Flow ()
reserveStudentPassActivationSlot purchasedPass mbPassDetails istTime = do
  let countKey = mkPassRouteActivationCountKey purchasedPass.personId purchasedPass.passTypeId
      activationLimit = studentPassActivationLimit mbPassDetails
  newCount <- Hedis.incr countKey
  let istOffset = 19800 :: NominalDiffTime
      tomorrowMidnightIST = DT.UTCTime (DT.addDays 1 (DT.utctDay istTime)) 0
      tomorrowMidnightUTC = DT.addUTCTime (negate istOffset) tomorrowMidnightIST
  now <- getCurrentTime
  let secsUntilMidnight = max 1 (round (DT.diffUTCTime tomorrowMidnightUTC now) :: Int)
  void $ Hedis.expire countKey secsUntilMidnight
  when (newCount > fromIntegral activationLimit) $ do
    void $ Hedis.decr countKey
    throwError (InvalidRequest "Pass activation limit reached for this pass type")

rollbackStudentPassActivation ::
  DPurchasedPass.PurchasedPass ->
  Environment.Flow ()
rollbackStudentPassActivation purchasedPass = do
  let countKey = mkPassRouteActivationCountKey purchasedPass.personId purchasedPass.passTypeId
  void $ Hedis.decr countKey

postMultimodalPassSwitchDeviceId ::
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    PassAPI.PassSwitchDeviceIdReq ->
    Environment.Flow APISuccess.APISuccess
  )
postMultimodalPassSwitchDeviceId (mbCallerPersonId, merchantId) req = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  deviceId <- getDeviceId person req.deviceId req.imeiNumber
  allActivePurchasedPasses <- QPurchasedPass.findAllByPersonIdWithFilters personId merchantId (Just [DPurchasedPass.Active, DPurchasedPass.PreBooked]) Nothing Nothing
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId person.merchantOperatingCityId))
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
  istTime <- getLocalCurrentTime timeDiffFromUtc
  let today = DT.utctDay istTime

  forM_ allActivePurchasedPasses $ \purchasedPass -> do
    when (purchasedPass.deviceId /= deviceId) $ do
      -- Check if there are other passes with the same passTypeId that already have this deviceId.
      duplicatePasses <- QPurchasedPass.findAllByPersonIdAndPassTypeIdAndStatus personId merchantId purchasedPass.passTypeId [DPurchasedPass.Active, DPurchasedPass.PreBooked, DPurchasedPass.Pending, DPurchasedPass.PhotoPending]
      let otherDevicePasses = filter (\p -> p.id /= purchasedPass.id && p.deviceId == deviceId) duplicatePasses

      case otherDevicePasses of
        [] -> do
          -- No conflict, simply update the device ID
          QPurchasedPass.updateDeviceIdById deviceId (purchasedPass.deviceSwitchCount + 1) purchasedPass.id
        conflictingPasses -> do
          -- There are conflicting passes with the same device ID and passTypeId
          -- Need to determine which pass to retain
          let allPassesToConsider = purchasedPass : conflictingPasses
              -- Determine which pass to retain based on priority:
              -- 1. Currently active pass (startDate <= today && endDate >= today)
              -- 2. Future start date pass (startDate > today)
              -- 3. If both are active, keep the one with more remaining days
              passToRetain = selectPassToRetain today allPassesToConsider
              passesToDelete = filter (\p -> p.id /= passToRetain.id) allPassesToConsider

          -- Update the device ID on the retained pass
          QPurchasedPass.updateDeviceIdById deviceId (passToRetain.deviceSwitchCount + 1) passToRetain.id

          -- Migrate all PurchasedPassPayment records from deleted passes to the retained pass
          forM_ passesToDelete $ \passToDelete -> do
            QPurchasedPassPayment.updatePurchasedPassIdByOldPurchasedPassId passToRetain.id passToDelete.id
            -- Delete the non-retained pass
            QPurchasedPass.deleteById passToDelete.id

          logInfo $ "Merged duplicate passes for personId: " <> personId.getId <> ", retained pass: " <> passToRetain.id.getId <> ", deleted passes: " <> show (map (.id.getId) passesToDelete)

  return APISuccess.Success
  where
    -- Select the pass to retain based on priority rules
    selectPassToRetain :: DT.Day -> [DPurchasedPass.PurchasedPass] -> DPurchasedPass.PurchasedPass
    selectPassToRetain today passes =
      let -- Priority 1: Currently active passes (startDate <= today && endDate >= today)
          activePasses = filter (\p -> p.startDate <= today && p.endDate >= today) passes
          -- Priority 2: Future start date passes (startDate > today)
          futurePasses = filter (\p -> p.startDate > today) passes
       in case activePasses of
            [single] -> single
            (_ : _) ->
              -- Multiple active passes, keep the one with more remaining days (later end date)
              maximumBy (EHS.comparing (.endDate)) activePasses
            [] ->
              case futurePasses of
                (_firstFuture : _) ->
                  -- Pick the future pass with the earliest start date but latest end date
                  maximumBy (EHS.comparing (.endDate)) futurePasses
                [] ->
                  -- Fallback: pick the most recently created pass
                  maximumBy (EHS.comparing (.createdAt)) passes

postMultimodalPassResetDeviceSwitchCount ::
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DPurchasedPass.PurchasedPass ->
    Environment.Flow APISuccess.APISuccess
  )
postMultimodalPassResetDeviceSwitchCount (mbCallerPersonId, merchantId) purchasedPassId = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
  purchasedPass <- QPurchasedPass.findById purchasedPassId >>= fromMaybeM (PurchasedPassNotFound purchasedPassId.getId)
  unless (purchasedPass.personId == personId) $ throwError AccessDenied
  unless (purchasedPass.merchantId == merchantId) $ throwError AccessDenied
  purchasedPasses <- QPurchasedPass.findAllByPersonIdWithFilters personId merchantId (Just [DPurchasedPass.Active]) Nothing Nothing
  case purchasedPasses of
    [_] -> return ()
    _ -> throwError $ InvalidRequest "Only one active pass is allowed"
  QPurchasedPass.updateDeviceIdById purchasedPass.deviceId 0 purchasedPass.id
  return APISuccess.Success

getMultimodalPassTransactions ::
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Environment.Flow [PassAPI.PurchasedPassTransactionAPIEntity]
  )
getMultimodalPassTransactions (mbCallerPersonId, _) mbLimitParam mbOffsetParam mbStatusParam = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
  let limit = fromMaybe 10 mbLimitParam
  let offset = fromMaybe 0 mbOffsetParam
  -- status is a comma-separated list (e.g. "Active,PreBooked"); unparseable tokens are dropped.
  let statuses = maybe [] (mapMaybe (readMaybe . T.unpack . T.strip) . T.splitOn ",") mbStatusParam
  allPurchasedPassTransactions <- case statuses of
    [] -> QPurchasedPassPayment.findAllWithPersonId (Just limit) (Just offset) personId
    _ -> QPurchasedPassPayment.findAllByPersonIdAndStatuses (Just limit) (Just offset) personId statuses
  return $ map buildPurchasedPassPaymentAPIEntity allPurchasedPassTransactions

buildPurchasedPassPaymentAPIEntity :: DPurchasedPassPayment.PurchasedPassPayment -> PassAPI.PurchasedPassTransactionAPIEntity
buildPurchasedPassPaymentAPIEntity purchasedPassPayment =
  PassAPI.PurchasedPassTransactionAPIEntity
    { id = purchasedPassPayment.id,
      startDate = purchasedPassPayment.startDate,
      endDate = purchasedPassPayment.endDate,
      status = purchasedPassPayment.status,
      amount = purchasedPassPayment.amount,
      passName = purchasedPassPayment.passName,
      passCode = purchasedPassPayment.passCode,
      passType = purchasedPassPayment.passEnum,
      createdAt = purchasedPassPayment.createdAt
    }

getDeviceId :: DP.Person -> Maybe Text -> Maybe Text -> Environment.Flow Text
getDeviceId person mbDeviceId mbImei = do
  case mbImei of
    Just imei -> pure imei
    Nothing -> case mbDeviceId of
      Just devId -> do
        fallbackImeiNumber <- decrypt `mapM` person.imeiNumber
        return $ fromMaybe devId fallbackImeiNumber
      Nothing -> throwError (InvalidRequest "Device ID or IMEI is required")

postMultimodalPassActivateToday ::
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Int ->
    Maybe DT.Day ->
    Environment.Flow APISuccess.APISuccess
  )
postMultimodalPassActivateToday args passNumber mbStartDate = postMultimodalPassActivateTodayUtil False args passNumber mbStartDate Nothing

postMultimodalPassActivateTodayUtil ::
  Bool ->
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Int ->
    Maybe DT.Day ->
    Maybe (Id.Id DPurchasedPassPayment.PurchasedPassPayment) ->
    Environment.Flow APISuccess.APISuccess
  )
postMultimodalPassActivateTodayUtil isDashboard (mbCallerPersonId, _merchantId) passNumber mbStartDate mbPurchasedPassPaymentId = do
  purchasedPass <- QPurchasedPass.findByPassNumber passNumber >>= fromMaybeM (PurchasedPassNotFound (show passNumber))
  unless isDashboard $ do
    personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
    unless (purchasedPass.personId == personId) $ throwError AccessDenied
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = purchasedPass.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId purchasedPass.merchantOperatingCityId))
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
  istTime <- getLocalCurrentTime timeDiffFromUtc
  let today = DT.utctDay istTime
  normalizedMbStartDate <- case mbStartDate of
    Just d | d == today -> return Nothing
    Just d | d < today && not isDashboard -> throwError (PassActivationNotReady purchasedPass.id.getId "Cannot schedule pass for a past date")
    _ -> return mbStartDate

  case normalizedMbStartDate of
    Nothing ->
      when ((not isDashboard && purchasedPass.status /= DPurchasedPass.PreBooked) || (isDashboard && purchasedPass.status /= DPurchasedPass.Active && purchasedPass.status /= DPurchasedPass.PreBooked)) $
        throwError (PassActivationNotReady purchasedPass.id.getId "Only pre-booked passes can be activated for today")
    Just _ ->
      unless (purchasedPass.status `elem` [DPurchasedPass.PreBooked, DPurchasedPass.Active]) $
        throwError (PassActivationNotReady purchasedPass.id.getId "Only active or pre-booked passes can be rescheduled")
  _ <- purchasedPass.maxValidDays & fromMaybeM (PassActivationNotReady purchasedPass.id.getId "Pass does not have a valid duration")
  let newStartDate = case normalizedMbStartDate of
        Nothing -> today
        Just date -> date
      newEndDate = calculatePassEndDate newStartDate purchasedPass.maxValidDays
      newStatus
        | newEndDate < today = DPurchasedPass.Expired
        | newStartDate <= today = DPurchasedPass.Active
        | otherwise = DPurchasedPass.PreBooked
  mbTargetPayment <- case mbPurchasedPassPaymentId of
    Just paymentId -> do
      payment <- QPurchasedPassPayment.findByPrimaryKey paymentId >>= fromMaybeM (InvalidRequest $ "Payment not found: " <> paymentId.getId)
      unless (payment.purchasedPassId == purchasedPass.id) $ throwError (InvalidRequest $ "Payment " <> paymentId.getId <> " does not belong to pass " <> purchasedPass.id.getId)
      unless (payment.status `elem` [DPurchasedPass.PreBooked, DPurchasedPass.Active]) $
        throwError (PassActivationNotReady purchasedPass.id.getId $ "Payment " <> paymentId.getId <> " is not in PreBooked or Active status")
      pure $ Just payment
    Nothing -> pure Nothing

  allPaymentsForPassCode <- QPurchasedPassPayment.findAllByPersonIdWithFilters Nothing Nothing purchasedPass.personId purchasedPass.merchantId [DPurchasedPass.Active, DPurchasedPass.PreBooked] purchasedPass.passCode
  let otherPassPayments = case mbTargetPayment of
        Just target -> filter (\p -> p.id /= target.id) allPaymentsForPassCode
        Nothing -> filter (\p -> p.purchasedPassId /= purchasedPass.id) allPaymentsForPassCode
      overlappingPasses = filter (\p -> hasDateOverlap (newStartDate, newEndDate) (p.startDate, p.endDate)) otherPassPayments

  unless (null overlappingPasses) $
    throwError (PassActivationOverlap purchasedPass.id.getId)

  -- Pick the payment to flip: the explicit target, or the parent's primary prebooked/active payment.
  paymentToUpdate <- case mbTargetPayment of
    Just p -> pure (Just p)
    Nothing -> listToMaybe <$> QPurchasedPassPayment.findAllByPurchasedPassIdAndStatusAndStartDate (Just 1) Nothing purchasedPass.id [DPurchasedPass.Active, DPurchasedPass.PreBooked] purchasedPass.startDate

  whenJust paymentToUpdate $ \payment ->
    QPurchasedPassPayment.updateStatusAndDatesById newStartDate newEndDate newStatus payment.id

  -- Keep the parent PurchasedPass in sync only when the target payment represents it
  -- (same startDate). Secondary renewal payments must not overwrite the parent row.
  let targetIsPrimary = maybe True (\p -> p.startDate == purchasedPass.startDate) mbTargetPayment
  when targetIsPrimary $
    QPurchasedPass.updatePurchaseData purchasedPass.id newStartDate newEndDate newStatus purchasedPass.benefitDescription purchasedPass.benefitType purchasedPass.benefitValue purchasedPass.passAmount
  return APISuccess.Success

-- | Upload a pass photo to S3 and attach the media id to the pass. Attach-only: it can run
-- concurrently with the payment webhook, so status transitions are owned elsewhere (payment
-- handler for Pending, pass-list reconcile for PhotoPending).
postMultimodalPassUploadProfilePicture ::
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DPurchasedPass.PurchasedPass ->
    Maybe Text ->
    IssueCommon.IssueMediaUploadReq ->
    Environment.Flow IssueCommon.IssueMediaUploadRes
  )
-- imeiNumber is accepted but unused: device binding is owned by select/switchDeviceId.
postMultimodalPassUploadProfilePicture (mbCallerPersonId, merchantId) purchasedPassId _mbImeiNumber req = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
  purchasedPass <- QPurchasedPass.findById purchasedPassId >>= fromMaybeM (PurchasedPassNotFound purchasedPassId.getId)

  unless (purchasedPass.personId == personId) $ throwError AccessDenied

  -- Allowed while a purchase is in flight (fresh buy, renewal on an Active pass, renewal of an
  -- Expired pass), a photo is owed, or a paid future term exists (PreBooked — lets the user change
  -- the photo for a renewed term) — not as an anytime photo swap on a live pass.
  let photoAttachable s = s `elem` [DPurchasedPass.Pending, DPurchasedPass.PhotoPending, DPurchasedPass.PreBooked]
  paymentRows <- QPurchasedPassPayment.findAllByPurchasedPassId purchasedPass.id
  unless (photoAttachable purchasedPass.status || any (photoAttachable . (.status)) paymentRows) $
    throwError (InvalidRequest "Pass has no purchase in progress and is not awaiting a photo")

  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  uploadRes <- IssueAction.mediaUploadToS3 merchant.mediaFileSizeUpperLimit merchant.mediaFileUrlPattern req "pass-photo" personId.getId
  let mediaId = uploadRes.fileId

  -- Attach-only: write the media id to the attachable payment row(s), never to the pass row. The
  -- pass-list reconcile (updatePurchasedPass) is the single owner that projects the payment row's
  -- photo onto the pass when its term activates — so an unpaid renewal or a stale Pending row
  -- can't overwrite the photo on a currently-live pass, and a photo changed on a paid future
  -- (PreBooked) term takes effect only when that term starts.
  QPurchasedPassPayment.updatePassPhotoMediaIdByPurchasedPassIdAndStatus (Just mediaId) purchasedPass.id [DPurchasedPass.Pending, DPurchasedPass.PhotoPending, DPurchasedPass.PreBooked]

  return uploadRes

-- | Fetch a pass photo's raw content from S3 by its media id, for rendering.
-- Only the owner (whose id is embedded in the S3 path) may fetch it.
getMultimodalPassPhoto ::
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DMF.MediaFile ->
    Environment.Flow Text
  )
getMultimodalPassPhoto (mbCallerPersonId, _merchantId) mediaId = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
  fetchPassPhotoFromS3 personId mediaId

-- | Attach an already-uploaded pass-photo media id to a pass (used by the
-- dashboard after it uploads the image to S3). Stores the media id on the pass
-- and its active/pending payment rows.
postMultimodalPassUpdateProfilePictureUtil ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id.Id DP.Person ->
  Id.Id DM.Merchant ->
  Id.Id DPurchasedPass.PurchasedPass ->
  Id.Id DMF.MediaFile ->
  m APISuccess.APISuccess
postMultimodalPassUpdateProfilePictureUtil personId merchantId purchasedPassId mediaId = do
  purchasedPass <- QPurchasedPass.findById purchasedPassId >>= fromMaybeM (PurchasedPassNotFound purchasedPassId.getId)
  unless (purchasedPass.personId == personId) $ throwError AccessDenied
  unless (purchasedPass.merchantId == merchantId) $ throwError AccessDenied
  QPurchasedPass.updatePassPhotoMediaIdById (Just mediaId) purchasedPass.id

  let validStatuses = [DPurchasedPass.Active, DPurchasedPass.PreBooked, DPurchasedPass.PhotoPending]
  QPurchasedPassPayment.updatePassPhotoMediaIdByPurchasedPassIdAndStatus (Just mediaId) purchasedPass.id validStatuses

  return APISuccess.Success

-- | Fetch a pass photo's raw content from S3 by media id, verifying the file
-- belongs to the given person (their id is embedded in the S3 path). Shared by
-- the customer GET endpoint and the dashboard render flow.
fetchPassPhotoFromS3 :: Id.Id DP.Person -> Id.Id DMF.MediaFile -> Environment.Flow Text
fetchPassPhotoFromS3 personId mediaId = do
  mediaFile <- QMediaFile.findById mediaId >>= fromMaybeM (InvalidRequest "Pass photo not found")
  s3FilePath <- mediaFile.s3FilePath & fromMaybeM (InvalidRequest "Pass photo has no associated S3 path")
  when (T.isInfixOf ".." s3FilePath) $ throwError (InvalidRequest "filePath must not contain path-traversal sequences")
  unless (T.isInfixOf ("pass-photo/" <> personId.getId <> "/") s3FilePath) $ throwError AccessDenied
  S3.get (T.unpack s3FilePath)

postMultimodalPassSetPrefSrcAndDest ::
  ( ( Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DPurchasedPass.PurchasedPass ->
    PassAPI.SetPassPrefSrcAndDestReq ->
    Environment.Flow APISuccess.APISuccess
  )
postMultimodalPassSetPrefSrcAndDest (mbCallerPersonId, merchantId) purchasedPassId req = do
  personId <- mbCallerPersonId & fromMaybeM (PersonNotFound "personId")
  purchasedPass <- QPurchasedPass.findById purchasedPassId >>= fromMaybeM (PurchasedPassNotFound purchasedPassId.getId)
  unless (purchasedPass.personId == personId) $ throwError AccessDenied
  unless (purchasedPass.merchantId == merchantId) $ throwError AccessDenied

  QPurchasedPass.updatePrefSrcAndDestById (Just req.prefSrc) (Just req.prefDest) purchasedPass.id

  return APISuccess.Success
