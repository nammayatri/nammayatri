module DBSync.Create where

import Config.Env
import Data.Maybe
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.Language as EL
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types as ET
import qualified Kernel.Beam.Types as KBT
import Types.DBSync
import Types.Event as Event
import Utils.Utils

runCreateCommands :: Show b => [(CreateDBCommand, b)] -> ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateCommands cmds = do
  dbConf <- fromJust <$> L.getOption KBT.PsqlDbCfg
  runCreate dbConf ("RegistrationToken" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RegistrationTokenObject obj), val) <- cmds]
    |::| runCreate dbConf ("BapMetadata" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BapMetadataObject obj), val) <- cmds]
    |::| runCreate dbConf ("Booking" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BookingObject obj), val) <- cmds]
    |::| runCreate dbConf ("BookingLocation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BookingLocationObject obj), val) <- cmds]
    |::| runCreate dbConf ("BookingCancellationReason" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BookingCancellationReasonObject obj), val) <- cmds]
    |::| runCreate dbConf ("BusinessEvent" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BusinessEventObject obj), val) <- cmds]
    |::| runCreate dbConf ("CallStatus" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (CallStatusObject obj), val) <- cmds]
    |::| runCreate dbConf ("CancellationReason" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (CancellationReasonObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverFlowStatus" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverFlowStatusObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverBlockReason" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverBlockReasonObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverFee" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverFeeObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverInformation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverInformationObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverLocation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverLocationObject obj), val) <- cmds]
    |::| runCreate dbConf ("AadhaarOtpReq" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (AadhaarOtpReqObject obj), val) <- cmds]
    |::| runCreate dbConf ("AadhaarOtpVerify" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (AadhaarOtpVerifyObject obj), val) <- cmds]
    |::| runCreate dbConf ("AadhaarVerification" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (AadhaarVerificationObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverLicense" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverLicenseObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverRCAssociation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverRCAssociationObject obj), val) <- cmds]
    |::| runCreate dbConf ("IdfyVerification" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (IdfyVerificationObject obj), val) <- cmds]
    |::| runCreate dbConf ("Image" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (ImageObject obj), val) <- cmds]
    |::| runCreate dbConf ("OperatingCity" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (OperatingCityObject obj), val) <- cmds]
    |::| runCreate dbConf ("VehicleRegistrationCertificate" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (VehicleRegistrationCertificateObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverQuote" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverQuoteObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverReferral" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverReferralObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverStats" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverStatsObject obj), val) <- cmds]
    |::| runCreate dbConf ("Estimate" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (EstimateObject obj), val) <- cmds]
    |::| runCreate dbConf ("Exophone" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (ExophoneObject obj), val) <- cmds]
    |::| runCreate dbConf ("FareParameters" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FareParametersObject obj), val) <- cmds]
    |::| runCreate dbConf ("FareParametersProgressiveDetails" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FareParametersProgressiveDetailsObject obj), val) <- cmds]
    |::| runCreate dbConf ("FareParametersSlabDetails" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FareParametersSlabDetailsObject obj), val) <- cmds]
    |::| runCreate dbConf ("FarePolicy" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FarePolicyObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverExtraFeeBounds" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverExtraFeeBoundsObject obj), val) <- cmds]
    |::| runCreate dbConf ("FarePolicyProgressiveDetails" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FarePolicyProgressiveDetailsObject obj), val) <- cmds]
    |::| runCreate dbConf ("FarePolicyProgressiveDetailsPerExtraKmRateSection" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FarePolicyProgressiveDetailsPerExtraKmRateSectionObject obj), val) <- cmds]
    |::| runCreate dbConf ("FarePolicySlabDetailsSlab" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FarePolicySlabDetailsSlabObject obj), val) <- cmds]
    |::| runCreate dbConf ("RestrictedExtraFare" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RestrictedExtraFareObject obj), val) <- cmds]
    |::| runCreate dbConf ("FareProduct" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FareProductObject obj), val) <- cmds]
    |::| runCreate dbConf ("Geometry" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (GeometryObject obj), val) <- cmds]
    |::| runCreate dbConf ("Comment" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (CommentObject obj), val) <- cmds]
    |::| runCreate dbConf ("IssueCategory" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (IssueCategoryObject obj), val) <- cmds]
    |::| runCreate dbConf ("IssueOption" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (IssueOptionObject obj), val) <- cmds]
    |::| runCreate dbConf ("IssueReport" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (IssueReportObject obj), val) <- cmds]
    |::| runCreate dbConf ("IssueTranslation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (IssueTranslationObject obj), val) <- cmds]
    |::| runCreate dbConf ("LeaderBoardConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (LeaderBoardConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("PlaceNameCache" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (PlaceNameCacheObject obj), val) <- cmds]
    |::| runCreate dbConf ("MediaFile" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MediaFileObject obj), val) <- cmds]
    |::| runCreate dbConf ("Merchant" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverIntelligentPoolConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverIntelligentPoolConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverPoolConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverPoolConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantLeaderBoardConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantLeaderBoardConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantMessage" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantMessageObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantPaymentMethod" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantPaymentMethodObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantServiceConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantServiceConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantServiceUsageConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantServiceUsageConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantOnboardingDocumentConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantOnboardingDocumentConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("TransporterConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (TransporterConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("Message" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MessageObject obj), val) <- cmds]
    |::| runCreate dbConf ("MessageReport" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MessageReportObject obj), val) <- cmds]
    |::| runCreate dbConf ("MessageTranslation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MessageTranslationObject obj), val) <- cmds]
    |::| runCreate dbConf ("MetaData" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MetaDataObject obj), val) <- cmds]
    |::| runCreate dbConf ("OnboardingDocumentConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (OnboardingDocumentConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("Person" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (PersonObject obj), val) <- cmds]
    |::| runCreate dbConf ("QuoteSpecialZone" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (QuoteSpecialZoneObject obj), val) <- cmds]
    |::| runCreate dbConf ("Rating" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RatingObject obj), val) <- cmds]
    |::| runCreate dbConf ("Ride" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RideObject obj), val) <- cmds]
    |::| runCreate dbConf ("RideDetails" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RideDetailsObject obj), val) <- cmds]
    |::| runCreate dbConf ("RiderDetails" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RiderDetailsObject obj), val) <- cmds]
    |::| runCreate dbConf ("SearchRequest" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (SearchRequestObject obj), val) <- cmds]
    |::| runCreate dbConf ("SearchReqLocation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (SearchReqLocationObject obj), val) <- cmds]
    |::| runCreate dbConf ("SearchRequestForDriver" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (SearchRequestForDriverObject obj), val) <- cmds]
    |::| runCreate dbConf ("SearchRequestSpecialZone" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (SearchRequestSpecialZoneObject obj), val) <- cmds]
    |::| runCreate dbConf ("SearchTry" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (SearchTryObject obj), val) <- cmds]
    |::| runCreate dbConf ("Vehicle" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (VehicleObject obj), val) <- cmds]
    |::| runCreate dbConf ("FeedbackForm" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FeedbackFormObject obj), val) <- cmds]
    |::| runCreate dbConf ("Feedback" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FeedbackObject obj), val) <- cmds]
    |::| runCreate dbConf ("FeedbackBadge" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FeedbackBadgeObject obj), val) <- cmds]
    |::| runCreate dbConf ("BecknRequest" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BecknRequestObject obj), val) <- cmds]
    |::| runCreate dbConf ("RegistryMapFallback" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RegistryMapFallbackObject obj), val) <- cmds]
  where
    runCreate dbConf model object = do
      let dbObjects = map (\(dbObject, _, _) -> dbObject) object
          byteStream = map (\(_, bts, _) -> bts) object
          entryIds = map (\(_, _, entryId) -> entryId) object
          cmdsToErrorQueue = map ("command" :: String,) byteStream
      maxRetries <- EL.runIO getMaxRetries
      if null object then pure [Right []] else runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds 0 maxRetries False

    runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries ignoreDuplicates = do
      res <- CDB.createMultiSqlWoReturning dbConf dbObjects ignoreDuplicates
      case (res, index) of -- Ignore duplicate entry
        (Right _, _) -> do
          -- EL.logInfoV ("Drainer Info" :: Text) $ createDBLogEntry model "CREATE" (t2 - t1) (cpuT2 - cpuT1) dbObjects -- Logging group latencies
          pure [Right entryIds]
        (Left (ET.DBError (ET.SQLError (ET.MysqlError (ET.MysqlSqlError 1062 err))) _), _) -> do
          EL.logInfo ("DUPLICATE_ENTRY" :: Text) ("Got duplicate entry for model: " <> model <> ", Error message: " <> err)
          void $ publishDBSyncMetric $ Event.DuplicateEntryCreate model
          -- Is retry delay needed here? :/
          runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries True -- Should retry count be increased here? :/
        (Left (ET.DBError (ET.SQLError (ET.PostgresError (ET.PostgresSqlError ("23505" :: Text) _ errMsg _ _))) _), _) -> do
          EL.logInfo ("DUPLICATE_ENTRY" :: Text) ("Got duplicate entry for model: " <> model <> ", Error message: " <> errMsg)
          void $ publishDBSyncMetric $ Event.DuplicateEntryCreate model
          -- Is retry delay needed here? :/
          runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries True -- Should retry count be increased here? :/
        (Left _, y) | y < maxRetries -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" model
          EL.runIO $ delay =<< getRetryDelay
          runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds (index + 1) maxRetries ignoreDuplicates -- Should we pass the same ignoreDuplicates or should we pass False here.
        (Left x, _) -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" model
          EL.logError ("Create failed: " :: Text) (show cmdsToErrorQueue <> "\n Error: " <> show x :: Text)
          pure [Left entryIds]
