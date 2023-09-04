module DBSync.Create where

import Config.Env
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text.Encoding as TE
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.Language as EL
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types as ET
import Kafka.Producer as KafkaProd
import Kafka.Producer as Producer
import qualified Kernel.Beam.Types as KBT
import Types.DBSync
import Types.Event as Event
import Utils.Utils

runCreateCommands :: Show b => [(CreateDBCommand, b)] -> Text -> ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateCommands cmds streamKey = do
  dbConf <- fromJust <$> L.getOption KBT.PsqlDbCfg

  runCreate dbConf streamKey ("RegistrationToken" :: Text) [(obj, val, entryId, RegistrationTokenObject obj) | (CreateDBCommand entryId _ _ _ _ (RegistrationTokenObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("BapMetadata" :: Text) [(obj, val, entryId, BapMetadataObject obj) | (CreateDBCommand entryId _ _ _ _ (BapMetadataObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Booking" :: Text) [(obj, val, entryId, BookingObject obj) | (CreateDBCommand entryId _ _ _ _ (BookingObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("BookingLocation" :: Text) [(obj, val, entryId, BookingLocationObject obj) | (CreateDBCommand entryId _ _ _ _ (BookingLocationObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("BookingCancellationReason" :: Text) [(obj, val, entryId, BookingCancellationReasonObject obj) | (CreateDBCommand entryId _ _ _ _ (BookingCancellationReasonObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("BusinessEvent" :: Text) [(obj, val, entryId, BusinessEventObject obj) | (CreateDBCommand entryId _ _ _ _ (BusinessEventObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("CallStatus" :: Text) [(obj, val, entryId, CallStatusObject obj) | (CreateDBCommand entryId _ _ _ _ (CallStatusObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("CancellationReason" :: Text) [(obj, val, entryId, CancellationReasonObject obj) | (CreateDBCommand entryId _ _ _ _ (CancellationReasonObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverFlowStatus" :: Text) [(obj, val, entryId, DriverFlowStatusObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverFlowStatusObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverBlockReason" :: Text) [(obj, val, entryId, DriverBlockReasonObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverBlockReasonObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverFee" :: Text) [(obj, val, entryId, DriverFeeObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverFeeObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverInformation" :: Text) [(obj, val, entryId, DriverInformationObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverInformationObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverLocation" :: Text) [(obj, val, entryId, DriverLocationObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverLocationObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("AadhaarOtpReq" :: Text) [(obj, val, entryId, AadhaarOtpReqObject obj) | (CreateDBCommand entryId _ _ _ _ (AadhaarOtpReqObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("AadhaarOtpVerify" :: Text) [(obj, val, entryId, AadhaarOtpVerifyObject obj) | (CreateDBCommand entryId _ _ _ _ (AadhaarOtpVerifyObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("AadhaarVerification" :: Text) [(obj, val, entryId, AadhaarVerificationObject obj) | (CreateDBCommand entryId _ _ _ _ (AadhaarVerificationObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverLicense" :: Text) [(obj, val, entryId, DriverLicenseObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverLicenseObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverRCAssociation" :: Text) [(obj, val, entryId, DriverRCAssociationObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverRCAssociationObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("IdfyVerification" :: Text) [(obj, val, entryId, IdfyVerificationObject obj) | (CreateDBCommand entryId _ _ _ _ (IdfyVerificationObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Image" :: Text) [(obj, val, entryId, ImageObject obj) | (CreateDBCommand entryId _ _ _ _ (ImageObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("OperatingCity" :: Text) [(obj, val, entryId, OperatingCityObject obj) | (CreateDBCommand entryId _ _ _ _ (OperatingCityObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("VehicleRegistrationCertificate" :: Text) [(obj, val, entryId, VehicleRegistrationCertificateObject obj) | (CreateDBCommand entryId _ _ _ _ (VehicleRegistrationCertificateObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverQuote" :: Text) [(obj, val, entryId, DriverQuoteObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverQuoteObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverReferral" :: Text) [(obj, val, entryId, DriverReferralObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverReferralObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverStats" :: Text) [(obj, val, entryId, DriverStatsObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverStatsObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Estimate" :: Text) [(obj, val, entryId, EstimateObject obj) | (CreateDBCommand entryId _ _ _ _ (EstimateObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Exophone" :: Text) [(obj, val, entryId, ExophoneObject obj) | (CreateDBCommand entryId _ _ _ _ (ExophoneObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("FareParameters" :: Text) [(obj, val, entryId, FareParametersObject obj) | (CreateDBCommand entryId _ _ _ _ (FareParametersObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("FareParametersProgressiveDetails" :: Text) [(obj, val, entryId, FareParametersProgressiveDetailsObject obj) | (CreateDBCommand entryId _ _ _ _ (FareParametersProgressiveDetailsObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("FareParametersSlabDetails" :: Text) [(obj, val, entryId, FareParametersSlabDetailsObject obj) | (CreateDBCommand entryId _ _ _ _ (FareParametersSlabDetailsObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("FarePolicy" :: Text) [(obj, val, entryId, FarePolicyObject obj) | (CreateDBCommand entryId _ _ _ _ (FarePolicyObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverExtraFeeBounds" :: Text) [(obj, val, entryId, DriverExtraFeeBoundsObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverExtraFeeBoundsObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("FarePolicyProgressiveDetails" :: Text) [(obj, val, entryId, FarePolicyProgressiveDetailsObject obj) | (CreateDBCommand entryId _ _ _ _ (FarePolicyProgressiveDetailsObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("FarePolicyProgressiveDetailsPerExtraKmRateSection" :: Text) [(obj, val, entryId, FarePolicyProgressiveDetailsPerExtraKmRateSectionObject obj) | (CreateDBCommand entryId _ _ _ _ (FarePolicyProgressiveDetailsPerExtraKmRateSectionObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("FarePolicySlabDetailsSlab" :: Text) [(obj, val, entryId, FarePolicySlabDetailsSlabObject obj) | (CreateDBCommand entryId _ _ _ _ (FarePolicySlabDetailsSlabObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("RestrictedExtraFare" :: Text) [(obj, val, entryId, RestrictedExtraFareObject obj) | (CreateDBCommand entryId _ _ _ _ (RestrictedExtraFareObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("FareProduct" :: Text) [(obj, val, entryId, FareProductObject obj) | (CreateDBCommand entryId _ _ _ _ (FareProductObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Geometry" :: Text) [(obj, val, entryId, GeometryObject obj) | (CreateDBCommand entryId _ _ _ _ (GeometryObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Comment" :: Text) [(obj, val, entryId, CommentObject obj) | (CreateDBCommand entryId _ _ _ _ (CommentObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("IssueCategory" :: Text) [(obj, val, entryId, IssueCategoryObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueCategoryObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("IssueOption" :: Text) [(obj, val, entryId, IssueOptionObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueOptionObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("IssueReport" :: Text) [(obj, val, entryId, IssueReportObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueReportObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("IssueTranslation" :: Text) [(obj, val, entryId, IssueTranslationObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueTranslationObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("LeaderBoardConfig" :: Text) [(obj, val, entryId, LeaderBoardConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (LeaderBoardConfigObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("PlaceNameCache" :: Text) [(obj, val, entryId, PlaceNameCacheObject obj) | (CreateDBCommand entryId _ _ _ _ (PlaceNameCacheObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("MediaFile" :: Text) [(obj, val, entryId, MediaFileObject obj) | (CreateDBCommand entryId _ _ _ _ (MediaFileObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Merchant" :: Text) [(obj, val, entryId, MerchantObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverIntelligentPoolConfig" :: Text) [(obj, val, entryId, DriverIntelligentPoolConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverIntelligentPoolConfigObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("DriverPoolConfig" :: Text) [(obj, val, entryId, DriverPoolConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverPoolConfigObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("MerchantLeaderBoardConfig" :: Text) [(obj, val, entryId, MerchantLeaderBoardConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantLeaderBoardConfigObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("MerchantMessage" :: Text) [(obj, val, entryId, MerchantMessageObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantMessageObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("MerchantPaymentMethod" :: Text) [(obj, val, entryId, MerchantPaymentMethodObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantPaymentMethodObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("MerchantServiceConfig" :: Text) [(obj, val, entryId, MerchantServiceConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantServiceConfigObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("MerchantServiceUsageConfig" :: Text) [(obj, val, entryId, MerchantServiceUsageConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantServiceUsageConfigObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("MerchantOnboardingDocumentConfig" :: Text) [(obj, val, entryId, MerchantOnboardingDocumentConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantOnboardingDocumentConfigObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("TransporterConfig" :: Text) [(obj, val, entryId, TransporterConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (TransporterConfigObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Message" :: Text) [(obj, val, entryId, MessageObject obj) | (CreateDBCommand entryId _ _ _ _ (MessageObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("MessageReport" :: Text) [(obj, val, entryId, MessageReportObject obj) | (CreateDBCommand entryId _ _ _ _ (MessageReportObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("MessageTranslation" :: Text) [(obj, val, entryId, MessageTranslationObject obj) | (CreateDBCommand entryId _ _ _ _ (MessageTranslationObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("MetaData" :: Text) [(obj, val, entryId, MetaDataObject obj) | (CreateDBCommand entryId _ _ _ _ (MetaDataObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("OnboardingDocumentConfig" :: Text) [(obj, val, entryId, OnboardingDocumentConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (OnboardingDocumentConfigObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Person" :: Text) [(obj, val, entryId, PersonObject obj) | (CreateDBCommand entryId _ _ _ _ (PersonObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("QuoteSpecialZone" :: Text) [(obj, val, entryId, QuoteSpecialZoneObject obj) | (CreateDBCommand entryId _ _ _ _ (QuoteSpecialZoneObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Rating" :: Text) [(obj, val, entryId, RatingObject obj) | (CreateDBCommand entryId _ _ _ _ (RatingObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Ride" :: Text) [(obj, val, entryId, RideObject obj) | (CreateDBCommand entryId _ _ _ _ (RideObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("RideDetails" :: Text) [(obj, val, entryId, RideDetailsObject obj) | (CreateDBCommand entryId _ _ _ _ (RideDetailsObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("RiderDetails" :: Text) [(obj, val, entryId, RiderDetailsObject obj) | (CreateDBCommand entryId _ _ _ _ (RiderDetailsObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("SearchRequest" :: Text) [(obj, val, entryId, SearchRequestObject obj) | (CreateDBCommand entryId _ _ _ _ (SearchRequestObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("SearchReqLocation" :: Text) [(obj, val, entryId, SearchReqLocationObject obj) | (CreateDBCommand entryId _ _ _ _ (SearchReqLocationObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("SearchRequestForDriver" :: Text) [(obj, val, entryId, SearchRequestForDriverObject obj) | (CreateDBCommand entryId _ _ _ _ (SearchRequestForDriverObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("SearchRequestSpecialZone" :: Text) [(obj, val, entryId, SearchRequestSpecialZoneObject obj) | (CreateDBCommand entryId _ _ _ _ (SearchRequestSpecialZoneObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("SearchTry" :: Text) [(obj, val, entryId, SearchTryObject obj) | (CreateDBCommand entryId _ _ _ _ (SearchTryObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Vehicle" :: Text) [(obj, val, entryId, VehicleObject obj) | (CreateDBCommand entryId _ _ _ _ (VehicleObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("FeedbackForm" :: Text) [(obj, val, entryId, FeedbackFormObject obj) | (CreateDBCommand entryId _ _ _ _ (FeedbackFormObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("Feedback" :: Text) [(obj, val, entryId, FeedbackObject obj) | (CreateDBCommand entryId _ _ _ _ (FeedbackObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("FeedbackBadge" :: Text) [(obj, val, entryId, FeedbackBadgeObject obj) | (CreateDBCommand entryId _ _ _ _ (FeedbackBadgeObject obj), val) <- cmds]
    |::| runCreate dbConf streamKey ("BecknRequest" :: Text) [(obj, val, entryId, BecknRequestObject obj) | (CreateDBCommand entryId _ _ _ _ (BecknRequestObject obj), val) <- cmds]
    |::| runCreate dbConf ("RegistryMapFallback" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RegistryMapFallbackObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverGoHomeRequest" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverGoHomeRequestObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverHomeLocation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverHomeLocationObject obj), val) <- cmds]
    |::| runCreate dbConf ("GoHomeConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (GoHomeConfigObject obj), val) <- cmds]
  where
    runCreate dbConf streamKey' model object = do
      let dbObjects = map (\(dbObject, _, _, _) -> dbObject) object
          byteStream = map (\(_, bts, _, _) -> bts) object
          entryIds = map (\(_, _, entryId, _) -> entryId) object
          dataObjects = map (\(_, _, _, dataObject) -> dataObject) object
          cmdsToErrorQueue = map ("command" :: String,) byteStream
      Env {..} <- ask
      void $ EL.runIO $ streamDriverDrainerCreates _kafkaConnection dataObjects streamKey'
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

streamDriverDrainerCreates :: ToJSON a => Producer.KafkaProducer -> [a] -> Text -> IO ()
streamDriverDrainerCreates producer dbObject streamKey = do
  let topicName = "driver-drainer-creates"
  mapM_ (KafkaProd.produceMessage producer . message topicName) dbObject
  where
    message topicName event =
      ProducerRecord
        { prTopic = TopicName topicName,
          prPartition = UnassignedPartition,
          prKey = Just $ TE.encodeUtf8 streamKey,
          prValue = Just . LBS.toStrict $ encode event
        }
