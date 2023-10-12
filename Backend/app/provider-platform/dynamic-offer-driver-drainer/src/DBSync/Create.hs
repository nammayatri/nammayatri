{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module DBSync.Create where

import Config.Env
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text as T hiding (elem)
import qualified Data.Text.Encoding as TE
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.Language as EL
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types as ET
import Kafka.Producer as KafkaProd
import Kafka.Producer as Producer
import Kernel.Beam.Lib.Utils (getMappings, replaceMappings)
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.Streaming.Kafka.KafkaTable as Kafka
import qualified "dynamic-offer-driver-app" Storage.Beam.BecknRequest as BR
import System.Timeout (timeout)
import Types.DBSync
import Types.Event as Event
import Utils.Utils

runCreateCommands :: Show b => [(CreateDBCommand, b)] -> Text -> ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateCommands cmds streamKey = do
  dbConf <- fromJust <$> L.getOption KBT.PsqlDbCfg

  runCreateInKafkaAndDb dbConf streamKey ("RegistrationToken" :: Text) [(obj, val, entryId, RegistrationTokenObject obj) | (CreateDBCommand entryId _ _ _ _ (RegistrationTokenObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("BapMetadata" :: Text) [(obj, val, entryId, BapMetadataObject obj) | (CreateDBCommand entryId _ _ _ _ (BapMetadataObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Booking" :: Text) [(obj, val, entryId, BookingObject obj) | (CreateDBCommand entryId _ _ _ _ (BookingObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("BookingCancellationReason" :: Text) [(obj, val, entryId, BookingCancellationReasonObject obj) | (CreateDBCommand entryId _ _ _ _ (BookingCancellationReasonObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("BusinessEvent" :: Text) [(obj, val, entryId, BusinessEventObject obj) | (CreateDBCommand entryId _ _ _ _ (BusinessEventObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("CallStatus" :: Text) [(obj, val, entryId, CallStatusObject obj) | (CreateDBCommand entryId _ _ _ _ (CallStatusObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("CancellationReason" :: Text) [(obj, val, entryId, CancellationReasonObject obj) | (CreateDBCommand entryId _ _ _ _ (CancellationReasonObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverBlockReason" :: Text) [(obj, val, entryId, DriverBlockReasonObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverBlockReasonObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FleetDriverAssociation" :: Text) [(obj, val, entryId, DriverBlockReasonObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverBlockReasonObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverPlan" :: Text) [(obj, val, entryId, DriverPlanObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverPlanObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverFee" :: Text) [(obj, val, entryId, DriverFeeObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverFeeObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverInformation" :: Text) [(obj, val, entryId, DriverInformationObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverInformationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("AadhaarOtpReq" :: Text) [(obj, val, entryId, AadhaarOtpReqObject obj) | (CreateDBCommand entryId _ _ _ _ (AadhaarOtpReqObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("AadhaarOtpVerify" :: Text) [(obj, val, entryId, AadhaarOtpVerifyObject obj) | (CreateDBCommand entryId _ _ _ _ (AadhaarOtpVerifyObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("AadhaarVerification" :: Text) [(obj, val, entryId, AadhaarVerificationObject obj) | (CreateDBCommand entryId _ _ _ _ (AadhaarVerificationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverLicense" :: Text) [(obj, val, entryId, DriverLicenseObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverLicenseObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverRcAssociation" :: Text) [(obj, val, entryId, DriverRcAssociationObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverRcAssociationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("IdfyVerification" :: Text) [(obj, val, entryId, IdfyVerificationObject obj) | (CreateDBCommand entryId _ _ _ _ (IdfyVerificationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Image" :: Text) [(obj, val, entryId, ImageObject obj) | (CreateDBCommand entryId _ _ _ _ (ImageObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("VehicleRegistrationCertificate" :: Text) [(obj, val, entryId, VehicleRegistrationCertificateObject obj) | (CreateDBCommand entryId _ _ _ _ (VehicleRegistrationCertificateObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverQuote" :: Text) [(obj, val, entryId, DriverQuoteObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverQuoteObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverReferral" :: Text) [(obj, val, entryId, DriverReferralObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverReferralObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverStats" :: Text) [(obj, val, entryId, DriverStatsObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverStatsObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Estimate" :: Text) [(obj, val, entryId, EstimateObject obj) | (CreateDBCommand entryId _ _ _ _ (EstimateObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Exophone" :: Text) [(obj, val, entryId, ExophoneObject obj) | (CreateDBCommand entryId _ _ _ _ (ExophoneObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FareParameters" :: Text) [(obj, val, entryId, FareParametersObject obj) | (CreateDBCommand entryId _ _ _ _ (FareParametersObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FareParametersProgressiveDetails" :: Text) [(obj, val, entryId, FareParametersProgressiveDetailsObject obj) | (CreateDBCommand entryId _ _ _ _ (FareParametersProgressiveDetailsObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FareParametersSlabDetails" :: Text) [(obj, val, entryId, FareParametersSlabDetailsObject obj) | (CreateDBCommand entryId _ _ _ _ (FareParametersSlabDetailsObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FarePolicy" :: Text) [(obj, val, entryId, FarePolicyObject obj) | (CreateDBCommand entryId _ _ _ _ (FarePolicyObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverExtraFeeBounds" :: Text) [(obj, val, entryId, DriverExtraFeeBoundsObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverExtraFeeBoundsObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FarePolicyProgressiveDetails" :: Text) [(obj, val, entryId, FarePolicyProgressiveDetailsObject obj) | (CreateDBCommand entryId _ _ _ _ (FarePolicyProgressiveDetailsObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FarePolicyProgressiveDetailsPerExtraKmRateSection" :: Text) [(obj, val, entryId, FarePolicyProgressiveDetailsPerExtraKmRateSectionObject obj) | (CreateDBCommand entryId _ _ _ _ (FarePolicyProgressiveDetailsPerExtraKmRateSectionObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FarePolicySlabDetailsSlab" :: Text) [(obj, val, entryId, FarePolicySlabDetailsSlabObject obj) | (CreateDBCommand entryId _ _ _ _ (FarePolicySlabDetailsSlabObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("RestrictedExtraFare" :: Text) [(obj, val, entryId, RestrictedExtraFareObject obj) | (CreateDBCommand entryId _ _ _ _ (RestrictedExtraFareObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FareProduct" :: Text) [(obj, val, entryId, FareProductObject obj) | (CreateDBCommand entryId _ _ _ _ (FareProductObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Geometry" :: Text) [(obj, val, entryId, GeometryObject obj) | (CreateDBCommand entryId _ _ _ _ (GeometryObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Comment" :: Text) [(obj, val, entryId, CommentObject obj) | (CreateDBCommand entryId _ _ _ _ (CommentObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("IssueCategory" :: Text) [(obj, val, entryId, IssueCategoryObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueCategoryObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("IssueOption" :: Text) [(obj, val, entryId, IssueOptionObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueOptionObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("IssueReport" :: Text) [(obj, val, entryId, IssueReportObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueReportObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("IssueTranslation" :: Text) [(obj, val, entryId, IssueTranslationObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueTranslationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("PlaceNameCache" :: Text) [(obj, val, entryId, PlaceNameCacheObject obj) | (CreateDBCommand entryId _ _ _ _ (PlaceNameCacheObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Mandate" :: Text) [(obj, val, entryId, MandateObject obj) | (CreateDBCommand entryId _ _ _ _ (MandateObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MediaFile" :: Text) [(obj, val, entryId, MediaFileObject obj) | (CreateDBCommand entryId _ _ _ _ (MediaFileObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Merchant" :: Text) [(obj, val, entryId, MerchantObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverIntelligentPoolConfig" :: Text) [(obj, val, entryId, DriverIntelligentPoolConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverIntelligentPoolConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverPoolConfig" :: Text) [(obj, val, entryId, DriverPoolConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverPoolConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantLeaderBoardConfig" :: Text) [(obj, val, entryId, MerchantLeaderBoardConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantLeaderBoardConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantMessage" :: Text) [(obj, val, entryId, MerchantMessageObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantMessageObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantPaymentMethod" :: Text) [(obj, val, entryId, MerchantPaymentMethodObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantPaymentMethodObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantServiceConfig" :: Text) [(obj, val, entryId, MerchantServiceConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantServiceConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantServiceUsageConfig" :: Text) [(obj, val, entryId, MerchantServiceUsageConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantServiceUsageConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantOnboardingDocumentConfig" :: Text) [(obj, val, entryId, MerchantOnboardingDocumentConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantOnboardingDocumentConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("TransporterConfig" :: Text) [(obj, val, entryId, TransporterConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (TransporterConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Message" :: Text) [(obj, val, entryId, MessageObject obj) | (CreateDBCommand entryId _ _ _ _ (MessageObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MessageReport" :: Text) [(obj, val, entryId, MessageReportObject obj) | (CreateDBCommand entryId _ _ _ _ (MessageReportObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MessageTranslation" :: Text) [(obj, val, entryId, MessageTranslationObject obj) | (CreateDBCommand entryId _ _ _ _ (MessageTranslationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MetaData" :: Text) [(obj, val, entryId, MetaDataObject obj) | (CreateDBCommand entryId _ _ _ _ (MetaDataObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Person" :: Text) [(obj, val, entryId, PersonObject obj) | (CreateDBCommand entryId _ _ _ _ (PersonObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("QuoteSpecialZone" :: Text) [(obj, val, entryId, QuoteSpecialZoneObject obj) | (CreateDBCommand entryId _ _ _ _ (QuoteSpecialZoneObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Rating" :: Text) [(obj, val, entryId, RatingObject obj) | (CreateDBCommand entryId _ _ _ _ (RatingObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Ride" :: Text) [(obj, val, entryId, RideObject obj) | (CreateDBCommand entryId _ _ _ _ (RideObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("RideDetails" :: Text) [(obj, val, entryId, RideDetailsObject obj) | (CreateDBCommand entryId _ _ _ _ (RideDetailsObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("RiderDetails" :: Text) [(obj, val, entryId, RiderDetailsObject obj) | (CreateDBCommand entryId _ _ _ _ (RiderDetailsObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("SearchRequest" :: Text) [(obj, val, entryId, SearchRequestObject obj) | (CreateDBCommand entryId _ _ _ _ (SearchRequestObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("SearchRequestForDriver" :: Text) [(obj, val, entryId, SearchRequestForDriverObject obj) | (CreateDBCommand entryId _ _ _ _ (SearchRequestForDriverObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("SearchRequestSpecialZone" :: Text) [(obj, val, entryId, SearchRequestSpecialZoneObject obj) | (CreateDBCommand entryId _ _ _ _ (SearchRequestSpecialZoneObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("SearchTry" :: Text) [(obj, val, entryId, SearchTryObject obj) | (CreateDBCommand entryId _ _ _ _ (SearchTryObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Vehicle" :: Text) [(obj, val, entryId, VehicleObject obj) | (CreateDBCommand entryId _ _ _ _ (VehicleObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Volunteer" :: Text) [(obj, val, entryId, VolunteerObject obj) | (CreateDBCommand entryId _ _ _ _ (VolunteerObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FeedbackForm" :: Text) [(obj, val, entryId, FeedbackFormObject obj) | (CreateDBCommand entryId _ _ _ _ (FeedbackFormObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Feedback" :: Text) [(obj, val, entryId, FeedbackObject obj) | (CreateDBCommand entryId _ _ _ _ (FeedbackObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FeedbackBadge" :: Text) [(obj, val, entryId, FeedbackBadgeObject obj) | (CreateDBCommand entryId _ _ _ _ (FeedbackBadgeObject obj), val) <- cmds]
    |::| runCreateInKafkaWithTopicNameAndDb dbConf streamKey ("BecknRequest" :: Text) [(obj, mkKafkaTableTopicName (BR.timeStamp obj), val, entryId, Kafka.mkKafkaTable @BR.BecknRequestT obj (BR.timeStamp obj)) | (CreateDBCommand entryId _ _ _ _ (BecknRequestObject obj), val) <- cmds] -- put KafkaTable to Kafka, not DBCreateObject
    |::| runCreateInKafkaAndDb dbConf streamKey ("RegistryMapFallback" :: Text) [(obj, val, entryId, RegistryMapFallbackObject obj) | (CreateDBCommand entryId _ _ _ _ (RegistryMapFallbackObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverGoHomeRequest" :: Text) [(obj, val, entryId, DriverGoHomeRequestObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverGoHomeRequestObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverHomeLocation" :: Text) [(obj, val, entryId, DriverHomeLocationObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverHomeLocationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("GoHomeConfig" :: Text) [(obj, val, entryId, GoHomeConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (GoHomeConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Location" :: Text) [(obj, val, entryId, LocationObject obj) | (CreateDBCommand entryId _ _ _ _ (LocationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("LocationMapping" :: Text) [(obj, val, entryId, LocationMappingObject obj) | (CreateDBCommand entryId _ _ _ _ (LocationMappingObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("PaymentOrder" :: Text) [(obj, val, entryId, PaymentOrderObject obj) | (CreateDBCommand entryId _ _ _ _ (PaymentOrderObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("PaymentTransaction" :: Text) [(obj, val, entryId, PaymentTransactionObject obj) | (CreateDBCommand entryId _ _ _ _ (PaymentTransactionObject obj), val) <- cmds]
  where
    -- functions for moving drainer data to kafka
    runCreate dbConf _ model object = do
      let dbObjects = map (\(dbObject, _, _, _, _) -> dbObject) object
          byteStream = map (\(_, _, bts, _, _) -> bts) object
          entryIds = map (\(_, _, _, entryId, _) -> entryId) object
          cmdsToErrorQueue = map ("command" :: String,) byteStream
      Env {..} <- ask
      maxRetries <- EL.runIO getMaxRetries
      if null object || model `elem` _dontEnableDbTables then pure [Right entryIds] else runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds 0 maxRetries False
    -- If KAFKA_PUSH is false then entry will be there in DB Else Create entry in Kafka only.
    runCreateInKafkaWithTopicName dbConf streamKey' model object = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runCreate dbConf streamKey' model object
        else
          if null object
            then pure [Right []]
            else do
              let objectIdentity = map (\(a, _, _, _) -> a) object
                  mappings = getMappings objectIdentity
                  newObjects = map (\object' -> replaceMappings (toJSON object') mappings) objectIdentity
                  entryIds = map (\(_, _, entryId', _) -> entryId') object
              Env {..} <- ask
              res <- EL.runIO $ streamDriverDrainerCreates _kafkaConnection newObjects streamKey' model
              either
                ( \_ -> do
                    void $ publishDBSyncMetric Event.KafkaPushFailure
                    EL.logError ("ERROR:" :: Text) ("Kafka Create Error " :: Text)
                    pure [Left entryIds]
                )
                (\_ -> pure [Right entryIds])
                res
    runCreateInKafkaWithTopicNameAndDb dbConf streamKey' model object = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runCreate dbConf streamKey' model object
        else
          if null object
            then pure [Right []]
            else do
              let entryIds = map (\(_, _, _, entryId, _) -> entryId) object
              kResults <- runCreateInKafkaWithTopicName dbConf streamKey' model object
              case kResults of
                [Right _] -> runCreate dbConf streamKey' model object
                _ -> pure [Left entryIds]
    -- Create entry in DB if KAFKA_PUSH key is set to false. Else creates in both.
    runCreateInKafkaAndDb dbConf streamKey' model objects =
      runCreateInKafkaWithTopicNameAndDb dbConf streamKey' model $
        objects <&> (\(obj, val, entryId, dbCreateObj) -> (obj, driverDrainerTopicName, val, entryId, dbCreateObj))

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

streamDriverDrainerCreates :: ToJSON a => Producer.KafkaProducer -> [a] -> Text -> Text -> IO (Either Text ())
streamDriverDrainerCreates producer dbObject streamKey model = do
  let topicName = "adob-sessionizer-" <> T.toLower model
  result' <- mapM (KafkaProd.produceMessage producer . message topicName) dbObject
  if any isJust result' then pure $ Left ("Kafka Error: " <> show result') else pure $ Right ()
  where
    message (event, topicName) =
      ProducerRecord
        { prTopic = topicName,
          prPartition = UnassignedPartition,
          prKey = Just $ TE.encodeUtf8 streamKey,
          prValue = Just . LBS.toStrict $ encode event
        }

driverDrainerTopicName :: TopicName
driverDrainerTopicName = TopicName "driver-drainer"

mkBecknRequestTopicName :: BR.BecknRequest -> TopicName
mkBecknRequestTopicName becknRequest = do
  TopicName $ "driver-beckn-request" <> "_" <> show (BR.countTopicNumber becknRequest.timeStamp)
