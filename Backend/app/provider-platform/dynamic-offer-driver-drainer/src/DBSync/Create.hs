{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module DBSync.Create where

import Config.Env
import Control.Exception (throwIO)
import Data.Aeson (Value (Object), encode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Scientific as Sci
import Data.Text as T hiding (any, map, null)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Database.PostgreSQL.Simple hiding (QueryError)
import Database.PostgreSQL.Simple.Types
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.Language as EL
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types as ET
import Kafka.Producer as KafkaProd
import Kafka.Producer as Producer
import Kernel.Beam.Lib.Utils (getMappings, replaceMappings)
import qualified Kernel.Beam.Types as KBT
import Text.Casing (quietSnake)
import Types.DBSync
import Types.Event as Event
import Utils.Utils

runCreateCommands :: [(CreateDBCommand, ByteString)] -> Text -> ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
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
    |::| runCreate dbConf streamKey ("BecknRequest" :: Text) [(obj, val, entryId, BecknRequestObject obj) | (CreateDBCommand entryId _ _ _ _ (BecknRequestObject obj), val) <- cmds]
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
      let dbObjects = map (\(dbObject, _, _, _) -> dbObject) object
          byteStream = map (\(_, bts, _, _) -> bts) object
          entryIds = map (\(_, _, entryId, _) -> entryId) object
          cmdsToErrorQueue = map ("command" :: String,) byteStream
      let objList = map getData byteStream
      let mapList = mapMaybe thd objList
      EL.logDebug ("ELEM Vijay" :: Text) (show objList)
      EL.logDebug ("List Vijay" :: Text) (show mapList)
      let insertList = mapMaybe (generateInsertForTable . getData) byteStream
      EL.logDebug ("Insert Vijay" :: Text) (show insertList)
      Env {..} <- ask
      maxRetries <- EL.runIO getMaxRetries
      if null object || model `elem` _dontEnableDbTables then pure [Right entryIds] else runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds 0 maxRetries False
    -- If KAFKA_PUSH is false then entry will be there in DB Else Create entry in Kafka only.
    thd (_, _, third) = third
    runCreateInKafka dbConf streamKey' model object = do
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
    -- Create entry in DB if KAFKA_PUSH key is set to false. Else creates in both.
    runCreateInKafkaAndDb dbConf streamKey' model object = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runCreate dbConf streamKey' model object
        else
          if null object
            then pure [Right []]
            else do
              let entryIds = map (\(_, _, entryId, _) -> entryId) object
              kResults <- runCreateInKafka dbConf streamKey' model object
              case kResults of
                [Right _] -> runCreate dbConf streamKey' model object
                _ -> pure [Left entryIds]

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
    message topicName event =
      ProducerRecord
        { prTopic = TopicName topicName,
          prPartition = UnassignedPartition,
          prKey = Just $ TE.encodeUtf8 streamKey,
          prValue = Just . LBS.toStrict $ encode event
        }

-- runCreateQuery :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
-- runCreateQuery createDataEntries _ =
--   let (mbModel, mbObject, mbMappings) = getData (snd createDataEntries)
--       insertQuery = generateInsertForTable (mbModel, mbObject, mbMappings)
--    in case insertQuery of
--         Just query -> do
--           EL.logDebug ("QueryVijay" :: Text) (show insertQuery)
--           Env {..} <- ask
--           res <- EL.runIO $  execute_ _pgConnection . Query $ TE.encodeUtf8 query
--           EL.logInfo ("Insert result " :: Text) (show res)
--           if res > 0
--             then do
--               EL.logInfo ("Insert successful " :: Text) ("Insert successful for query" <> query <> "with streamData" <> TE.decodeUtf8 (snd createDataEntries))
--               pure $ Right (fst createDataEntries)
--             else do
--               EL.logInfo ("Insert failed " :: Text) ("Insert failed for query" <> query <> "with streamData" <> TE.decodeUtf8 (snd createDataEntries))
--               pure $ Left (fst createDataEntries)
--         Nothing -> do
--           EL.logInfo ("No query generated for streamData " :: Text) (TE.decodeUtf8 (snd createDataEntries))
--           pure $ Left (fst createDataEntries)

newtype QueryError = QueryError Text
  deriving (Show)

instance Exception QueryError

-- Execute a query and throw a custom error if it fails
executeQuery :: Connection -> Query -> IO ()
executeQuery conn query = do
  result <- try $ execute_ conn query :: IO (Either SomeException Int64)
  case result of
    Left e -> throwIO $ QueryError $ "Query execution failed: " <> T.pack (show e)
    Right _ -> return ()

runCreateQuery :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runCreateQuery createDataEntries _ =
  let (mbModel, mbObject, mbMappings) = getData (snd createDataEntries)
      insertQuery = generateInsertForTable (mbModel, mbObject, mbMappings)
   in case insertQuery of
        Just query -> do
          EL.logDebug ("QueryVijay" :: Text) (show insertQuery)
          Env {..} <- ask
          result <- EL.runIO $ try $ executeQuery _pgConnection (Query $ TE.encodeUtf8 query)
          case result of
            Left (QueryError errorMsg) -> do
              EL.logError ("Query execution error " :: Text) errorMsg
              pure $ Left (fst createDataEntries)
            Right _ -> do
              EL.logInfo ("Insert successful " :: Text) ("Insert successful for query" <> query <> "with streamData" <> TE.decodeUtf8 (snd createDataEntries))
              pure $ Right (fst createDataEntries)
        -- Right res -> do
        --   EL.logInfo ("Insert result " :: Text) (show res)
        --   if res > 0
        --     then do
        --       EL.logInfo ("Insert successful " :: Text) ("Insert successful for query" <> query <> "with streamData" <> TE.decodeUtf8 (snd createDataEntries))
        --       pure $ Right (fst createDataEntries)
        --     else do
        --       EL.logInfo ("Insert failed " :: Text) ("Insert failed for query" <> query <> "with streamData" <> TE.decodeUtf8 (snd createDataEntries))
        --       pure $ Left (fst createDataEntries)
        Nothing -> do
          EL.logInfo ("No query generated for streamData: " :: Text) (TE.decodeUtf8 (snd createDataEntries))
          pure $ Left (fst createDataEntries)

getData :: ByteString -> (Maybe T.Text, Maybe A.Object, Maybe A.Object)
getData dbCommandByteString = do
  case A.decode $ BSL.fromStrict dbCommandByteString of
    Just _decodedDBCommandObject@(Data.Aeson.Object o) ->
      let mbModel = case HM.lookup "contents" o of
            Just _commandArray@(A.Array a) -> case V.last a of
              _commandObject@(Data.Aeson.Object command) -> case HM.lookup "tag" command of
                Just (A.String modelTag) -> pure $ T.pack (quietSnake (T.unpack (T.take (T.length modelTag - 6) modelTag)))
                _ -> Nothing
              _ -> Nothing
            _ -> Nothing

          mbObject = case HM.lookup "contents" o of
            Just _commandArray@(A.Array a) -> case V.last a of
              _commandObject@(Data.Aeson.Object command) -> case HM.lookup "contents" command of
                Just (Data.Aeson.Object object) -> return object
                _ -> Nothing
              _ -> Nothing
            _ -> Nothing
          mbMapings = case HM.lookup "contents" o of
            Just _commandArray@(A.Array a) -> case V.last a of
              _commandObject@(Data.Aeson.Object command) -> case HM.lookup "mappings" command of
                Just (Data.Aeson.Object mappingArray) -> return mappingArray
                _ -> Nothing
              _ -> Nothing
            _ -> Nothing
       in (mbModel, mbObject, mbMapings)
    _ -> (Nothing, Nothing, Nothing)

generateInsertForTable :: (Maybe Text, Maybe A.Object, Maybe A.Object) -> Maybe Text
generateInsertForTable (mbModel, mbObject, mbMappings) =
  case (mbModel, mbObject) of
    (Just model, Just object) -> do
      let object' = removeNullAndEmptyValues object
      let keys = HM.keys object'
          newKeys = replaceMappings keys (fromJust mbMappings)
          newKeys' = map (quote' . T.pack . quietSnake . T.unpack) newKeys
          values = map (quote . valueToText . fromMaybe "" . (`HM.lookup` object')) keys
          table = "atlas_driver_offer_bpp." <> model
          inserts = T.intercalate ", " newKeys'
          valuesList = T.intercalate ", " values
      Just $ "INSERT INTO " <> table <> " (" <> inserts <> ") VALUES (" <> valuesList <> ");"
    _ -> Nothing
  where
    quote x = "'" <> x <> "'"
    quote' x = "\"" <> x <> "\""

removeNullAndEmptyValues :: A.Object -> A.Object
removeNullAndEmptyValues = HM.filter (not . isNullOrEmpty)

-- Check if a value is null or an empty string
isNullOrEmpty :: A.Value -> Bool
isNullOrEmpty A.Null = True
isNullOrEmpty (A.String str) = null str
isNullOrEmpty _ = False

replaceMappings :: [T.Text] -> A.Object -> [T.Text]
replaceMappings elements obj = map replaceElement elements
  where
    replaceElement element =
      case HM.lookup element obj of
        Just (A.String value) -> value
        _ -> element

valueToText :: Data.Aeson.Value -> T.Text
valueToText (A.String t) = t
valueToText (A.Number n) =
  if Sci.isInteger n
    then T.pack (show (Sci.coefficient n)) -- Convert to integer if it's an integer
    else T.pack (show (Sci.toRealFloat n)) -- Convert to floating-point
valueToText (A.Bool b) = if b then "true" else "false"
valueToText (A.Array a) = show (map valueToText (V.toList a))
valueToText (A.Object obj) = show (A.encode obj)
valueToText A.Null = "null"
