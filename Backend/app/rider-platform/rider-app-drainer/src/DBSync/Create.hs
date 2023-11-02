module DBSync.Create where

import Config.Env
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock.POSIX as Time
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.Language as EL
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types as ET
import Kafka.Producer as KafkaProd
import Kafka.Producer as Producer
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude (UTCTime)
import qualified Kernel.Streaming.Kafka.KafkaTable as Kafka
import Kernel.Types.Error
import System.Timeout (timeout)
import Text.Casing (quietSnake)
import qualified "rider-app" Tools.Beam.UtilsTH as App
import Types.DBSync
import Types.Event as Event
import Utils.Utils

runCreateCommands :: Show b => [(CreateDBCommand, b)] -> Text -> ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateCommands cmds streamKey = do
  dbConf <- fromJust <$> L.getOption KBT.PsqlDbCfg
  runCreateInKafkaAndDb dbConf streamKey ("AppInstalls" :: Text) [(obj, val, entryId, AppInstallsObject obj) | (CreateDBCommand entryId _ _ _ _ (AppInstallsObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("AppInstalls" :: Text) [(obj, val, entryId, AppInstallsObject obj) | (CreateDBCommand entryId _ _ _ _ (AppInstallsObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("BlackListOrg" :: Text) [(obj, val, entryId, BlackListOrgObject obj) | (CreateDBCommand entryId _ _ _ _ (BlackListOrgObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Booking" :: Text) [(obj, val, entryId, BookingObject obj) | (CreateDBCommand entryId _ _ _ _ (BookingObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("BookingCancellationReason" :: Text) [(obj, val, entryId, BookingCancellationReasonObject obj) | (CreateDBCommand entryId _ _ _ _ (BookingCancellationReasonObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("CallbackRequest" :: Text) [(obj, val, entryId, CallbackRequestObject obj) | (CreateDBCommand entryId _ _ _ _ (CallbackRequestObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("CallStatus" :: Text) [(obj, val, entryId, CallStatusObject obj) | (CreateDBCommand entryId _ _ _ _ (CallStatusObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("CancellationReason" :: Text) [(obj, val, entryId, CancellationReasonObject obj) | (CreateDBCommand entryId _ _ _ _ (CancellationReasonObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("DriverOffer" :: Text) [(obj, val, entryId, DriverOfferObject obj) | (CreateDBCommand entryId _ _ _ _ (DriverOfferObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Estimate" :: Text) [(obj, val, entryId, EstimateObject obj) | (CreateDBCommand entryId _ _ _ _ (EstimateObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("EstimateBreakup" :: Text) [(obj, val, entryId, EstimateBreakupObject obj) | (CreateDBCommand entryId _ _ _ _ (EstimateBreakupObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Exophone" :: Text) [(obj, val, entryId, ExophoneObject obj) | (CreateDBCommand entryId _ _ _ _ (ExophoneObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FareBreakup" :: Text) [(obj, val, entryId, FareBreakupObject obj) | (CreateDBCommand entryId _ _ _ _ (FareBreakupObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Geometry" :: Text) [(obj, val, entryId, GeometryObject obj) | (CreateDBCommand entryId _ _ _ _ (GeometryObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Issue" :: Text) [(obj, val, entryId, IssueObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Comment" :: Text) [(obj, val, entryId, CommentObject obj) | (CreateDBCommand entryId _ _ _ _ (CommentObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("IssueCategory" :: Text) [(obj, val, entryId, IssueCategoryObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueCategoryObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("IssueOption" :: Text) [(obj, val, entryId, IssueOptionObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueOptionObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("IssueReport" :: Text) [(obj, val, entryId, IssueReportObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueReportObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("IssueTranslation" :: Text) [(obj, val, entryId, IssueTranslationObject obj) | (CreateDBCommand entryId _ _ _ _ (IssueTranslationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("PlaceNameCache" :: Text) [(obj, val, entryId, PlaceNameCacheObject obj) | (CreateDBCommand entryId _ _ _ _ (PlaceNameCacheObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Merchant" :: Text) [(obj, val, entryId, MerchantObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantMessage" :: Text) [(obj, val, entryId, MerchantMessageObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantMessageObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantPaymentMethod" :: Text) [(obj, val, entryId, MerchantPaymentMethodObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantPaymentMethodObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantServiceConfig" :: Text) [(obj, val, entryId, MerchantServiceConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantServiceConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantServiceUsageConfig" :: Text) [(obj, val, entryId, MerchantServiceUsageConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantServiceUsageConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MerchantConfig" :: Text) [(obj, val, entryId, MerchantConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (MerchantConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("MediaFile" :: Text) [(obj, val, entryId, MediaFileObject obj) | (CreateDBCommand entryId _ _ _ _ (MediaFileObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("OnSearchEvent" :: Text) [(obj, val, entryId, OnSearchEventObject obj) | (CreateDBCommand entryId _ _ _ _ (OnSearchEventObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("PaymentOrder" :: Text) [(obj, val, entryId, PaymentOrderObject obj) | (CreateDBCommand entryId _ _ _ _ (PaymentOrderObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("PaymentTransaction" :: Text) [(obj, val, entryId, PaymentTransactionObject obj) | (CreateDBCommand entryId _ _ _ _ (PaymentTransactionObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Person" :: Text) [(obj, val, entryId, PersonObject obj) | (CreateDBCommand entryId _ _ _ _ (PersonObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("PersonDefaultEmergencyNumber" :: Text) [(obj, val, entryId, PersonDefaultEmergencyNumberObject obj) | (CreateDBCommand entryId _ _ _ _ (PersonDefaultEmergencyNumberObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("PersonFlowStatus" :: Text) [(obj, val, entryId, PersonFlowStatusObject obj) | (CreateDBCommand entryId _ _ _ _ (PersonFlowStatusObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Quote" :: Text) [(obj, val, entryId, QuoteObject obj) | (CreateDBCommand entryId _ _ _ _ (QuoteObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("RegistrationToken" :: Text) [(obj, val, entryId, RegistrationTokenObject obj) | (CreateDBCommand entryId _ _ _ _ (RegistrationTokenObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("RentalSlab" :: Text) [(obj, val, entryId, RentalSlabObject obj) | (CreateDBCommand entryId _ _ _ _ (RentalSlabObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Rating" :: Text) [(obj, val, entryId, RatingObject obj) | (CreateDBCommand entryId _ _ _ _ (RatingObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Ride" :: Text) [(obj, val, entryId, RideObject obj) | (CreateDBCommand entryId _ _ _ _ (RideObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("SavedReqLocation" :: Text) [(obj, val, entryId, SavedReqLocationObject obj) | (CreateDBCommand entryId _ _ _ _ (SavedReqLocationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("SearchRequest" :: Text) [(obj, val, entryId, SearchRequestObject obj) | (CreateDBCommand entryId _ _ _ _ (SearchRequestObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Sos" :: Text) [(obj, val, entryId, SosObject obj) | (CreateDBCommand entryId _ _ _ _ (SosObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("SpecialZoneQuote" :: Text) [(obj, val, entryId, SpecialZoneQuoteObject obj) | (CreateDBCommand entryId _ _ _ _ (SpecialZoneQuoteObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("TripTerms" :: Text) [(obj, val, entryId, TripTermsObject obj) | (CreateDBCommand entryId _ _ _ _ (TripTermsObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Webengage" :: Text) [(obj, val, entryId, WebengageObject obj) | (CreateDBCommand entryId _ _ _ _ (WebengageObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("FeedbackForm" :: Text) [(obj, val, entryId, FeedbackFormObject obj) | (CreateDBCommand entryId _ _ _ _ (FeedbackFormObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("HotSpotConfig" :: Text) [(obj, val, entryId, HotSpotConfigObject obj) | (CreateDBCommand entryId _ _ _ _ (HotSpotConfigObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("BecknRequest" :: Text) [(obj, val, entryId, BecknRequestObject obj) | (CreateDBCommand entryId _ _ _ _ (BecknRequestObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("Location" :: Text) [(obj, val, entryId, LocationObject obj) | (CreateDBCommand entryId _ _ _ _ (LocationObject obj), val) <- cmds]
    |::| runCreateInKafkaAndDb dbConf streamKey ("LocationMapping" :: Text) [(obj, val, entryId, LocationMappingObject obj) | (CreateDBCommand entryId _ _ _ _ (LocationMappingObject obj), val) <- cmds]
  where
    runCreate dbConf _ model object = do
      let dbObjects = map (\(dbObject, _, _, _) -> dbObject) object
          byteStream = map (\(_, bts, _, _) -> bts) object
          entryIds = map (\(_, _, entryId, _) -> entryId) object
          cmdsToErrorQueue = map ("command" :: String,) byteStream
      Env {..} <- ask
      maxRetries <- EL.runIO getMaxRetries
      if null object || model `elem` _dontEnableDbTables then pure [Right entryIds] else runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds 0 maxRetries False
    -- If KAFKA_PUSH is false then entry will be there in DB Else Create entry in Kafka only.
    runCreateInKafka dbConf streamKey' model object = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runCreate dbConf streamKey' model object
        else
          if null object
            then pure [Right []]
            else do
              let entryIds = map (\(_, _, entryId', _) -> entryId') object
              Env {..} <- ask
              tables <- L.getOption KBT.Tables
              let tableName = textToSnakeCaseText model
              pushToS3 <- case tables of
                Nothing -> L.throwException $ InternalError "Tables not found"
                Just tables' -> do
                  pure $ tableName `elem` tables'.kafkaS3Tables
              res <- EL.runIO $ streamRiderDrainerCreates _kafkaConnection tableName pushToS3 object streamKey'
              either
                ( \err -> do
                    void $ publishDBSyncMetric Event.KafkaPushFailure
                    EL.logError ("ERROR:" :: Text) $ ("Kafka Rider Create Error: " :: Text) <> show err
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
      case (res, index) of
        (Right _, _) -> do
          pure [Right entryIds]
        (Left (ET.DBError (ET.SQLError (ET.MysqlError (ET.MysqlSqlError 1062 err))) _), _) -> do
          EL.logInfo ("DUPLICATE_ENTRY" :: Text) ("Got duplicate entry for model: " <> model <> ", Error message: " <> err)
          void $ publishDBSyncMetric $ Event.DuplicateEntryCreate model
          runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries True
        (Left (ET.DBError (ET.SQLError (ET.PostgresError (ET.PostgresSqlError ("23505" :: Text) _ errMsg _ _))) _), _) -> do
          EL.logInfo ("DUPLICATE_ENTRY" :: Text) ("Got duplicate entry for model: " <> model <> ", Error message: " <> errMsg)
          void $ publishDBSyncMetric $ Event.DuplicateEntryCreate model
          runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries True
        (Left _, y) | y < maxRetries -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" model
          EL.runIO $ delay =<< getRetryDelay
          runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds (index + 1) maxRetries ignoreDuplicates
        (Left x, _) -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" model
          EL.logError ("Create failed: " :: Text) (show cmdsToErrorQueue <> "\n Error: " <> show x :: Text)
          pure [Left entryIds]

streamRiderDrainerCreates ::
  ( ToJSON (table Identity)
  ) =>
  Producer.KafkaProducer ->
  Text ->
  Bool ->
  [(table Identity, b, EL.KVDBStreamEntryID, DBCreateObject)] ->
  Text ->
  IO (Either Text ())
streamRiderDrainerCreates producer tableName pushToS3 dbObject streamKey = do
  result' <- forM dbObject $ \obj -> do
    result'' <- KafkaProd.produceMessage producer . message $ obj
    flushResult <- timeout (5 * 60 * 1000000) $ KafkaProd.flushProducer producer
    pure $ case flushResult of
      Just _ -> maybe (Right ()) (Left . show @Text) result''
      Nothing -> Left "KafkaProd.flushProducer timed out after 5 minutes"

  if any isLeft result' then pure $ Left ("Kafka Error: " <> show result') else pure $ Right ()
  where
    message (obj, _, entryId, dbCreateObject) = do
      let (topicName, kafkaObject) =
            if pushToS3
              then do
                let timestamp = mkTimeStamp entryId
                let kafkaTable =
                      Kafka.KafkaTable
                        { schemaName = T.pack App.currentSchemaName,
                          tableName,
                          tableContent = toJSON obj,
                          timestamp
                        }
                (mkS3TableTopicName timestamp, encode kafkaTable)
              else (riderDrainerTopicName, encode dbCreateObject)
      ProducerRecord
        { prTopic = topicName,
          prPartition = UnassignedPartition,
          prKey = Just $ TE.encodeUtf8 streamKey,
          prValue = Just . LBS.toStrict $ kafkaObject
        }

riderDrainerTopicName :: TopicName
riderDrainerTopicName = TopicName "rider-drainer"

mkTimeStamp :: EL.KVDBStreamEntryID -> UTCTime
mkTimeStamp (EL.KVDBStreamEntryID posixTime _) = Time.posixSecondsToUTCTime $ fromInteger (posixTime `div` 1000)

mkS3TableTopicName :: UTCTime -> TopicName
mkS3TableTopicName timestamp = do
  TopicName $ "kafka-table" <> "_" <> show (Kafka.countTopicNumber timestamp)

textToSnakeCaseText :: Text -> Text
textToSnakeCaseText = T.pack . quietSnake . T.unpack
