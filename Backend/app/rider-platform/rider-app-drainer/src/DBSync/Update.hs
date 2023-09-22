{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module DBSync.Update where

import Config.Env
import Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Extra (mapLeft)
import Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import qualified Data.Serialize as Serialize
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Beam as B hiding (runUpdate)
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.Types as EKT
import EulerHS.KVConnector.Utils as Utils
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import EulerHS.Types as ET
import Kafka.Producer as KafkaProd
import Kafka.Producer as Producer
import qualified Kernel.Beam.Functions as BeamFunction
import qualified Kernel.Beam.Types as KBT
import Sequelize (Model, Set, Where)
import System.Timeout (timeout)
import Text.Casing
import Types.DBSync
import Types.Event as Event
import Utils.Utils

updateDB ::
  forall beM be table m.
  ( HasCallStack,
    ET.BeamRuntime be beM,
    ET.BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    EL.MonadFlow m,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity)
  ) =>
  ET.DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  ByteString ->
  m (Either MeshError ())
updateDB dbConf _ setClause whereClause _ =
  do
    either (pure . Left) (pure . Right) . mapLeft MDBError
    =<< CDB.updateOneWoReturning dbConf Nothing setClause whereClause

getUpdatedValue ::
  forall beM be table m.
  ( HasCallStack,
    ET.BeamRuntime be beM,
    ET.BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    EL.MonadFlow m,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity)
  ) =>
  Text ->
  Where be table ->
  m (Either MeshError (table Identity))
getUpdatedValue tag _ = do
  res <- EL.runKVDB BeamFunction.meshConfig.kvRedis $ EL.get $ fromString $ T.unpack tag
  case res of
    Right (Just r) -> do
      let (decodeResult :: MeshResult [table Identity], isLive) = Utils.decodeToField $ BSL.fromChunks [r]
       in case decodeResult of
            Right [decodeRes] -> return $ Right decodeRes
            Right _ -> return $ Left (UnexpectedError "Redis Error: No Data for the key")
            Left _ -> return $ Left (UnexpectedError "Redis Error: Decode Failed")
    _ -> return $ Left (UnexpectedError "Redis Error")

runUpdateCommands :: (UpdateDBCommand, ByteString) -> Text -> Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdateCommands (cmd, val) streamKey = do
  let dbConf = fromJust <$> EL.getOption KBT.PsqlDbCfg
  case cmd of
    UpdateDBCommand id _ tag _ _ (AppInstallsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("AppInstalls" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BlackListOrgOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("BlackListOrg" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BookingOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Booking" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BookingCancellationReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("BookingCancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (CallbackRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("CallbackRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (CallStatusOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("CallStatus" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (CancellationReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("CancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverOfferOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("DriverOffer" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (EstimateOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Estimate" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (EstimateBreakupOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("EstimateBreakup" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (ExophoneOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Exophone" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FareBreakupOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("FareBreakup" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (GeometryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Geometry" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Issue" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (CommentOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Comment" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueCategoryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("IssueCategory" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueOptionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("IssueOption" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueReportOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("IssueReport" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueTranslationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("IssueTranslation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PlaceNameCacheOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("PlaceNameCache" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Merchant" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantMessageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("MerchantMessage" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantPaymentMethodOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("MerchantPaymentMethod" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantServiceConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("MerchantServiceConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantServiceUsageConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("MerchantServiceUsageConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("MerchantConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MediaFileOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("MediaFile" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (OnSearchEventOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("OnSearchEvent" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PaymentOrderOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("PaymentOrder" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PaymentTransactionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("PaymentTransaction" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PersonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Person" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PersonDefaultEmergencyNumberOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("PersonDefaultEmergencyNumber" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PersonFlowStatusOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("PersonFlowStatus" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (QuoteOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Quote" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RegistrationTokenOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("RegistrationToken" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RentalSlabOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("RentalSlab" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RideOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Ride" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SavedReqLocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("SavedReqLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SearchRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("SearchRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SosOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Sos" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SpecialZoneQuoteOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("SpecialZoneQuote" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (TripTermsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("TripTerms" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (WebengageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("Webengage" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FeedbackFormOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("FeedbackForm" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (HotSpotConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses tag whereClause ("HotSpotConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BecknRequestOptions _ setClauses whereClause) -> runUpdate id val streamKey setClauses whereClause ("BecknRequest" :: Text) =<< dbConf
  where
    runUpdate id value _ setClause whereClause model dbConf = do
      maxRetries <- EL.runIO getMaxRetries
      Env {..} <- ask
      if model `elem` _dontEnableDbTables then pure $ Right id else runUpdateWithRetries id value setClause whereClause model dbConf 0 maxRetries
    -- If KAFKA_PUSH is false then entry will be there in DB Else Updates entry in Kafka only.
    runUpdateInKafka id value dbStreamKey' setClause whereClause model dbConf tag = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runUpdate id value dbStreamKey' setClause whereClause model dbConf
        else do
          res <- getUpdatedValue tag whereClause
          case res of
            Right dataObj -> do
              Env {..} <- ask
              let updatedJSON = getDbUpdateDataJson model dataObj
              res'' <- EL.runIO $ streamRiderDrainerUpdates _kafkaConnection updatedJSON dbStreamKey'
              either
                ( \_ -> do
                    void $ publishDBSyncMetric Event.KafkaPushFailure
                    EL.logError ("ERROR:" :: Text) ("Kafka Rider Update Error " :: Text)
                    pure $ Left (UnexpectedError "Kafka Rider Update Error", id)
                )
                (\_ -> pure $ Right id)
                res''
            Left _ -> do
              let updatedJSON = getDbUpdateDataJson model $ updValToJSON $ jsonKeyValueUpdates setClause <> getPKeyandValuesList tag
              Env {..} <- ask
              res'' <- EL.runIO $ streamRiderDrainerUpdates _kafkaConnection updatedJSON dbStreamKey'
              either
                ( \_ -> do
                    void $ publishDBSyncMetric Event.KafkaPushFailure
                    EL.logError ("ERROR:" :: Text) ("Kafka Rider Update Error " :: Text)
                    pure $ Left (UnexpectedError "Kafka Rider Update Error", id)
                )
                (\_ -> pure $ Right id)
                res''

    -- Updates entry in DB if KAFKA_PUSH key is set to false. Else Updates in both.
    runUpdateInKafkaAndDb id value dbStreamKey' setClause tag whereClause model dbConf = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runUpdate id value dbStreamKey' setClause whereClause model dbConf
        else do
          res <- runUpdateInKafka id value dbStreamKey' setClause whereClause model dbConf tag
          either (\_ -> pure $ Left (UnexpectedError "Kafka Error", id)) (\_ -> runUpdate id value dbStreamKey' setClause whereClause model dbConf) res

    runUpdateWithRetries id value setClause whereClause model dbConf retryIndex maxRetries = do
      res <- updateDB dbConf Nothing setClause whereClause value
      case (res, retryIndex) of
        (Left _, y) | y < maxRetries -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" model
          EL.runIO $ delay =<< getRetryDelay
          runUpdateWithRetries id value setClause whereClause model dbConf (retryIndex + 1) maxRetries
        (Left _, _) -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" model
          EL.logError (("Update failed for model: " :: Text) <> T.pack (show model)) (show [("command" :: String, value)] :: Text)
          pure $ Left (UnexpectedError "Update failed for model", id)
        (Right _, _) -> do
          pure $ Right id

streamRiderDrainerUpdates :: ToJSON a => Producer.KafkaProducer -> a -> Text -> IO (Either Text ())
streamRiderDrainerUpdates producer dbObject dbStreamKey = do
  let topicName = "rider-drainer"
  void $ KafkaProd.produceMessage producer (message topicName dbObject)
  flushResult <- timeout (5 * 60 * 1000000) $ prodPush producer
  case flushResult of
    Just _ -> do
      pure $ Right ()
    Nothing -> pure $ Left "KafkaProd.flushProducer timed out after 5 minutes"
  where
    prodPush producer' = KafkaProd.flushProducer producer' >> pure True

    message topicName event =
      ProducerRecord
        { prTopic = TopicName topicName,
          prPartition = UnassignedPartition,
          prKey = Just $ TE.encodeUtf8 dbStreamKey,
          prValue = Just . LBS.toStrict $ encode event
        }

getDbUpdateDataJson :: ToJSON a => Text -> a -> A.Value
getDbUpdateDataJson model a =
  A.object
    [ "contents"
        .= A.toJSON a,
      "tag" .= T.pack (pascal (T.unpack model) <> "Object"),
      "type" .= ("UPDATE" :: Text)
    ]

updValToJSON :: [(Text, A.Value)] -> A.Value
updValToJSON keyValuePairs = A.Object $ HM.fromList keyValuePairs

getPKeyandValuesList :: Text -> [(Text, A.Value)]
getPKeyandValuesList pKeyAndValue = go (splitOn "_" pKeyTrimmed) []
  where
    go (tName : k : v : rest) acc = go (tName : rest) ((k, A.String v) : acc)
    go _ acc = acc
    pKeyTrimmed = case splitOn "{" pKeyAndValue of
      [] -> ""
      (x : _) -> x
