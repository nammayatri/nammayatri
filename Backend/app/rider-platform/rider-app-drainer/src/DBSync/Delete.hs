{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module DBSync.Delete where

import Config.Env
import Data.Either.Extra (mapLeft)
import Data.Maybe (fromJust)
import Data.Text as T
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Types as KBT
import Sequelize
import System.Timeout (timeout)
import Text.Casing
import Types.DBSync
import Types.Event as Event
import Utils.Utils

runDeleteCommands :: (DeleteDBCommand, ByteString) -> Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runDeleteCommands (cmd, val) = do
  let dbConf = fromJust <$> EL.getOption KBT.PsqlDbCfg
  case cmd of
    DeleteDBCommand id _ _ _ _ (AppInstallsDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("AppInstalls" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BlackListOrgDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("BlackListOrg" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Booking" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingLocationDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("BookingLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingCancellationReasonDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("BookingCancellationReason" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CallbackRequestDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("CallbackRequest" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CallStatusDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("CallStatus" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CancellationReasonDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("CancellationReason" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverOfferDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("DriverOffer" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (EstimateDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Estimate" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (EstimateBreakupDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("EstimateBreakup" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (ExophoneDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Exophone" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FareBreakupDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("FareBreakup" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (GeometryDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Geometry" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IssueDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Issue" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DirectionsCacheDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("DirectionsCache" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PlaceNameCacheDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("PlaceNameCache" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Merchant" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantMessageDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("MerchantMessage" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantPaymentMethodDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("MerchantPaymentMethod" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantServiceConfigDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("MerchantServiceConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantServiceUsageConfigDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("MerchantServiceUsageConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantConfigDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("MerchantConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (OnSearchEventDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("OnSearchEvent" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PaymentOrderDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("PaymentOrder" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PaymentTransactionDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("PaymentTransaction" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PersonDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Person" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PersonDefaultEmergencyNumberDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("PersonDefaultEmergencyNumber" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PersonFlowStatusDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("PersonFlowStatus" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (QuoteDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Quote" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RegistrationTokenDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("RegistrationToken" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RentalSlabDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("RentalSlab" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RideDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Ride" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SavedReqLocationDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("SavedReqLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchRequestDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("SearchRequest" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchReqLocationDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("SearchReqLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SosDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Sos" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SpecialZoneQuoteDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("SpecialZoneQuote" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (TripTermsDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("TripTerms" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (WebengageDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("Webengage" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FeedbackFormDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("FeedbackForm" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (HotSpotConfigDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("HotSpotConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BecknRequestDeleteOptions _ whereClause) -> runDeleteInKafkaAndDb id val dbStreamKey whereClause ("BecknRequest" :: Text) =<< dbConf
  where
    runDelete id value _ whereClause model dbConf = do
      maxRetries <- EL.runIO getMaxRetries
      runDeleteWithRetries id value whereClause model dbConf 0 maxRetries

    runDeleteInKafka id value dbstremKey whereClause model dbConf = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runDelete id value dbstremKey whereClause model dbConf
        else do
          Env {..} <- ask
          res <- EL.runIO $ streamDriverDrainerDeletes _kafkaConnection (getDbDeleteDataJson model whereClause) dbstremKey
          either (\_ -> pure $ Left (UnexpectedError "Kafka Error", id)) (\_ -> pure $ Right id) res

    runDeleteInKafkaAndDb id value dbstremKey whereClause model dbConf = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runDelete id value dbstremKey whereClause model dbConf
        else do
          res <- runDeleteInKafka id value dbstremKey whereClause model dbConf
          either (\_ -> pure $ Left (UnexpectedError "Kafka Error", id)) (\_ -> runDelete id value dbstremKey whereClause model dbConf) res

    runDeleteWithRetries id value whereClause model dbConf retryIndex maxRetries = do
      res <- mapLeft MDBError <$> CDB.deleteAllReturning dbConf whereClause

      case (res, retryIndex) of
        (Left _, y) | y < maxRetries -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Delete" model
          EL.runIO $ delay =<< getRetryDelay
          runDeleteWithRetries id value whereClause model dbConf (retryIndex + 1) maxRetries
        (Left x, _) -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Delete" model
          EL.logError (("Delete failed: " :: Text) <> T.pack (show x)) (show [("command" :: String, value)] :: Text)
          pure $ Left (x, id)
        (Right _, _) -> do
          pure $ Right id

streamDriverDrainerDeletes :: ToJSON a => Producer.KafkaProducer -> a -> Text -> IO (Either Text ())
streamDriverDrainerDeletes producer dbObject dbStreamKey = do
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

getDbDeleteDataJson :: forall be table. (Model be table, MeshMeta be table) => Text -> Where be table -> A.Value
getDbDeleteDataJson model whereClause =
  A.object
    [ "contents"
        .= A.object
          [ "where" .= modelEncodeWhere whereClause
          ],
      "tag" .= T.pack (pascal (T.unpack model)),
      "type" .= ("DELETE" :: Text)
    ]
