{-# OPTIONS_GHC -Wno-type-defaults #-}

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
    DeleteDBCommand id _ _ _ _ (AppInstallsDeleteOptions _ whereClause) -> runDelete id val whereClause ("AppInstalls" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BlackListOrgDeleteOptions _ whereClause) -> runDelete id val whereClause ("BlackListOrg" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingDeleteOptions _ whereClause) -> runDelete id val whereClause ("Booking" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingLocationDeleteOptions _ whereClause) -> runDelete id val whereClause ("BookingLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingCancellationReasonDeleteOptions _ whereClause) -> runDelete id val whereClause ("BookingCancellationReason" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CallbackRequestDeleteOptions _ whereClause) -> runDelete id val whereClause ("CallbackRequest" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CallStatusDeleteOptions _ whereClause) -> runDelete id val whereClause ("CallStatus" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CancellationReasonDeleteOptions _ whereClause) -> runDelete id val whereClause ("CancellationReason" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverOfferDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverOffer" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (EstimateDeleteOptions _ whereClause) -> runDelete id val whereClause ("Estimate" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (EstimateBreakupDeleteOptions _ whereClause) -> runDelete id val whereClause ("EstimateBreakup" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (ExophoneDeleteOptions _ whereClause) -> runDelete id val whereClause ("Exophone" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FareBreakupDeleteOptions _ whereClause) -> runDelete id val whereClause ("FareBreakup" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (GeometryDeleteOptions _ whereClause) -> runDelete id val whereClause ("Geometry" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IssueDeleteOptions _ whereClause) -> runDelete id val whereClause ("Issue" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DirectionsCacheDeleteOptions _ whereClause) -> runDelete id val whereClause ("DirectionsCache" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PlaceNameCacheDeleteOptions _ whereClause) -> runDelete id val whereClause ("PlaceNameCache" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantDeleteOptions _ whereClause) -> runDelete id val whereClause ("Merchant" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantMessageDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantMessage" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantPaymentMethodDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantPaymentMethod" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantServiceConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantServiceConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantServiceUsageConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantServiceUsageConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (OnSearchEventDeleteOptions _ whereClause) -> runDelete id val whereClause ("OnSearchEvent" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PaymentOrderDeleteOptions _ whereClause) -> runDelete id val whereClause ("PaymentOrder" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PaymentTransactionDeleteOptions _ whereClause) -> runDelete id val whereClause ("PaymentTransaction" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PersonDeleteOptions _ whereClause) -> runDelete id val whereClause ("Person" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PersonDefaultEmergencyNumberDeleteOptions _ whereClause) -> runDelete id val whereClause ("PersonDefaultEmergencyNumber" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PersonFlowStatusDeleteOptions _ whereClause) -> runDelete id val whereClause ("PersonFlowStatus" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (QuoteDeleteOptions _ whereClause) -> runDelete id val whereClause ("Quote" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RegistrationTokenDeleteOptions _ whereClause) -> runDelete id val whereClause ("RegistrationToken" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RentalSlabDeleteOptions _ whereClause) -> runDelete id val whereClause ("RentalSlab" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RideDeleteOptions _ whereClause) -> runDelete id val whereClause ("Ride" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SavedReqLocationDeleteOptions _ whereClause) -> runDelete id val whereClause ("SavedReqLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchRequestDeleteOptions _ whereClause) -> runDelete id val whereClause ("SearchRequest" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchReqLocationDeleteOptions _ whereClause) -> runDelete id val whereClause ("SearchReqLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SosDeleteOptions _ whereClause) -> runDelete id val whereClause ("Sos" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SpecialZoneQuoteDeleteOptions _ whereClause) -> runDelete id val whereClause ("SpecialZoneQuote" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (TripTermsDeleteOptions _ whereClause) -> runDelete id val whereClause ("TripTerms" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (WebengageDeleteOptions _ whereClause) -> runDelete id val whereClause ("Webengage" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FeedbackFormDeleteOptions _ whereClause) -> runDelete id val whereClause ("FeedbackForm" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (HotSpotConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("HotSpotConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BecknRequestDeleteOptions _ whereClause) -> runDelete id val whereClause ("BecknRequest" :: Text) =<< dbConf
  where
    runDelete id value whereClause model dbConf = do
      maxRetries <- EL.runIO getMaxRetries
      runDeleteWithRetries id value whereClause model dbConf 0 maxRetries

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
