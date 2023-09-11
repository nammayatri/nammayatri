{-# OPTIONS_GHC -Wno-type-defaults #-}

module DBSync.Create where

import Config.Env
import Data.Maybe (fromJust)
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.Language as EL
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types as ET
import qualified Kernel.Beam.Types as KBT
import System.Timeout (timeout)
import Types.DBSync
import Types.Event as Event
import Utils.Utils

runCreateCommands :: Show b => [(CreateDBCommand, b)] -> ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateCommands cmds = do
  dbConf <- fromJust <$> L.getOption KBT.PsqlDbCfg
  runCreate dbConf ("AppInstalls" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (AppInstallsObject obj), val) <- cmds]
    |::| runCreate dbConf ("BlackListOrg" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BlackListOrgObject obj), val) <- cmds]
    |::| runCreate dbConf ("Booking" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BookingObject obj), val) <- cmds]
    |::| runCreate dbConf ("BookingLocation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BookingLocationObject obj), val) <- cmds]
    |::| runCreate dbConf ("BookingCancellationReason" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BookingCancellationReasonObject obj), val) <- cmds]
    |::| runCreate dbConf ("CallbackRequest" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (CallbackRequestObject obj), val) <- cmds]
    |::| runCreate dbConf ("CallStatus" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (CallStatusObject obj), val) <- cmds]
    |::| runCreate dbConf ("CancellationReason" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (CancellationReasonObject obj), val) <- cmds]
    |::| runCreate dbConf ("DriverOffer" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DriverOfferObject obj), val) <- cmds]
    |::| runCreate dbConf ("Estimate" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (EstimateObject obj), val) <- cmds]
    |::| runCreate dbConf ("EstimateBreakup" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (EstimateBreakupObject obj), val) <- cmds]
    |::| runCreate dbConf ("Exophone" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (ExophoneObject obj), val) <- cmds]
    |::| runCreate dbConf ("FareBreakup" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FareBreakupObject obj), val) <- cmds]
    |::| runCreate dbConf ("Geometry" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (GeometryObject obj), val) <- cmds]
    |::| runCreate dbConf ("Issue" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (IssueObject obj), val) <- cmds]
    |::| runCreate dbConf ("DirectionsCache" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (DirectionsCacheObject obj), val) <- cmds]
    |::| runCreate dbConf ("PlaceNameCache" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (PlaceNameCacheObject obj), val) <- cmds]
    |::| runCreate dbConf ("Merchant" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantMessage" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantMessageObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantPaymentMethod" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantPaymentMethodObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantServiceConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantServiceConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantServiceUsageConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantServiceUsageConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("MerchantConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (MerchantConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("OnSearchEvent" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (OnSearchEventObject obj), val) <- cmds]
    |::| runCreate dbConf ("PaymentOrder" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (PaymentOrderObject obj), val) <- cmds]
    |::| runCreate dbConf ("PaymentTransaction" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (PaymentTransactionObject obj), val) <- cmds]
    |::| runCreate dbConf ("Person" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (PersonObject obj), val) <- cmds]
    |::| runCreate dbConf ("PersonDefaultEmergencyNumber" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (PersonDefaultEmergencyNumberObject obj), val) <- cmds]
    |::| runCreate dbConf ("PersonFlowStatus" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (PersonFlowStatusObject obj), val) <- cmds]
    |::| runCreate dbConf ("Quote" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (QuoteObject obj), val) <- cmds]
    |::| runCreate dbConf ("RegistrationToken" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RegistrationTokenObject obj), val) <- cmds]
    |::| runCreate dbConf ("RentalSlab" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RentalSlabObject obj), val) <- cmds]
    |::| runCreate dbConf ("Ride" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (RideObject obj), val) <- cmds]
    |::| runCreate dbConf ("SavedReqLocation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (SavedReqLocationObject obj), val) <- cmds]
    |::| runCreate dbConf ("SearchRequest" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (SearchRequestObject obj), val) <- cmds]
    |::| runCreate dbConf ("SearchReqLocation" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (SearchReqLocationObject obj), val) <- cmds]
    |::| runCreate dbConf ("Sos" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (SosObject obj), val) <- cmds]
    |::| runCreate dbConf ("SpecialZoneQuote" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (SpecialZoneQuoteObject obj), val) <- cmds]
    |::| runCreate dbConf ("TripTerms" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (TripTermsObject obj), val) <- cmds]
    |::| runCreate dbConf ("Webengage" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (WebengageObject obj), val) <- cmds]
    |::| runCreate dbConf ("FeedbackForm" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (FeedbackFormObject obj), val) <- cmds]
    |::| runCreate dbConf ("HotSpotConfig" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (HotSpotConfigObject obj), val) <- cmds]
    |::| runCreate dbConf ("BecknRequest" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BecknRequestObject obj), val) <- cmds]
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

streamDriverDrainerCreates :: ToJSON a => Producer.KafkaProducer -> [a] -> Text -> IO (Either Text ())
streamDriverDrainerCreates producer dbObject streamKey = do
  let topicName = "rider-drainer"
  mapM_ (KafkaProd.produceMessage producer . message topicName) dbObject
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
          prKey = Just $ TE.encodeUtf8 streamKey,
          prValue = Just . LBS.toStrict $ encode event
        }
