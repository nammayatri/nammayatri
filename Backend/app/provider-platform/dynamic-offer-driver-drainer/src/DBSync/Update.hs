{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module DBSync.Update where

import Config.Env
import Data.Aeson as A
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Bifunctor as BiFunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Extra (mapLeft)
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
runUpdateCommands (cmd, val) dbStreamKey = do
  let dbConf = fromJust <$> EL.getOption KBT.PsqlDbCfg
  case cmd of
    UpdateDBCommand id _ tag _ _ (RegistrationTokenOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("RegistrationToken" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BapMetadataOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("BapMetadata" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BookingOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Booking" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BookingCancellationReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("BookingCancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BusinessEventOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("BusinessEvent" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (CallStatusOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("CallStatus" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (CancellationReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("CancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverBlockReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverBlockReason" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FleetDriverAssociationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FleetDriverAssociation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverFeeOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverFee" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverInformationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverInformation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (AadhaarOtpReqOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("AadhaarOtpReq" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (AadhaarOtpVerifyOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("AadhaarOtpVerify" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (AadhaarVerificationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("AadhaarVerification" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverLicenseOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverLicense" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverRcAssociationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverRcAssociation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IdfyVerificationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("IdfyVerification" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (ImageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Image" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (VehicleRegistrationCertificateOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("VehicleRegistrationCertificate" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverQuoteOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverQuote" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverReferralOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverReferral" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverStatsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverStats" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (EstimateOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Estimate" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (ExophoneOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Exophone" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FareParametersOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FareParameters" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FareParametersProgressiveDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FareParametersProgressiveDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FareParametersSlabDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FareParametersSlabDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FarePolicyOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FarePolicy" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverExtraFeeBoundsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverExtraFeeBounds" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FarePolicyProgressiveDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FarePolicyProgressiveDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FarePolicyProgressiveDetailsPerExtraKmRateSection" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FarePolicySlabDetailsSlabOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FarePolicySlabDetailsSlab" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RestrictedExtraFareOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("RestrictedExtraFare" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FareProductOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FareProduct" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (GeometryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Geometry" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (CommentOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Comment" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueCategoryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("IssueCategory" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueOptionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("IssueOption" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueReportOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("IssueReport" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueTranslationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("IssueTranslation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PlaceNameCacheOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("PlaceNameCache" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MediaFileOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MediaFile" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Merchant" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverIntelligentPoolConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverIntelligentPoolConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverPoolConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverPoolConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantLeaderBoardConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantLeaderBoardConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantMessageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantMessage" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantPaymentMethodOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantPaymentMethod" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantServiceConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantServiceConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantServiceUsageConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantServiceUsageConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantOnboardingDocumentConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantOnboardingDocumentConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (TransporterConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("TransporterConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MessageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Message" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MessageReportOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MessageReport" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MessageTranslationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MessageTranslation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MetaDataOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MetaData" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PersonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Person" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (QuoteSpecialZoneOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("QuoteSpecialZone" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RatingOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Rating" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RideOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Ride" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RideDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("RideDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RiderDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("RiderDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SearchRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("SearchRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SearchRequestForDriverOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("SearchRequestForDriver" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SearchRequestSpecialZoneOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("SearchRequestSpecialZone" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SearchTryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("SearchTry" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (VehicleOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Vehicle" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (VolunteerOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Volunteer" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FeedbackFormOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FeedbackForm" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FeedbackOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Feedback" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FeedbackBadgeOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FeedbackBadge" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BecknRequestOptions _ setClauses whereClause) -> runUpdate id val dbStreamKey setClauses whereClause ("BecknRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RegistryMapFallbackOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("RegistryMapFallback" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverGoHomeRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverGoHomeRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverHomeLocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverHomeLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (GoHomeConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("GoHomeConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (LocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("LocationConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (LocationMappingOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("LocationMappingConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PaymentOrderOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("PaymentOrder" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PaymentTransactionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("PaymentTransaction" :: Text) =<< dbConf
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
              res'' <- EL.runIO $ streamDriverDrainerUpdates _kafkaConnection updatedJSON dbStreamKey'
              either
                ( \_ -> do
                    void $ publishDBSyncMetric Event.KafkaPushFailure
                    EL.logError ("ERROR:" :: Text) ("Kafka Driver Update Error " :: Text)
                    pure $ Left (UnexpectedError "Kafka Driver Update Error", id)
                )
                (\_ -> pure $ Right id)
                res''
            Left _ -> do
              let updatedJSON = getDbUpdateDataJson model $ updValToJSON $ jsonKeyValueUpdates setClause <> getPKeyandValuesList tag
              Env {..} <- ask
              res'' <- EL.runIO $ streamDriverDrainerUpdates _kafkaConnection updatedJSON dbStreamKey'
              either
                ( \_ -> do
                    void $ publishDBSyncMetric Event.KafkaPushFailure
                    EL.logError ("ERROR:" :: Text) ("Kafka Driver Update Error " :: Text)
                    pure $ Left (UnexpectedError "Kafka Driver Update Error", id)
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
          EL.logError (("Update failed for model: " <> show model) :: Text) (show [("command" :: String, value)] :: Text)
          pure $ Left (UnexpectedError "Update failed for model", id)
        (Right _, _) -> do
          pure $ Right id

streamDriverDrainerUpdates :: ToJSON a => Producer.KafkaProducer -> a -> Text -> IO (Either Text ())
streamDriverDrainerUpdates producer dbObject dbStreamKey = do
  let topicName = "driver-drainer"
  result' <- KafkaProd.produceMessage producer (message topicName dbObject)
  case result' of
    Just err -> pure $ Left $ T.pack ("Kafka Error: " <> show err)
    _ -> pure $ Right ()
  where
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
updValToJSON keyValuePairs = A.Object $ AKM.fromList . map (BiFunctor.first AesonKey.fromText) $ keyValuePairs

getPKeyandValuesList :: Text -> [(Text, A.Value)]
getPKeyandValuesList pKeyAndValue = go (splitOn "_" pKeyTrimmed) []
  where
    go (tName : k : v : rest) acc = go (tName : rest) ((k, A.String v) : acc)
    go _ acc = acc
    pKeyTrimmed = case splitOn "{" pKeyAndValue of
      [] -> ""
      (x : _) -> x
