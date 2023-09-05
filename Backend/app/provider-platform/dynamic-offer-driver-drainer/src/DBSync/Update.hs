{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module DBSync.Update where

import Config.Env
import Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Extra (mapLeft)
import Data.Maybe (fromJust)
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Beam as B hiding (runUpdate)
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.DBSync
import EulerHS.KVConnector.Types
import EulerHS.KVConnector.Utils as EKU
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import EulerHS.Types as ET
import Kafka.Producer as KafkaProd
import Kafka.Producer as Producer
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
    FromJSON (table Identity)
  ) =>
  ET.DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  ByteString ->
  m (Either MeshError [A.Value])
updateDB dbConf _ setClause whereClause bts = do
  either (pure . Left) ((Right <$>) . mapM updateModel') . mapLeft MDBError
    =<< runExceptT
      ( do
          updateObj <- ExceptT $ CDB.findAll dbConf Nothing whereClause
          ExceptT $ CDB.updateOneWoReturning dbConf Nothing setClause whereClause
          pure updateObj
      )
  where
    updateModel' model = do
      let val = (EKU.updateModel @be @table) model (EKU.jsonKeyValueUpdates setClause)
      case val of
        Right obj -> pure obj
        Left err -> do
          EL.logError (("Model Update failed: " :: Text) <> T.pack (show err)) (show [("command" :: String, bts)] :: Text)
          pure A.Null

runUpdateCommands :: (UpdateDBCommand, ByteString) -> Text -> Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdateCommands (cmd, val) dbStreamKey = do
  let dbConf = fromJust <$> EL.getOption KBT.PsqlDbCfg
  case cmd of
    UpdateDBCommand id _ _ _ _ (RegistrationTokenOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("RegistrationToken" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BapMetadataOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("BapMetadata" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BookingOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Booking" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BookingLocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("BookingLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BookingCancellationReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("BookingCancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BusinessEventOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("BusinessEvent" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (CallStatusOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("CallStatus" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (CancellationReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("CancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverFlowStatusOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverFlowStatus" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverBlockReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverBlockReason" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverFeeOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverFee" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverInformationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverInformation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverLocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (AadhaarOtpReqOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("AadhaarOtpReq" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (AadhaarOtpVerifyOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("AadhaarOtpVerify" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (AadhaarVerificationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("AadhaarVerification" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverLicenseOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverLicense" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverRCAssociationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverRCAssociation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IdfyVerificationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("IdfyVerification" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (ImageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Image" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (OperatingCityOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("OperatingCity" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (VehicleRegistrationCertificateOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("VehicleRegistrationCertificate" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverQuoteOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverQuote" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverReferralOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverReferral" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverStatsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverStats" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (EstimateOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Estimate" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (ExophoneOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Exophone" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FareParametersOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("FareParameters" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FareParametersProgressiveDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("FareParametersProgressiveDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FareParametersSlabDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("FareParametersSlabDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FarePolicyOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("FarePolicy" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverExtraFeeBoundsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverExtraFeeBounds" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FarePolicyProgressiveDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("FarePolicyProgressiveDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("FarePolicyProgressiveDetailsPerExtraKmRateSection" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FarePolicySlabDetailsSlabOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("FarePolicySlabDetailsSlab" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RestrictedExtraFareOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("RestrictedExtraFare" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FareProductOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("FareProduct" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (GeometryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Geometry" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (CommentOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Comment" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IssueCategoryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("IssueCategory" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IssueOptionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("IssueOption" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IssueReportOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("IssueReport" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IssueTranslationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("IssueTranslation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (LeaderBoardConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("LeaderBoardConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (PlaceNameCacheOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("PlaceNameCache" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MediaFileOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("MediaFile" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Merchant" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverIntelligentPoolConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverIntelligentPoolConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverPoolConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverPoolConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantLeaderBoardConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("MerchantLeaderBoardConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantMessageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("MerchantMessage" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantPaymentMethodOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("MerchantPaymentMethod" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantServiceConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("MerchantServiceConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantServiceUsageConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("MerchantServiceUsageConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantOnboardingDocumentConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("MerchantOnboardingDocumentConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (TransporterConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("TransporterConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MessageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Message" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MessageReportOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("MessageReport" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MessageTranslationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("MessageTranslation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MetaDataOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("MetaData" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (OnboardingDocumentConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("OnboardingDocumentConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (PersonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Person" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (QuoteSpecialZoneOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("QuoteSpecialZone" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RatingOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Rating" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RideOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Ride" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RideDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("RideDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RiderDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("RiderDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("SearchRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchReqLocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("SearchReqLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchRequestForDriverOptions _ setClauses whereClause) -> runUpdateInKafka id val dbStreamKey setClauses whereClause ("SearchRequestForDriver" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchRequestSpecialZoneOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("SearchRequestSpecialZone" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchTryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("SearchTry" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (VehicleOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Vehicle" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FeedbackFormOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("FeedbackForm" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FeedbackOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("Feedback" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FeedbackBadgeOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("FeedbackBadge" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BecknRequestOptions _ setClauses whereClause) -> runUpdate id val dbStreamKey setClauses whereClause ("BecknRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RegistryMapFallbackOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("RegistryMapFallback" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverGoHomeRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverGoHomeRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverHomeLocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("DriverHomeLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (GoHomeConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses whereClause ("GoHomeConfig" :: Text) =<< dbConf
  where
    runUpdate id value _ setClause whereClause model dbConf = do
      maxRetries <- EL.runIO getMaxRetries
      runUpdateWithRetries id value setClause whereClause model dbConf 0 maxRetries

    runUpdateInKafka id value dbStreamKey' setClause whereClause model dbConf = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runUpdate id value dbStreamKey' setClause whereClause model dbConf
        else do
          let setAndWhere = getDbUpdateDataJson model (jsonKeyValueUpdates setClause) whereClause
          Env {..} <- ask
          res <- EL.runIO $ streamDriverDrainerUpdates _kafkaConnection setAndWhere dbStreamKey'
          either (\_ -> pure $ Left (UnexpectedError "Kafka Error", id)) (\_ -> pure $ Right id) res

    runUpdateInKafkaAndDb id value dbStreamKey' setClause whereClause model dbConf = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runUpdate id value dbStreamKey' setClause whereClause model dbConf
        else do
          res <- runUpdateInKafka id value dbStreamKey' setClause whereClause model dbConf
          either (\_ -> pure $ Left (UnexpectedError "Kafka Error", id)) (\_ -> runUpdate id value dbStreamKey' setClause whereClause model dbConf) res

    runUpdateWithRetries id value setClause whereClause model dbConf retryIndex maxRetries = do
      res <- updateDB dbConf Nothing setClause whereClause value
      case (res, retryIndex) of
        (Left _, y) | y < maxRetries -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" model
          EL.runIO $ delay =<< getRetryDelay
          runUpdateWithRetries id value setClause whereClause model dbConf (retryIndex + 1) maxRetries
        (Left x, _) -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" model
          EL.logError (("Update failed: " :: Text) <> T.pack (show x)) (show [("command" :: String, value)] :: Text)
          pure $ Left (x, id)
        (Right _, _) -> do
          pure $ Right id

streamDriverDrainerUpdates :: ToJSON a => Producer.KafkaProducer -> a -> Text -> IO (Either Text ())
streamDriverDrainerUpdates producer dbObject dbStreamKey = do
  let topicName = "driver-drainer"
  res <- KafkaProd.produceMessage producer (message topicName dbObject)
  case res of
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

getDbUpdateDataJson :: forall be table. (Model be table, MeshMeta be table) => Text -> [(Text, A.Value)] -> Where be table -> A.Value
getDbUpdateDataJson model upd whereClause =
  A.object
    [ "contents"
        .= A.object
          [ "set" .= A.object [k .= v | (k, v) <- upd],
            "where" .= modelEncodeWhere whereClause
          ],
      "tag" .= T.pack (pascal (T.unpack model)),
      "type" .= ("UPDATE" :: Text)
    ]
