module DBSync.Delete where

import Config.Env
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Extra (mapLeft)
import Data.Maybe
import Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.DBSync
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import Kafka.Producer
import qualified Kafka.Producer as KafkaProd
import qualified Kafka.Producer as Producer
import qualified Kernel.Beam.Types as KBT
import Sequelize
import Text.Casing
import Types.DBSync
import Types.Event as Event
import Utils.Utils

runDeleteCommands :: (DeleteDBCommand, ByteString) -> Text -> Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runDeleteCommands (cmd, val) dbStreamKey = do
  let dbConf = fromJust <$> EL.getOption KBT.PsqlDbCfg
  case cmd of
    DeleteDBCommand id _ _ _ _ (RegistrationTokenDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("RegistrationToken" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BapMetadataDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("BapMetadata" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Booking" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingLocationDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("BookingLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingCancellationReasonDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("BookingCancellationReason" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BusinessEventDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("BusinessEvent" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CallStatusDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("CallStatus" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CancellationReasonDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("CancellationReason" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverFlowStatusDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverFlowStatus" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverBlockReasonDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverBlockReason" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverFeeDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverFee" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverInformationDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverInformation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverLocationDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (AadhaarOtpReqDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("AadhaarOtpReq" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (AadhaarOtpVerifyDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("AadhaarOtpVerify" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (AadhaarVerificationDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("AadhaarVerification" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverLicenseDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverLicense" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverRCAssociationDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverRCAssociation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IdfyVerificationDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("IdfyVerification" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (ImageDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Image" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (OperatingCityDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("OperatingCity" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (VehicleRegistrationCertificateDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("VehicleRegistrationCertificate" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverQuoteDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverQuote" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverReferralDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverReferral" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverStatsDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverStats" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (EstimateDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Estimate" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (ExophoneDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Exophone" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FareParametersDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("FareParameters" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FareParametersProgressiveDetailsDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("FareParametersProgressiveDetails" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FareParametersSlabDetailsDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("FareParametersSlabDetails" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FarePolicyDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("FarePolicy" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverExtraFeeBoundsDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverExtraFeeBounds" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FarePolicyProgressiveDetailsDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("FarePolicyProgressiveDetails" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FarePolicyProgressiveDetailsPerExtraKmRateSectionDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("FarePolicyProgressiveDetailsPerExtraKmRateSection" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FarePolicySlabDetailsSlabDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("FarePolicySlabDetailsSlab" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RestrictedExtraFareDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("RestrictedExtraFare" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FareProductDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("FareProduct" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (GeometryDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Geometry" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CommentDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Comment" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IssueCategoryDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("IssueCategory" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IssueOptionDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("IssueOption" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IssueReportDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("IssueReport" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IssueTranslationDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("IssueTranslation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (LeaderBoardConfigDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("LeaderBoardConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PlaceNameCacheDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("PlaceNameCache" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MediaFileDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("MediaFile" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Merchant" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverIntelligentPoolConfigDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverIntelligentPoolConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverPoolConfigDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("DriverPoolConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantLeaderBoardConfigDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("MerchantLeaderBoardConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantMessageDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("MerchantMessage" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantPaymentMethodDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("MerchantPaymentMethod" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantServiceConfigDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("MerchantServiceConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantServiceUsageConfigDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("MerchantServiceUsageConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantOnboardingDocumentConfigDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("MerchantOnboardingDocumentConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (TransporterConfigDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("TransporterConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MessageDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Message" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MessageReportDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("MessageReport" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MessageTranslationDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("MessageTranslation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MetaDataDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("MetaData" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (OnboardingDocumentConfigDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("OnboardingDocumentConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PersonDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Person" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (QuoteSpecialZoneDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("QuoteSpecialZone" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RatingDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Rating" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RideDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Ride" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RideDetailsDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("RideDetails" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RiderDetailsDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("RiderDetails" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchRequestDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("SearchRequest" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchReqLocationDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("SearchReqLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchRequestForDriverDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("SearchRequestForDriver" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchRequestSpecialZoneDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("SearchRequestSpecialZone" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchTryDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("SearchTry" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (VehicleDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Vehicle" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FeedbackFormDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("FeedbackForm" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FeedbackDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("Feedback" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FeedbackBadgeDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("FeedbackBadge" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BecknRequestDeleteOptions _ whereClause) -> runDelete id val dbStreamKey whereClause ("BecknRequest" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverGoHomeRequestDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverGoHomeRequest" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverHomeLocationDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverHomeLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (GoHomeConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("GoHomeConfig" :: Text) =<< dbConf
  where
    runDelete id value dbStreamKey' whereClause model dbConf = do
      maxRetries <- EL.runIO getMaxRetries
      Env {..} <- ask
      void $ EL.runIO $ streamDriverDrainerDeletes _kafkaConnection (getDbDeleteDataJson model whereClause) dbStreamKey'
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

streamDriverDrainerDeletes :: ToJSON a => Producer.KafkaProducer -> a -> Text -> IO ()
streamDriverDrainerDeletes producer dbObject dbStreamKey = do
  let topicName = "driver-drainer-deletes"
  void $ KafkaProd.produceMessage producer (message topicName dbObject)
  where
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
      "tag" .= T.pack (pascal (T.unpack model))
    ]
