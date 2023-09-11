{-# OPTIONS_GHC -Wno-type-defaults #-}

module DBSync.Delete where

import Config.Env
import Data.Either.Extra (mapLeft)
import Data.Maybe
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
    DeleteDBCommand id _ _ _ _ (RegistrationTokenDeleteOptions _ whereClause) -> runDelete id val whereClause ("RegistrationToken" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BapMetadataDeleteOptions _ whereClause) -> runDelete id val whereClause ("BapMetadata" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingDeleteOptions _ whereClause) -> runDelete id val whereClause ("Booking" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingLocationDeleteOptions _ whereClause) -> runDelete id val whereClause ("BookingLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BookingCancellationReasonDeleteOptions _ whereClause) -> runDelete id val whereClause ("BookingCancellationReason" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (BusinessEventDeleteOptions _ whereClause) -> runDelete id val whereClause ("BusinessEvent" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CallStatusDeleteOptions _ whereClause) -> runDelete id val whereClause ("CallStatus" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CancellationReasonDeleteOptions _ whereClause) -> runDelete id val whereClause ("CancellationReason" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverFlowStatusDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverFlowStatus" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverBlockReasonDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverBlockReason" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverFeeDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverFee" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverInformationDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverInformation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverLocationDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (AadhaarOtpReqDeleteOptions _ whereClause) -> runDelete id val whereClause ("AadhaarOtpReq" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (AadhaarOtpVerifyDeleteOptions _ whereClause) -> runDelete id val whereClause ("AadhaarOtpVerify" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (AadhaarVerificationDeleteOptions _ whereClause) -> runDelete id val whereClause ("AadhaarVerification" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverLicenseDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverLicense" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverRCAssociationDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverRCAssociation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IdfyVerificationDeleteOptions _ whereClause) -> runDelete id val whereClause ("IdfyVerification" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (ImageDeleteOptions _ whereClause) -> runDelete id val whereClause ("Image" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (OperatingCityDeleteOptions _ whereClause) -> runDelete id val whereClause ("OperatingCity" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (VehicleRegistrationCertificateDeleteOptions _ whereClause) -> runDelete id val whereClause ("VehicleRegistrationCertificate" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverQuoteDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverQuote" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverReferralDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverReferral" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverStatsDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverStats" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (EstimateDeleteOptions _ whereClause) -> runDelete id val whereClause ("Estimate" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (ExophoneDeleteOptions _ whereClause) -> runDelete id val whereClause ("Exophone" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FareParametersDeleteOptions _ whereClause) -> runDelete id val whereClause ("FareParameters" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FareParametersProgressiveDetailsDeleteOptions _ whereClause) -> runDelete id val whereClause ("FareParametersProgressiveDetails" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FareParametersSlabDetailsDeleteOptions _ whereClause) -> runDelete id val whereClause ("FareParametersSlabDetails" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FarePolicyDeleteOptions _ whereClause) -> runDelete id val whereClause ("FarePolicy" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverExtraFeeBoundsDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverExtraFeeBounds" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FarePolicyProgressiveDetailsDeleteOptions _ whereClause) -> runDelete id val whereClause ("FarePolicyProgressiveDetails" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FarePolicyProgressiveDetailsPerExtraKmRateSectionDeleteOptions _ whereClause) -> runDelete id val whereClause ("FarePolicyProgressiveDetailsPerExtraKmRateSection" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FarePolicySlabDetailsSlabDeleteOptions _ whereClause) -> runDelete id val whereClause ("FarePolicySlabDetailsSlab" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RestrictedExtraFareDeleteOptions _ whereClause) -> runDelete id val whereClause ("RestrictedExtraFare" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FareProductDeleteOptions _ whereClause) -> runDelete id val whereClause ("FareProduct" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (GeometryDeleteOptions _ whereClause) -> runDelete id val whereClause ("Geometry" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (CommentDeleteOptions _ whereClause) -> runDelete id val whereClause ("Comment" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IssueCategoryDeleteOptions _ whereClause) -> runDelete id val whereClause ("IssueCategory" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IssueOptionDeleteOptions _ whereClause) -> runDelete id val whereClause ("IssueOption" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IssueReportDeleteOptions _ whereClause) -> runDelete id val whereClause ("IssueReport" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (IssueTranslationDeleteOptions _ whereClause) -> runDelete id val whereClause ("IssueTranslation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (LeaderBoardConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("LeaderBoardConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PlaceNameCacheDeleteOptions _ whereClause) -> runDelete id val whereClause ("PlaceNameCache" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MediaFileDeleteOptions _ whereClause) -> runDelete id val whereClause ("MediaFile" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantDeleteOptions _ whereClause) -> runDelete id val whereClause ("Merchant" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverIntelligentPoolConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverIntelligentPoolConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (DriverPoolConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("DriverPoolConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantLeaderBoardConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantLeaderBoardConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantMessageDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantMessage" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantPaymentMethodDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantPaymentMethod" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantServiceConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantServiceConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantServiceUsageConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantServiceUsageConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MerchantOnboardingDocumentConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantOnboardingDocumentConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (TransporterConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("TransporterConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MessageDeleteOptions _ whereClause) -> runDelete id val whereClause ("Message" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MessageReportDeleteOptions _ whereClause) -> runDelete id val whereClause ("MessageReport" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MessageTranslationDeleteOptions _ whereClause) -> runDelete id val whereClause ("MessageTranslation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (MetaDataDeleteOptions _ whereClause) -> runDelete id val whereClause ("MetaData" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (OnboardingDocumentConfigDeleteOptions _ whereClause) -> runDelete id val whereClause ("OnboardingDocumentConfig" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (PersonDeleteOptions _ whereClause) -> runDelete id val whereClause ("Person" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (QuoteSpecialZoneDeleteOptions _ whereClause) -> runDelete id val whereClause ("QuoteSpecialZone" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RatingDeleteOptions _ whereClause) -> runDelete id val whereClause ("Rating" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RideDeleteOptions _ whereClause) -> runDelete id val whereClause ("Ride" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RideDetailsDeleteOptions _ whereClause) -> runDelete id val whereClause ("RideDetails" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (RiderDetailsDeleteOptions _ whereClause) -> runDelete id val whereClause ("RiderDetails" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchRequestDeleteOptions _ whereClause) -> runDelete id val whereClause ("SearchRequest" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchReqLocationDeleteOptions _ whereClause) -> runDelete id val whereClause ("SearchReqLocation" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchRequestForDriverDeleteOptions _ whereClause) -> runDelete id val whereClause ("SearchRequestForDriver" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchRequestSpecialZoneDeleteOptions _ whereClause) -> runDelete id val whereClause ("SearchRequestSpecialZone" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (SearchTryDeleteOptions _ whereClause) -> runDelete id val whereClause ("SearchTry" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (VehicleDeleteOptions _ whereClause) -> runDelete id val whereClause ("Vehicle" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FeedbackFormDeleteOptions _ whereClause) -> runDelete id val whereClause ("FeedbackForm" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FeedbackDeleteOptions _ whereClause) -> runDelete id val whereClause ("Feedback" :: Text) =<< dbConf
    DeleteDBCommand id _ _ _ _ (FeedbackBadgeDeleteOptions _ whereClause) -> runDelete id val whereClause ("FeedbackBadge" :: Text) =<< dbConf
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
  let topicName = "driver-drainer"
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
