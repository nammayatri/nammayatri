module DBSync.Update where

import Config.Env
import Data.Aeson as A
import Data.Either.Extra (mapLeft)
import Data.Maybe (fromJust)
import Data.Text as T
import Database.Beam as B hiding (runUpdate)
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.Types
import EulerHS.KVConnector.Utils as EKU
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import EulerHS.Types as ET
import qualified Kernel.Beam.Types as KBT
import Sequelize (Model, Set, Where)
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

runUpdateCommands :: (UpdateDBCommand, ByteString) -> Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdateCommands (cmd, val) = do
  let dbConf = fromJust <$> EL.getOption KBT.PsqlDbCfg
  case cmd of
    UpdateDBCommand id _ _ _ _ (RegistrationTokenOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("RegistrationToken" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BapMetadataOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("BapMetadata" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BookingOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Booking" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BookingLocationOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("BookingLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BookingCancellationReasonOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("BookingCancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BusinessEventOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("BusinessEvent" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (CallStatusOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("CallStatus" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (CancellationReasonOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("CancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverFlowStatusOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverFlowStatus" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverBlockReasonOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverBlockReason" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverFeeOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverFee" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverInformationOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverInformation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverLocationOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (AadhaarOtpReqOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("AadhaarOtpReq" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (AadhaarOtpVerifyOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("AadhaarOtpVerify" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (AadhaarVerificationOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("AadhaarVerification" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverLicenseOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverLicense" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverRCAssociationOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverRCAssociation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IdfyVerificationOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("IdfyVerification" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (ImageOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Image" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (OperatingCityOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("OperatingCity" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (VehicleRegistrationCertificateOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("VehicleRegistrationCertificate" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverQuoteOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverQuote" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverReferralOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverReferral" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverStatsOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverStats" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (EstimateOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Estimate" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (ExophoneOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Exophone" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FareParametersOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("FareParameters" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FareParametersProgressiveDetailsOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("FareParametersProgressiveDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FareParametersSlabDetailsOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("FareParametersSlabDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FarePolicyOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("FarePolicy" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverExtraFeeBoundsOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverExtraFeeBounds" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FarePolicyProgressiveDetailsOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("FarePolicyProgressiveDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("FarePolicyProgressiveDetailsPerExtraKmRateSection" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FarePolicySlabDetailsSlabOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("FarePolicySlabDetailsSlab" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RestrictedExtraFareOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("RestrictedExtraFare" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FareProductOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("FareProduct" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (GeometryOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Geometry" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (CommentOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Comment" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IssueCategoryOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("IssueCategory" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IssueOptionOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("IssueOption" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IssueReportOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("IssueReport" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IssueTranslationOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("IssueTranslation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (LeaderBoardConfigOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("LeaderBoardConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (PlaceNameCacheOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("PlaceNameCache" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MediaFileOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MediaFile" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Merchant" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverIntelligentPoolConfigOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverIntelligentPoolConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverPoolConfigOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DriverPoolConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantLeaderBoardConfigOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantLeaderBoardConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantMessageOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantMessage" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantPaymentMethodOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantPaymentMethod" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantServiceConfigOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantServiceConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantServiceUsageConfigOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantServiceUsageConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantOnboardingDocumentConfigOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantOnboardingDocumentConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (TransporterConfigOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TransporterConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MessageOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Message" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MessageReportOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MessageReport" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MessageTranslationOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MessageTranslation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MetaDataOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MetaData" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (OnboardingDocumentConfigOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("OnboardingDocumentConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (PersonOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Person" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (QuoteSpecialZoneOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("QuoteSpecialZone" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RatingOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Rating" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RideOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Ride" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RideDetailsOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("RideDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RiderDetailsOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("RiderDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchRequestOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("SearchRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchReqLocationOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("SearchReqLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchRequestForDriverOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("SearchRequestForDriver" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchRequestSpecialZoneOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("SearchRequestSpecialZone" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchTryOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("SearchTry" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (VehicleOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Vehicle" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FeedbackFormOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("FeedbackForm" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FeedbackOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Feedback" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FeedbackBadgeOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("FeedbackBadge" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BecknRequestOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("BecknRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RegistryMapFallbackOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("RegistryMapFallback" :: Text) =<< dbConf
  where
    runUpdate id value setClause whereClause model dbConf = do
      maxRetries <- EL.runIO getMaxRetries
      runUpdateWithRetries id value setClause whereClause model dbConf 0 maxRetries

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
