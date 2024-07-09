module SharedLogic.CallBPPInternal where

import API.Types.UI.FavouriteDriver
import qualified Data.HashMap.Strict as HM
import Domain.Types.FeedbackForm
import EulerHS.Types (EulerClient, client)
import IssueManagement.Common (IssueReportType)
import Kernel.External.Slack.Types
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common hiding (Error)
import qualified Kernel.Utils.Servant.Client as EC
import Servant hiding (throwError)
import Tools.Metrics (CoreMetrics)

data RefereeLinkInfoReq = RefereeLinkInfoReq
  { referralCode :: Text,
    customerMobileNumber :: Text,
    customerMobileCountryCode :: Text,
    isMultipleDeviceIdExist :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type LinkRefereeAPI =
  "internal"
    :> Capture "merchantId" Text
    :> "referee"
    :> Header "token" Text
    :> ReqBody '[JSON] RefereeLinkInfoReq
    :> Post '[JSON] APISuccess

linkRefereeClient :: Text -> Maybe Text -> RefereeLinkInfoReq -> EulerClient APISuccess
linkRefereeClient = client likeRefereeApi

likeRefereeApi :: Proxy LinkRefereeAPI
likeRefereeApi = Proxy

linkReferee ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  Bool ->
  m APISuccess
linkReferee apiKey internalUrl merchantId referralCode phoneNumber countryCode isMultipleDeviceIdExist = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (linkRefereeClient merchantId (Just apiKey) (RefereeLinkInfoReq referralCode phoneNumber countryCode (Just isMultipleDeviceIdExist))) "LinkReferee" likeRefereeApi

type FeedbackFormAPI =
  "internal"
    :> "beckn"
    :> "booking"
    :> ( "feedback"
           :> ReqBody '[JSON] FeedbackFormReq
           :> Post '[JSON] APISuccess
       )

feedbackFormClient :: FeedbackFormReq -> EulerClient APISuccess
feedbackFormClient = client (Proxy @FeedbackFormAPI)

feedbackFormApi :: Proxy FeedbackFormAPI
feedbackFormApi = Proxy

feedbackForm ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  FeedbackFormReq ->
  m APISuccess
feedbackForm internalUrl request = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (feedbackFormClient request) "FeedbackForm" feedbackFormApi

data CancellationDuesReq = CancellationDuesReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type DisputeCancellationDuesAPI =
  "internal"
    :> Capture "merchantId" Text
    :> Capture "merchantCity" Context.City
    :> "dispute"
    :> Header "token" Text
    :> ReqBody '[JSON] CancellationDuesReq
    :> Post '[JSON] APISuccess

disputeCancellationDuesClient :: Text -> Context.City -> Maybe Text -> CancellationDuesReq -> EulerClient APISuccess
disputeCancellationDuesClient = client disputeCancellationDuesApi

disputeCancellationDuesApi :: Proxy DisputeCancellationDuesAPI
disputeCancellationDuesApi = Proxy

disputeCancellationDues ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Context.City ->
  m APISuccess
disputeCancellationDues apiKey internalUrl merchantId phoneNumber countryCode merchantCity = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (disputeCancellationDuesClient merchantId merchantCity (Just apiKey) (CancellationDuesReq phoneNumber countryCode)) "DisputeCancellationDues" disputeCancellationDuesApi

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { customerCancellationDues :: HighPrecMoney,
    customerCancellationDuesWithCurrency :: Maybe PriceAPIEntity,
    disputeChancesUsed :: Int,
    canBlockCustomer :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type GetCancellationDuesDetailsAPI =
  "internal"
    :> Capture "merchantId" Text
    :> Capture "merchantCity" Context.City
    :> "getCancellationDuesDetails"
    :> Header "token" Text
    :> ReqBody '[JSON] CancellationDuesReq
    :> Get '[JSON] CancellationDuesDetailsRes

getCancellationDuesDetailsClient :: Text -> Context.City -> Maybe Text -> CancellationDuesReq -> EulerClient CancellationDuesDetailsRes
getCancellationDuesDetailsClient = client getCancellationDuesDetailsApi

getCancellationDuesDetailsApi :: Proxy GetCancellationDuesDetailsAPI
getCancellationDuesDetailsApi = Proxy

getCancellationDuesDetails ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Context.City ->
  m CancellationDuesDetailsRes
getCancellationDuesDetails apiKey internalUrl merchantId phoneNumber countryCode merchantCity = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getCancellationDuesDetailsClient merchantId merchantCity (Just apiKey) (CancellationDuesReq phoneNumber countryCode)) "GetCancellationDuesDetails" getCancellationDuesDetailsApi

data GetFavouriteDriverInfoReq = GetFavouriteDriverInfoReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type GetFavouriteDriversAPI =
  "internal"
    :> Capture "merchantId" Text
    :> "getFavouriteDrivers"
    :> Header "token" Text
    :> ReqBody '[JSON] GetFavouriteDriverInfoReq
    :> Get '[JSON] [FavouriteDriverResp]

getFavouriteDriversClient :: Text -> Maybe Text -> GetFavouriteDriverInfoReq -> EulerClient [FavouriteDriverResp]
getFavouriteDriversClient = client getFavouriteDriversApi

getFavouriteDriversApi :: Proxy GetFavouriteDriversAPI
getFavouriteDriversApi = Proxy

getFavouriteDriverList ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m [FavouriteDriverResp]
getFavouriteDriverList apiKey internalUrl merchantId phoneNumber countryCode = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getFavouriteDriversClient merchantId (Just apiKey) (GetFavouriteDriverInfoReq phoneNumber countryCode)) "GetFavouriteDrivers" getFavouriteDriversApi

type RemoveFavouriteDriverAPI =
  "internal"
    :> Capture "merchantId" Text
    :> Capture "driverId" Text
    :> "removeFavouriteDriver"
    :> Header "token" Text
    :> ReqBody '[JSON] GetFavouriteDriverInfoReq
    :> Post '[JSON] APISuccess

removeFavouriteDriversClient :: Text -> Text -> Maybe Text -> GetFavouriteDriverInfoReq -> EulerClient APISuccess
removeFavouriteDriversClient = client removeFavouriteDriverApi

removeFavouriteDriverApi :: Proxy RemoveFavouriteDriverAPI
removeFavouriteDriverApi = Proxy

removeFavouriteDriver ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  m APISuccess
removeFavouriteDriver apiKey internalUrl merchantId phoneNumber countryCode driverId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (removeFavouriteDriversClient merchantId driverId (Just apiKey) (GetFavouriteDriverInfoReq phoneNumber countryCode)) "RemoveFavouriteDriver" removeFavouriteDriverApi

type CustomerCancellationDuesSyncAPI =
  "internal"
    :> Capture "merchantId" Text
    :> Capture "merchantCity" Context.City
    :> "customerCancellationDuesSync"
    :> Header "token" Text
    :> ReqBody '[JSON] CustomerCancellationDuesSyncReq
    :> Post '[JSON] APISuccess

data CustomerCancellationDuesSyncReq = CustomerCancellationDuesSyncReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text,
    cancellationCharges :: Maybe HighPrecMoney,
    cancellationChargesWithCurrency :: Maybe PriceAPIEntity,
    disputeChancesUsed :: Maybe Int,
    paymentMadeToDriver :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

customerCancellationDuesSyncClient :: Text -> Context.City -> Maybe Text -> CustomerCancellationDuesSyncReq -> EulerClient APISuccess
customerCancellationDuesSyncClient = client customerCancellationDuesSyncApi

customerCancellationDuesSyncApi :: Proxy CustomerCancellationDuesSyncAPI
customerCancellationDuesSyncApi = Proxy

customerCancellationDuesSync ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Maybe HighPrecMoney ->
  Maybe PriceAPIEntity ->
  Maybe Int ->
  Bool ->
  Context.City ->
  m APISuccess
customerCancellationDuesSync apiKey internalUrl merchantId phoneNumber countryCode cancellationCharges cancellationChargesWithCurrency disputeChancesUsed paymentMadeToDriver merchantCity = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (customerCancellationDuesSyncClient merchantId merchantCity (Just apiKey) (CustomerCancellationDuesSyncReq phoneNumber countryCode cancellationCharges cancellationChargesWithCurrency disputeChancesUsed paymentMadeToDriver)) "CustomerCancellationDuesSync" customerCancellationDuesSyncApi

-- DEPRECATED
type ReportACIssueAPI =
  "internal"
    :> Capture "rideId" Text
    :> "reportACIssue"
    :> Header "token" Text
    :> Post '[JSON] APISuccess

reportACIssueClient :: Text -> Maybe Text -> EulerClient APISuccess
reportACIssueClient = client reportACIssueApi

reportACIssueApi :: Proxy ReportACIssueAPI
reportACIssueApi = Proxy

reportACIssue ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  m APISuccess
reportACIssue apiKey internalUrl bppRideId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (reportACIssueClient bppRideId (Just apiKey)) "ReportACIssue" reportACIssueApi

type ReportIssueAPI =
  "internal"
    :> Capture "rideId" Text
    :> "reportIssue"
    :> MandatoryQueryParam "issueType" IssueReportType
    :> Header "token" Text
    :> Post '[JSON] APISuccess

reportIssueClient :: Text -> IssueReportType -> Maybe Text -> EulerClient APISuccess
reportIssueClient = client reportIssueApi

reportIssueApi :: Proxy ReportIssueAPI
reportIssueApi = Proxy

reportIssue ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  IssueReportType ->
  m APISuccess
reportIssue apiKey internalUrl bppRideId issueReportType = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (reportIssueClient bppRideId issueReportType (Just apiKey)) "ReportACIssue" reportACIssueApi

data DriverProfileRes = DriverProfileRes
  { certificates :: [Text],
    driverName :: Text,
    likedByRidersNum :: Int,
    trips :: Int,
    approvalRate :: Maybe Centesimal,
    cancellation :: Int,
    onboardedAt :: UTCTime,
    pledges :: [Text],
    expertAt :: [Text],
    whyNY :: [Text],
    languages :: [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type GetgetknowYourDriverDetailsAPI =
  "internal"
    :> Capture "rideId" Text
    :> "knowYourDriver"
    :> Header "token" Text
    :> Get '[JSON] DriverProfileRes

getknowYourDriverClient :: Text -> Maybe Text -> EulerClient DriverProfileRes
getknowYourDriverClient = client getKnowYourDriverApi

getKnowYourDriverApi :: Proxy GetgetknowYourDriverDetailsAPI
getKnowYourDriverApi = Proxy

getknowYourDriverDetails ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  m DriverProfileRes
getknowYourDriverDetails apiKey internalUrl bppRideId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getknowYourDriverClient bppRideId (Just apiKey)) "KnowYourDriver" getKnowYourDriverApi

type GetKnowYourFavDriverDetailsAPI =
  "internal"
    :> Capture "driverId" Text
    :> "knowYourFavDriver"
    :> Header "token" Text
    :> Get '[JSON] DriverProfileRes

getKnowYourDriverClient :: Text -> Maybe Text -> EulerClient DriverProfileRes
getKnowYourDriverClient = client getKnowYourFavDriverApi

getKnowYourFavDriverApi :: Proxy GetKnowYourFavDriverDetailsAPI
getKnowYourFavDriverApi = Proxy

getKnowYourFavDriverDetails ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  m DriverProfileRes
getKnowYourFavDriverDetails apiKey internalUrl bppRideId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getKnowYourDriverClient bppRideId (Just apiKey)) "KnowYourFavDriver" getKnowYourFavDriverApi

-- newtype VehicleServiceResp = VehicleServiceResp
--   { allServices = [VehicleService]
--   }
--   deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- data VehicleService = VehicleService
--   { serviceTierName :: Text,
--     vehicleImageUrl :: Text,
--     selectByDefault :: Bool
--   } deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- type VehicleServiceAPI =
--     :> Capture "merchantId" Text
--     :> Capture "merchantCity" Context.City
--     :> "serviceTierWithAssets"
--     :> Header "token" Text
--     :> Post '[JSON] VehicleServiceResp

-- vehicleServiceApi :: Proxy VehicleServiceAPI
-- vehicleServiceApi = Proxy

-- vehicleServiceClient :: Text -> Context.City -> EulerClient VehicleServiceResp
-- vehicleServiceClient = client vehicleServiceApi

-- serviceTierWithAssets ::
--   (MonadFlow m,
--   CacheFlow m r,
--   EsqDBFlow m r
--   ) =>
--   Text ->
--   BaseUrl ->
--   Text ->
--   Context.City ->
--   m VehicleServiceResp
-- serviceTierWithAssets apiKey internalUrl merchantId merchantCity = do
--   internalEndPointHashMap <- asks (.internalEndPointHashMap)
--   EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (vehicleServiceClient merchantId merchantCity (Just apiKey)) "serviceTierWithAssets" vehicleServiceApi
