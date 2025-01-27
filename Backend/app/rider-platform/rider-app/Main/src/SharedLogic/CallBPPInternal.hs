{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallBPPInternal where

import API.Types.UI.FavouriteDriver
import qualified Data.HashMap.Strict as HM
import Data.Text as T
import Domain.Types.Common
import Domain.Types.FeedbackForm
import Domain.Types.Merchant
import qualified Domain.Types.RefereeLink as LibTypes
import EulerHS.Types (EulerClient, client)
import IssueManagement.Common (IssueReportType)
import Kernel.External.Maps.Types
import qualified Kernel.External.Maps.Types as Maps
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
    isMultipleDeviceIdExist :: Maybe Bool,
    alreadyReferred :: Maybe Bool,
    shareReferrerInfo :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type LinkRefereeAPI =
  "internal"
    :> Capture "merchantId" Text
    :> "referee"
    :> Header "token" Text
    :> ReqBody '[JSON] RefereeLinkInfoReq
    :> Post '[JSON] LibTypes.LinkRefereeRes

linkRefereeClient :: Text -> Maybe Text -> RefereeLinkInfoReq -> EulerClient LibTypes.LinkRefereeRes
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
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  m LibTypes.LinkRefereeRes
linkReferee apiKey internalUrl merchantId referralCode phoneNumber countryCode isMultipleDeviceIdExist alreadyReferred shareReferrerInfo = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (linkRefereeClient merchantId (Just apiKey) buildLinkRefereeReq) "LinkReferee" likeRefereeApi
  where
    buildLinkRefereeReq = do
      RefereeLinkInfoReq
        { customerMobileNumber = phoneNumber,
          customerMobileCountryCode = countryCode,
          ..
        }

type CallCustomerFCMAPI =
  "internal"
    :> Capture "rideId" Text
    :> "callCustomerFCM"
    :> Header "token" Text
    :> Post '[JSON] APISuccess

callCustomerFCMClient :: Text -> Maybe Text -> EulerClient APISuccess
callCustomerFCMClient = client callCustomerFCMApi

callCustomerFCMApi :: Proxy CallCustomerFCMAPI
callCustomerFCMApi = Proxy

callCustomerFCM ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  m APISuccess
callCustomerFCM apiKey internalUrl bppRideId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (callCustomerFCMClient bppRideId (Just apiKey)) "CallCustomerFCM" callCustomerFCMApi

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
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (reportIssueClient bppRideId issueReportType (Just apiKey)) "ReportIssue" reportIssueApi

data DriverReview = DriverReview
  { riderName :: Maybe Text,
    review :: Maybe Text,
    feedBackPills :: [Text],
    rating :: Int,
    tripDate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DriverStatSummary = DriverStatSummary
  { avgRating :: Maybe Centesimal,
    numTrips :: Int,
    cancellationRate :: Int,
    likedByRidersNum :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DriverProfileRes = DriverProfileRes
  { certificates :: [Text],
    homeTown :: Maybe Text,
    driverName :: Text,
    aboutMe :: Maybe Text,
    drivingSince :: Maybe Int,
    onboardedAt :: UTCTime,
    pledges :: [Text],
    driverStats :: DriverStatSummary,
    languages :: [Text],
    aspirations :: [Text],
    vehicleNum :: Maybe Text,
    vechicleVariant :: Maybe Text,
    vehicleTags :: [Text],
    images :: [Text],
    profileImage :: Maybe Text,
    topReviews :: [DriverReview]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type GetgetknowYourDriverDetailsAPI =
  "internal"
    :> Capture "rideId" Text
    :> "knowYourDriver"
    :> QueryParam "isImages" Bool
    :> Header "token" Text
    :> Get '[JSON] DriverProfileRes

getknowYourDriverClient :: Text -> Maybe Bool -> Maybe Text -> EulerClient DriverProfileRes
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
  Maybe Bool ->
  m DriverProfileRes
getknowYourDriverDetails apiKey internalUrl bppRideId withImages = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getknowYourDriverClient bppRideId withImages (Just apiKey)) "KnowYourDriver" getKnowYourDriverApi

type GetKnowYourFavDriverDetailsAPI =
  "internal"
    :> Capture "driverId" Text
    :> "knowYourFavDriver"
    :> QueryParam "isImages" Bool
    :> Header "token" Text
    :> Get '[JSON] DriverProfileRes

getKnowYourDriverClient :: Text -> Maybe Bool -> Maybe Text -> EulerClient DriverProfileRes
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
  Maybe Bool ->
  m DriverProfileRes
getKnowYourFavDriverDetails apiKey internalUrl bppRideId withImages = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getKnowYourDriverClient bppRideId withImages (Just apiKey)) "KnowYourFavDriver" getKnowYourFavDriverApi

type GetDriverCoordinatesDetailsAPI =
  "internal"
    :> Capture "rideId" Text
    :> "getDriverCoordinates"
    :> Header "token" Text
    :> Get '[JSON] (Maybe Maps.LatLong)

getDriverCoordinatesClient :: Text -> Maybe Text -> EulerClient (Maybe Maps.LatLong)
getDriverCoordinatesClient = client getDriverCoordinatesApi

getDriverCoordinatesApi :: Proxy GetDriverCoordinatesDetailsAPI
getDriverCoordinatesApi = Proxy

getDriverCoordinatesDetails ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  m (Maybe Maps.LatLong)
getDriverCoordinatesDetails apiKey internalUrl bppRideId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getDriverCoordinatesClient bppRideId (Just apiKey)) "GetDriverCoordinates" getDriverCoordinatesApi

type PopulateTipAmountAPI =
  "internal"
    :> Capture "rideId" Text
    :> Capture "tipAmount" HighPrecMoney
    :> "populateTipAmount"
    :> Header "token" Text
    :> Post '[JSON] APISuccess

populateTipAmountClient :: Text -> HighPrecMoney -> Maybe Text -> EulerClient APISuccess
populateTipAmountClient = client populateTipAmountApi

populateTipAmountApi :: Proxy PopulateTipAmountAPI
populateTipAmountApi = Proxy

populateTipAmount ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  HighPrecMoney ->
  m APISuccess
populateTipAmount apiKey internalUrl bppRideId tipAmount = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (populateTipAmountClient bppRideId tipAmount (Just apiKey)) "PopulateTipAmount" populateTipAmountApi

type GetDeliveryImageAPI =
  "internal"
    :> "ride"
    :> Capture "rideId" Text
    :> "deliveryImage"
    :> Header "token" Text
    :> Get '[JSON] Text

getDeliveryImageApi :: Proxy GetDeliveryImageAPI
getDeliveryImageApi = Proxy

getDeliveryImageClient :: Text -> Maybe Text -> EulerClient Text
getDeliveryImageClient = client getDeliveryImageApi

getDeliveryImage ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  m Text
getDeliveryImage apiKey internalUrl bppRideId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getDeliveryImageClient bppRideId (Just apiKey)) "GetDeliveryImage" getDeliveryImageApi

data CalculateFareReq = CalculateFareReq
  { dropLatLong :: Kernel.External.Maps.Types.LatLong,
    pickupLatLong :: Kernel.External.Maps.Types.LatLong,
    mbDistance :: Maybe Meters,
    mbDuration :: Maybe Seconds
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FareData = FareData
  { estimatedDistance :: Maybe Meters,
    maxFare :: HighPrecMoney,
    minFare :: HighPrecMoney,
    tollNames :: Maybe [Text],
    tripCategory :: TripCategory,
    vehicleServiceTier :: ServiceTierType,
    vehicleServiceTierName :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetFareResponse = FareResponse {estimatedFares :: [FareData]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type GetFareAPI =
  "internal"
    :> Capture "merchantId" Text
    :> Capture "merchantCity" Context.City
    :> "calculateFare"
    :> Header "token" Text
    :> ReqBody '[JSON] CalculateFareReq
    :> Post '[JSON] GetFareResponse

getFareClient :: Text -> Context.City -> Maybe Text -> CalculateFareReq -> EulerClient GetFareResponse
getFareClient = client getFareApi

getFareApi :: Proxy GetFareAPI
getFareApi = Proxy

getFare ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Merchant ->
  Context.City ->
  CalculateFareReq ->
  m GetFareResponse
getFare merchant city req = do
  let apiKey = merchant.driverOfferApiKey
  let internalUrl = merchant.driverOfferBaseUrl
  let merchantId = merchant.driverOfferMerchantId
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getFareClient merchantId city (Just apiKey) req) "GetFare" getFareApi

data IsIntercityReq = IsIntercityReq
  { pickupLatLong :: LatLong,
    mbDropLatLong :: Maybe LatLong
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data IsIntercityResp = IsIntercityResp
  { isInterCity :: Bool,
    isCrossCity :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

type GetIsInterCityAPI =
  "internal"
    :> Capture "merchantId" Text
    :> "isInterCity"
    :> Header "token" Text
    :> ReqBody '[JSON] IsIntercityReq
    :> Post '[JSON] IsIntercityResp

getIsInterCityClient :: Text -> Maybe Text -> IsIntercityReq -> EulerClient IsIntercityResp
getIsInterCityClient = client getIsInterCityApi

getIsInterCityApi :: Proxy GetIsInterCityAPI
getIsInterCityApi = Proxy

getIsInterCity ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Merchant ->
  IsIntercityReq ->
  m IsIntercityResp
getIsInterCity merchant req = do
  let apiKey = merchant.driverOfferApiKey
  let internalUrl = merchant.driverOfferBaseUrl
  let merchantId = merchant.driverOfferMerchantId
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (getIsInterCityClient merchantId (Just apiKey) req) "GetIsInterCity" getIsInterCityApi
