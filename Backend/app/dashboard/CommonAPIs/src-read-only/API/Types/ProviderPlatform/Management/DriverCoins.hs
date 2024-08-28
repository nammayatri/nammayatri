{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.DriverCoins where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data BulkUploadCoinsReq = BulkUploadCoinsReq
  { driverIdListWithCoins :: [API.Types.ProviderPlatform.Management.DriverCoins.DriverIdListWithCoins],
    bulkUploadTitle :: API.Types.ProviderPlatform.Management.DriverCoins.Translations,
    expirationTime :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BulkUploadCoinsReqV2 = BulkUploadCoinsReqV2
  { driverIdListWithCoins :: [API.Types.ProviderPlatform.Management.DriverCoins.DriverIdListWithAmount],
    bulkUploadTitle :: API.Types.ProviderPlatform.Management.DriverCoins.Translations,
    expirationTime :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    eventFunction :: API.Types.ProviderPlatform.Management.DriverCoins.DriverCoinsFunctionType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CoinBurnHistoryItem = CoinBurnHistoryItem
  { numCoins :: Kernel.Prelude.Int,
    cash :: Kernel.Types.Common.HighPrecMoney,
    cashWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    title :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CoinEarnHistoryItem = CoinEarnHistoryItem
  { coins :: Kernel.Prelude.Int,
    eventFunction :: API.Types.ProviderPlatform.Management.DriverCoins.DriverCoinsFunctionType,
    createdAt :: Kernel.Prelude.UTCTime,
    expirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    coinsUsed :: Kernel.Prelude.Int,
    status :: API.Types.ProviderPlatform.Management.DriverCoins.CoinStatus,
    bulkUploadTitle :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.DriverCoins.Translations
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CoinHistoryRes = CoinHistoryRes
  { coinBalance :: Kernel.Prelude.Int,
    coinEarned :: Kernel.Prelude.Int,
    coinUsed :: Kernel.Prelude.Int,
    coinExpired :: Kernel.Prelude.Int,
    coinEarnHistory :: [API.Types.ProviderPlatform.Management.DriverCoins.CoinEarnHistoryItem],
    coinBurnHistory :: [API.Types.ProviderPlatform.Management.DriverCoins.CoinBurnHistoryItem]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CoinMessage
  = CoinAdded
  | CoinSubtracted
  | FareRecomputation
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CoinStatus
  = Used
  | Remaining
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverCoinsFunctionType
  = OneOrTwoStarRating
  | RideCompleted
  | FiveStarRating
  | BookingCancellation
  | CustomerReferral
  | DriverReferral
  | TwoRidesCompleted
  | FiveRidesCompleted
  | TenRidesCompleted
  | EightPlusRidesInOneDay
  | PurpleRideCompleted
  | LeaderBoardTopFiveHundred
  | TrainingCompleted
  | BulkUploadFunction
  | BulkUploadFunctionV2 API.Types.ProviderPlatform.Management.DriverCoins.CoinMessage
  deriving stock (Generic, Show, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverIdListWithAmount = DriverIdListWithAmount {driverId :: Kernel.Prelude.Text, amount :: Kernel.Types.Common.HighPrecMoney, amountWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverIdListWithCoins = DriverIdListWithCoins {driverId :: Kernel.Prelude.Text, coins :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Translations = Translations
  { en :: Kernel.Prelude.Text,
    bn :: Kernel.Prelude.Text,
    hi :: Kernel.Prelude.Text,
    ml :: Kernel.Prelude.Text,
    ta :: Kernel.Prelude.Text,
    te :: Kernel.Prelude.Text,
    kn :: Kernel.Prelude.Text,
    fr :: Kernel.Prelude.Text
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("coins" :> (PostDriverCoinsBulkUploadCoins :<|> PostDriverCoinsBulkUploadCoinsV2 :<|> GetDriverCoinsCoinHistory))

type PostDriverCoinsBulkUploadCoins = ("bulkUploadCoins" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinsReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverCoinsBulkUploadCoinsV2 =
  ( "bulkUploadCoinsV2" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinsReqV2
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverCoinsCoinHistory =
  ( "coinHistory" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> QueryParam "limit" Kernel.Prelude.Integer
      :> QueryParam
           "offset"
           Kernel.Prelude.Integer
      :> Get '[JSON] API.Types.ProviderPlatform.Management.DriverCoins.CoinHistoryRes
  )

data DriverCoinsAPIs = DriverCoinsAPIs
  { postDriverCoinsBulkUploadCoins :: API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverCoinsBulkUploadCoinsV2 :: API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinsReqV2 -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverCoinsCoinHistory :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.DriverCoins.CoinHistoryRes
  }

mkDriverCoinsAPIs :: (Client EulerHS.Types.EulerClient API -> DriverCoinsAPIs)
mkDriverCoinsAPIs driverCoinsClient = (DriverCoinsAPIs {..})
  where
    postDriverCoinsBulkUploadCoins :<|> postDriverCoinsBulkUploadCoinsV2 :<|> getDriverCoinsCoinHistory = driverCoinsClient

data DriverCoinsEndpointDSL
  = PostDriverCoinsBulkUploadCoinsEndpoint
  | PostDriverCoinsBulkUploadCoinsV2Endpoint
  | GetDriverCoinsCoinHistoryEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
