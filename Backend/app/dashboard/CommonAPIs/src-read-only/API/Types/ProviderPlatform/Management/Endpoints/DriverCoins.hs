{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.DriverCoins where

import qualified Dashboard.Common
import qualified Dashboard.Common.DriverCoins
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data BulkUploadCoinRes = BulkUploadCoinRes {success :: Kernel.Prelude.Int, failed :: Kernel.Prelude.Int, failedItems :: [BulkUploadFailedItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BulkUploadCoinsReq = BulkUploadCoinsReq {driverIdListWithCoins :: [DriverIdListWithCoins], bulkUploadTitle :: Translations, expirationTime :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BulkUploadCoinsReqV2 = BulkUploadCoinsReqV2
  { driverIdListWithCoins :: [DriverIdListWithAmount],
    bulkUploadTitle :: Translations,
    expirationTime :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    eventFunction :: Dashboard.Common.DriverCoins.DriverCoinsFunctionType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BulkUploadFailedItem = BulkUploadFailedItem {driverId :: Kernel.Prelude.Text, errorMessage :: Kernel.Prelude.Text}
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
    eventFunction :: Dashboard.Common.DriverCoins.DriverCoinsFunctionType,
    createdAt :: Kernel.Prelude.UTCTime,
    expirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    coinsUsed :: Kernel.Prelude.Int,
    status :: CoinStatus,
    bulkUploadTitle :: Kernel.Prelude.Maybe Translations,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CoinHistoryRes = CoinHistoryRes
  { coinBalance :: Kernel.Prelude.Int,
    coinEarned :: Kernel.Prelude.Int,
    coinUsed :: Kernel.Prelude.Int,
    coinExpired :: Kernel.Prelude.Int,
    coinEarnHistory :: [CoinEarnHistoryItem],
    coinBurnHistory :: [CoinBurnHistoryItem]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CoinStatus
  = Used
  | Remaining
  deriving stock (Eq, Show, Generic, Read, Ord)
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

data UpdateBlacklistedCoinEventsReq = UpdateBlacklistedCoinEventsReq {blacklistedEvents :: [Dashboard.Common.DriverCoins.DriverCoinsFunctionType]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("coins" :> (PostDriverCoinsBulkUploadCoins :<|> PostDriverCoinsBulkUploadCoinsV2 :<|> GetDriverCoinsCoinHistory :<|> PostDriverCoinsBlacklistedEventsUpdate))

type PostDriverCoinsBulkUploadCoins = ("bulkUploadCoins" :> ReqBody '[JSON] BulkUploadCoinsReq :> Post '[JSON] BulkUploadCoinRes)

type PostDriverCoinsBulkUploadCoinsV2 = ("bulkUploadCoinsV2" :> ReqBody '[JSON] BulkUploadCoinsReqV2 :> Post '[JSON] BulkUploadCoinRes)

type GetDriverCoinsCoinHistory =
  ( "coinHistory" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> QueryParam "limit" Kernel.Prelude.Integer
      :> QueryParam
           "offset"
           Kernel.Prelude.Integer
      :> Get '[JSON] CoinHistoryRes
  )

type PostDriverCoinsBlacklistedEventsUpdate =
  ( "blacklistedEvents" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "update"
      :> ReqBody
           '[JSON]
           UpdateBlacklistedCoinEventsReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data DriverCoinsAPIs = DriverCoinsAPIs
  { postDriverCoinsBulkUploadCoins :: BulkUploadCoinsReq -> EulerHS.Types.EulerClient BulkUploadCoinRes,
    postDriverCoinsBulkUploadCoinsV2 :: BulkUploadCoinsReqV2 -> EulerHS.Types.EulerClient BulkUploadCoinRes,
    getDriverCoinsCoinHistory :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> EulerHS.Types.EulerClient CoinHistoryRes,
    postDriverCoinsBlacklistedEventsUpdate :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateBlacklistedCoinEventsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkDriverCoinsAPIs :: (Client EulerHS.Types.EulerClient API -> DriverCoinsAPIs)
mkDriverCoinsAPIs driverCoinsClient = (DriverCoinsAPIs {..})
  where
    postDriverCoinsBulkUploadCoins :<|> postDriverCoinsBulkUploadCoinsV2 :<|> getDriverCoinsCoinHistory :<|> postDriverCoinsBlacklistedEventsUpdate = driverCoinsClient

data DriverCoinsUserActionType
  = POST_DRIVER_COINS_BULK_UPLOAD_COINS
  | POST_DRIVER_COINS_BULK_UPLOAD_COINS_V2
  | GET_DRIVER_COINS_COIN_HISTORY
  | POST_DRIVER_COINS_BLACKLISTED_EVENTS_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''DriverCoinsUserActionType])
