{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Revenue where

import qualified Dashboard.ProviderPlatform.Driver
import Data.OpenApi (ToSchema)
import qualified Data.Time
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Servant
import Servant.Client

data AllFees = AllFees
  { numDrivers :: Kernel.Prelude.Int,
    numRides :: Kernel.Prelude.Int,
    openMarketAmount :: Kernel.Prelude.Int,
    specialZoneAmount :: Kernel.Prelude.Int,
    status :: Dashboard.ProviderPlatform.Driver.DriverFeeStatus,
    totalAmount :: Kernel.Prelude.Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CollectionList = CollectionList {offlineCollection :: [API.Types.ProviderPlatform.Revenue.CollectionListElem], onlineCollection :: [API.Types.ProviderPlatform.Revenue.CollectionListElem]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CollectionListElem = CollectionListElem
  { date :: Data.Time.Day,
    hour :: Kernel.Prelude.Int,
    numDrivers :: Kernel.Prelude.Int,
    openMarketAmount :: Kernel.Prelude.Int,
    specialZoneAmount :: Kernel.Prelude.Int,
    totalAmount :: Kernel.Prelude.Int,
    totalRides :: Kernel.Prelude.Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type API = ("revenue" :> (GetRevenueCollectionHistory :<|> GetRevenueAllFeeHistory))

type GetRevenueCollectionHistory =
  ( "collectionHistory" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "place" Kernel.Prelude.Text
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam "volunteerId" Kernel.Prelude.Text
      :> Get '[JSON] API.Types.ProviderPlatform.Revenue.CollectionList
  )

type GetRevenueAllFeeHistory = ("allFeeHistory" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "to" Kernel.Prelude.UTCTime :> Get '[JSON] [API.Types.ProviderPlatform.Revenue.AllFees])

data RevenueAPIs = RevenueAPIs
  { getRevenueCollectionHistory :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Revenue.CollectionList,
    getRevenueAllFeeHistory :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient [API.Types.ProviderPlatform.Revenue.AllFees]
  }

mkRevenueAPIs :: (Client EulerHS.Types.EulerClient API -> RevenueAPIs)
mkRevenueAPIs revenueClient = (RevenueAPIs {..})
  where
    getRevenueCollectionHistory :<|> getRevenueAllFeeHistory = revenueClient
