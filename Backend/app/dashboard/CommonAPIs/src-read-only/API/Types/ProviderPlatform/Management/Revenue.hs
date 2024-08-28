{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Revenue where

import qualified Dashboard.ProviderPlatform.RideBooking.Driver
import Data.OpenApi (ToSchema)
import qualified Data.Time
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import Servant
import Servant.Client

data AllFees = AllFees
  { status :: Dashboard.ProviderPlatform.RideBooking.Driver.DriverFeeStatus,
    numRides :: Kernel.Prelude.Int,
    numDrivers :: Kernel.Prelude.Int,
    totalAmount :: Kernel.Prelude.Int,
    specialZoneAmount :: Kernel.Prelude.Int,
    openMarketAmount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CollectionList = CollectionList {onlineCollection :: [API.Types.ProviderPlatform.Management.Revenue.CollectionListElem], offlineCollection :: [API.Types.ProviderPlatform.Management.Revenue.CollectionListElem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CollectionListElem = CollectionListElem
  { totalAmount :: Kernel.Prelude.Int,
    specialZoneAmount :: Kernel.Prelude.Int,
    openMarketAmount :: Kernel.Prelude.Int,
    totalRides :: Kernel.Prelude.Int,
    numDrivers :: Kernel.Prelude.Int,
    date :: Data.Time.Day,
    hour :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("revenue" :> (GetRevenueCollectionHistory :<|> GetRevenueAllFeeHistory))

type GetRevenueCollectionHistory =
  ( "collectionHistory" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "place" Kernel.Prelude.Text
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam "volunteerId" Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Revenue.CollectionList
  )

type GetRevenueAllFeeHistory =
  ( "allFeeHistory" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           [API.Types.ProviderPlatform.Management.Revenue.AllFees]
  )

data RevenueAPIs = RevenueAPIs
  { getRevenueCollectionHistory :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Revenue.CollectionList,
    getRevenueAllFeeHistory :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient [API.Types.ProviderPlatform.Management.Revenue.AllFees]
  }

mkRevenueAPIs :: (Client EulerHS.Types.EulerClient API -> RevenueAPIs)
mkRevenueAPIs revenueClient = (RevenueAPIs {..})
  where
    getRevenueCollectionHistory :<|> getRevenueAllFeeHistory = revenueClient

data RevenueEndpointDSL
  = GetRevenueCollectionHistoryEndpoint
  | GetRevenueAllFeeHistoryEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
