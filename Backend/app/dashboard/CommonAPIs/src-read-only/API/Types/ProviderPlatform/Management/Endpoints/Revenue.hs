{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.ProviderPlatform.Management.Endpoints.Revenue where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Dashboard.Common.Driver
import qualified Kernel.Prelude
import qualified Data.Time
import qualified EulerHS.Types
import qualified Data.Singletons.TH



data AllFees
    = AllFees {status :: Dashboard.Common.Driver.DriverFeeStatus,
               numRides :: Kernel.Prelude.Int,
               numDrivers :: Kernel.Prelude.Int,
               totalAmount :: Kernel.Prelude.Int,
               specialZoneAmount :: Kernel.Prelude.Int,
               openMarketAmount :: Kernel.Prelude.Int}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data CollectionList
    = CollectionList {onlineCollection :: [CollectionListElem], offlineCollection :: [CollectionListElem]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data CollectionListElem
    = CollectionListElem {totalAmount :: Kernel.Prelude.Int,
                          specialZoneAmount :: Kernel.Prelude.Int,
                          openMarketAmount :: Kernel.Prelude.Int,
                          totalRides :: Kernel.Prelude.Int,
                          numDrivers :: Kernel.Prelude.Int,
                          date :: Data.Time.Day,
                          hour :: Kernel.Prelude.Int}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
type API = ("revenue" :> (GetRevenueCollectionHistory :<|> GetRevenueAllFeeHistory))
type GetRevenueCollectionHistory = ("collectionHistory" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "place" Kernel.Prelude.Text :> QueryParam "to"
                                                                                                                                                            Kernel.Prelude.UTCTime :> QueryParam "volunteerId" Kernel.Prelude.Text :> Get ('[JSON]) CollectionList)
type GetRevenueAllFeeHistory = ("allFeeHistory" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "to"
                                                                                                                                                                                             Kernel.Prelude.UTCTime :> Get ('[JSON]) [AllFees])
data RevenueAPIs
    = RevenueAPIs {getRevenueCollectionHistory :: (Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient CollectionList),
                   getRevenueAllFeeHistory :: (Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient [AllFees])}
mkRevenueAPIs :: (Client EulerHS.Types.EulerClient API -> RevenueAPIs)
mkRevenueAPIs revenueClient = (RevenueAPIs {..})
                  where getRevenueCollectionHistory :<|> getRevenueAllFeeHistory = revenueClient
data RevenueUserActionType
    = GET_REVENUE_COLLECTION_HISTORY | GET_REVENUE_ALL_FEE_HISTORY
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''RevenueUserActionType)])

