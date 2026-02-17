{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.DriverWallet where

import qualified "this" API.Types.UI.DriverWallet
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Plan
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Servant.Client

type API = ("driverWallet" :> (GetDriverWalletWalletTransactions :<|> PostDriverWalletWalletPayout :<|> PostDriverWalletWalletTopup :<|> GetDriverWalletWalletPayoutHistory))

type GetDriverWalletWalletTransactions =
  ( "wallet" :> Capture "driverId" (Kernel.Types.Id.Id Domain.Types.Person.Driver) :> "transactions"
      :> QueryParam
           "fromDate"
           Kernel.Prelude.UTCTime
      :> QueryParam "toDate" Kernel.Prelude.UTCTime
      :> Get ('[JSON]) API.Types.UI.DriverWallet.WalletSummaryResponse
  )

type PostDriverWalletWalletPayout = ("wallet" :> Capture "driverId" (Kernel.Types.Id.Id Domain.Types.Person.Driver) :> "payout" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostDriverWalletWalletTopup =
  ( "wallet" :> Capture "driverId" (Kernel.Types.Id.Id Domain.Types.Person.Driver) :> "topup"
      :> ReqBody
           ('[JSON])
           API.Types.UI.DriverWallet.TopUpRequest
      :> Post ('[JSON]) Domain.Action.UI.Plan.PlanSubscribeRes
  )

type GetDriverWalletWalletPayoutHistory =
  ( "wallet" :> Capture "driverId" (Kernel.Types.Id.Id Domain.Types.Person.Driver) :> "payoutHistory"
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> QueryParam
           "status"
           [Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus]
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           ('[JSON])
           API.Types.UI.DriverWallet.PayoutHistoryResponse
  )

data DriverWalletAPIs = DriverWalletAPIs
  { getDriverWalletWalletTransactions :: (Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient API.Types.UI.DriverWallet.WalletSummaryResponse),
    postDriverWalletWalletPayout :: (Kernel.Types.Id.Id Domain.Types.Person.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postDriverWalletWalletTopup :: (Kernel.Types.Id.Id Domain.Types.Person.Driver -> API.Types.UI.DriverWallet.TopUpRequest -> EulerHS.Types.EulerClient Domain.Action.UI.Plan.PlanSubscribeRes),
    getDriverWalletWalletPayoutHistory :: (Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe ([Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus]) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient API.Types.UI.DriverWallet.PayoutHistoryResponse)
  }

mkDriverWalletAPIs :: (Client EulerHS.Types.EulerClient API -> DriverWalletAPIs)
mkDriverWalletAPIs driverWalletClient = (DriverWalletAPIs {..})
  where
    getDriverWalletWalletTransactions :<|> postDriverWalletWalletPayout :<|> postDriverWalletWalletTopup :<|> getDriverWalletWalletPayoutHistory = driverWalletClient

data DriverWalletUserActionType
  = GET_DRIVER_WALLET_WALLET_TRANSACTIONS
  | POST_DRIVER_WALLET_WALLET_PAYOUT
  | POST_DRIVER_WALLET_WALLET_TOPUP
  | GET_DRIVER_WALLET_WALLET_PAYOUT_HISTORY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''DriverWalletUserActionType)])
