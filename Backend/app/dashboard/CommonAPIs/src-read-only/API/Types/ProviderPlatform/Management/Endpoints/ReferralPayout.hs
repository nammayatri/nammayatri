{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.ReferralPayout where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.External.Payout.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Servant.Client

data ReferralPayoutRegistrationRes = ReferralPayoutRegistrationRes {orderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder, orderResp :: Kernel.External.Payment.Interface.Types.CreateOrderResp}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("referralPayout" :> (PostReferralPayoutDashboardPayoutDeleteVpa :<|> GetReferralPayoutDashboardPayoutRegistration :<|> GetReferralPayoutDashboardPayoutOrderStatus))

type PostReferralPayoutDashboardPayoutDeleteVpa =
  ( "dashboardPayout" :> "delete" :> "vpa" :> QueryParam "driverId" (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver))
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetReferralPayoutDashboardPayoutRegistration =
  ( "dashboardPayout" :> "registration" :> QueryParam "driverId" (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver))
      :> Get
           ('[JSON])
           ReferralPayoutRegistrationRes
  )

type GetReferralPayoutDashboardPayoutOrderStatus =
  ( "dashboardPayout" :> "order" :> "status"
      :> QueryParam
           "driverId"
           (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver))
      :> MandatoryQueryParam "orderId" Data.Text.Text
      :> Get
           ('[JSON])
           Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp
  )

data ReferralPayoutAPIs = ReferralPayoutAPIs
  { postReferralPayoutDashboardPayoutDeleteVpa :: (Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getReferralPayoutDashboardPayoutRegistration :: (Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> EulerHS.Types.EulerClient ReferralPayoutRegistrationRes),
    getReferralPayoutDashboardPayoutOrderStatus :: (Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Data.Text.Text -> EulerHS.Types.EulerClient Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp)
  }

mkReferralPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> ReferralPayoutAPIs)
mkReferralPayoutAPIs referralPayoutClient = (ReferralPayoutAPIs {..})
  where
    postReferralPayoutDashboardPayoutDeleteVpa :<|> getReferralPayoutDashboardPayoutRegistration :<|> getReferralPayoutDashboardPayoutOrderStatus = referralPayoutClient

data ReferralPayoutUserActionType
  = POST_REFERRAL_PAYOUT_DASHBOARD_PAYOUT_DELETE_VPA
  | GET_REFERRAL_PAYOUT_DASHBOARD_PAYOUT_REGISTRATION
  | GET_REFERRAL_PAYOUT_DASHBOARD_PAYOUT_ORDER_STATUS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''ReferralPayoutUserActionType)])
