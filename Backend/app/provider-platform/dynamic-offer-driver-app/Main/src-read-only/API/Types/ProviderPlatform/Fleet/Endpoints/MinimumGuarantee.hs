{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.MinimumGuarantee where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified Domain.Types.MgEnrollment
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data FleetMgDriverDay = FleetMgDriverDay {driverId :: Kernel.Prelude.Text, date :: Data.Time.Day, status :: Kernel.Prelude.Maybe Domain.Types.MgEnrollment.MgEnrollmentStatus, canOptIn :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetMgFailure = FleetMgFailure {driverId :: Kernel.Prelude.Text, reason :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetMgInvitation = FleetMgInvitation
  { campaignId :: Kernel.Types.Id.Id Dashboard.Common.MgCampaign,
    name :: Kernel.Prelude.Text,
    startDate :: Data.Time.Day,
    endDate :: Data.Time.Day,
    guaranteeAmount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    drivers :: [FleetMgDriverDay]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetMgInvitationsRes = FleetMgInvitationsRes {invitations :: [FleetMgInvitation]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetMgOptReq = FleetMgOptReq
  { fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    campaignId :: Kernel.Types.Id.Id Dashboard.Common.MgCampaign,
    driverIds :: [Kernel.Prelude.Text],
    date :: Data.Time.Day,
    acceptTerms :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetMgOptReq where
  hideSecrets = Kernel.Prelude.identity

data FleetMgOptRes = FleetMgOptRes {done :: [Kernel.Prelude.Text], failed :: [FleetMgFailure]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("fleet" :> (GetMinimumGuaranteeMinimumGuaranteeInvitationsHelper :<|> PostMinimumGuaranteeMinimumGuaranteeOptInHelper :<|> PostMinimumGuaranteeMinimumGuaranteeOptOutHelper))

type GetMinimumGuaranteeInvitations =
  ( "minimumGuarantee" :> "invitations" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get '[JSON] FleetMgInvitationsRes
  )

type GetMinimumGuaranteeMinimumGuaranteeInvitationsHelper =
  ( "minimumGuarantee" :> "invitations" :> Capture "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get '[JSON] FleetMgInvitationsRes
  )

type PostMinimumGuaranteeOptIn = ("minimumGuarantee" :> "optIn" :> ReqBody '[JSON] FleetMgOptReq :> Post '[JSON] FleetMgOptRes)

type PostMinimumGuaranteeMinimumGuaranteeOptInHelper = ("minimumGuarantee" :> "optIn" :> Capture "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetMgOptReq :> Post '[JSON] FleetMgOptRes)

type PostMinimumGuaranteeOptOut = ("minimumGuarantee" :> "optOut" :> ReqBody '[JSON] FleetMgOptReq :> Post '[JSON] FleetMgOptRes)

type PostMinimumGuaranteeMinimumGuaranteeOptOutHelper = ("minimumGuarantee" :> "optOut" :> Capture "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetMgOptReq :> Post '[JSON] FleetMgOptRes)

data MinimumGuaranteeAPIs = MinimumGuaranteeAPIs
  { getMinimumGuaranteeInvitations :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient FleetMgInvitationsRes,
    postMinimumGuaranteeOptIn :: Kernel.Prelude.Text -> FleetMgOptReq -> EulerHS.Types.EulerClient FleetMgOptRes,
    postMinimumGuaranteeOptOut :: Kernel.Prelude.Text -> FleetMgOptReq -> EulerHS.Types.EulerClient FleetMgOptRes
  }

mkMinimumGuaranteeAPIs :: (Client EulerHS.Types.EulerClient API -> MinimumGuaranteeAPIs)
mkMinimumGuaranteeAPIs minimumGuaranteeClient = (MinimumGuaranteeAPIs {..})
  where
    getMinimumGuaranteeInvitations :<|> postMinimumGuaranteeOptIn :<|> postMinimumGuaranteeOptOut = minimumGuaranteeClient

data MinimumGuaranteeUserActionType
  = GET_MINIMUM_GUARANTEE_INVITATIONS
  | POST_MINIMUM_GUARANTEE_OPT_IN
  | POST_MINIMUM_GUARANTEE_OPT_OUT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''MinimumGuaranteeUserActionType])
