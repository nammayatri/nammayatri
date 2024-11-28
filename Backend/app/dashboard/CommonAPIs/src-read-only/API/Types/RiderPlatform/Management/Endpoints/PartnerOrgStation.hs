{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.PartnerOrgStation where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data CreatePartnerOrgStationReq = CreatePartnerOrgStationReq {partnerOrgStationID :: Kernel.Prelude.Text, partnerOrgID :: Kernel.Prelude.Text, stationID :: Kernel.Prelude.Text, stationName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreatePartnerOrgStationReq where
  hideSecrets = Kernel.Prelude.identity

data UpdatePartnerOrgStationReq = UpdatePartnerOrgStationReq {oldPartnerOrgStationID :: Kernel.Prelude.Text, newPartnerOrgStationID :: Kernel.Prelude.Text, partnerOrgID :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdatePartnerOrgStationReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("partnerOrgStation" :> (PostPartnerOrgStationCreatePartnerOrgStation :<|> PostPartnerOrgStationUpdatePartnerOrgStation))

type PostPartnerOrgStationCreatePartnerOrgStation = ("createPartnerOrgStation" :> ReqBody '[JSON] CreatePartnerOrgStationReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPartnerOrgStationUpdatePartnerOrgStation = ("updatePartnerOrgStation" :> ReqBody '[JSON] UpdatePartnerOrgStationReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data PartnerOrgStationAPIs = PartnerOrgStationAPIs
  { postPartnerOrgStationCreatePartnerOrgStation :: CreatePartnerOrgStationReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPartnerOrgStationUpdatePartnerOrgStation :: UpdatePartnerOrgStationReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkPartnerOrgStationAPIs :: (Client EulerHS.Types.EulerClient API -> PartnerOrgStationAPIs)
mkPartnerOrgStationAPIs partnerOrgStationClient = (PartnerOrgStationAPIs {..})
  where
    postPartnerOrgStationCreatePartnerOrgStation :<|> postPartnerOrgStationUpdatePartnerOrgStation = partnerOrgStationClient

data PartnerOrgStationUserActionType
  = POST_PARTNER_ORG_STATION_CREATE_PARTNER_ORG_STATION
  | POST_PARTNER_ORG_STATION_UPDATE_PARTNER_ORG_STATION
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PartnerOrgStationUserActionType])
