{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.UpdatePartnerOrgStationID where

import qualified Data.Aeson
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

data ReqData = ReqData {oldPartnerOrgStationID :: Kernel.Prelude.Text, newPartnerOrgStationID :: Kernel.Prelude.Text, partnerOrgID :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ReqData where
  hideSecrets = Kernel.Prelude.identity

type API = ("updatePartnerOrgStationID" :> PostUpdatePartnerOrgStationIDUpdatePartnerOrgStationID)

type PostUpdatePartnerOrgStationIDUpdatePartnerOrgStationID = ("updatePartnerOrgStationID" :> ReqBody '[JSON] ReqData :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

newtype UpdatePartnerOrgStationIDAPIs = UpdatePartnerOrgStationIDAPIs {postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID :: ReqData -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess}

mkUpdatePartnerOrgStationIDAPIs :: (Client EulerHS.Types.EulerClient API -> UpdatePartnerOrgStationIDAPIs)
mkUpdatePartnerOrgStationIDAPIs updatePartnerOrgStationIDClient = (UpdatePartnerOrgStationIDAPIs {..})
  where
    postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID = updatePartnerOrgStationIDClient

data UpdatePartnerOrgStationIDUserActionType
  = POST_UPDATE_PARTNER_ORG_STATION_ID_UPDATE_PARTNER_ORG_STATION_ID
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON UpdatePartnerOrgStationIDUserActionType where
  toJSON POST_UPDATE_PARTNER_ORG_STATION_ID_UPDATE_PARTNER_ORG_STATION_ID = Data.Aeson.String "POST_UPDATE_PARTNER_ORG_STATION_ID_UPDATE_PARTNER_ORG_STATION_ID"

instance FromJSON UpdatePartnerOrgStationIDUserActionType where
  parseJSON (Data.Aeson.String "POST_UPDATE_PARTNER_ORG_STATION_ID_UPDATE_PARTNER_ORG_STATION_ID") = pure POST_UPDATE_PARTNER_ORG_STATION_ID_UPDATE_PARTNER_ORG_STATION_ID
  parseJSON _ = fail "POST_UPDATE_PARTNER_ORG_STATION_ID_UPDATE_PARTNER_ORG_STATION_ID expected"

$(Data.Singletons.TH.genSingletons [''UpdatePartnerOrgStationIDUserActionType])
