{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.InsertPartnerOrgStation where

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

data InsertData = InsertData {partnerOrgStationID :: Kernel.Prelude.Text, partnerOrgID :: Kernel.Prelude.Text, stationID :: Kernel.Prelude.Text, stationName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets InsertData where
  hideSecrets = Kernel.Prelude.identity

type API = ("insertPartnerOrgStation" :> PostInsertPartnerOrgStationInsertPartnerOrgStation)

type PostInsertPartnerOrgStationInsertPartnerOrgStation = ("insertPartnerOrgStation" :> ReqBody '[JSON] InsertData :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

newtype InsertPartnerOrgStationAPIs = InsertPartnerOrgStationAPIs {postInsertPartnerOrgStationInsertPartnerOrgStation :: InsertData -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess}

mkInsertPartnerOrgStationAPIs :: (Client EulerHS.Types.EulerClient API -> InsertPartnerOrgStationAPIs)
mkInsertPartnerOrgStationAPIs insertPartnerOrgStationClient = (InsertPartnerOrgStationAPIs {..})
  where
    postInsertPartnerOrgStationInsertPartnerOrgStation = insertPartnerOrgStationClient

data InsertPartnerOrgStationUserActionType
  = POST_INSERT_PARTNER_ORG_STATION_INSERT_PARTNER_ORG_STATION
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON InsertPartnerOrgStationUserActionType where
  toJSON POST_INSERT_PARTNER_ORG_STATION_INSERT_PARTNER_ORG_STATION = Data.Aeson.String "POST_INSERT_PARTNER_ORG_STATION_INSERT_PARTNER_ORG_STATION"

instance FromJSON InsertPartnerOrgStationUserActionType where
  parseJSON (Data.Aeson.String "POST_INSERT_PARTNER_ORG_STATION_INSERT_PARTNER_ORG_STATION") = pure POST_INSERT_PARTNER_ORG_STATION_INSERT_PARTNER_ORG_STATION
  parseJSON _ = fail "POST_INSERT_PARTNER_ORG_STATION_INSERT_PARTNER_ORG_STATION expected"

$(Data.Singletons.TH.genSingletons [''InsertPartnerOrgStationUserActionType])
