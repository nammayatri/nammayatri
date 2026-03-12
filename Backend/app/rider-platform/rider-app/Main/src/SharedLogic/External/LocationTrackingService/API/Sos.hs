module SharedLogic.External.LocationTrackingService.API.Sos where

import Data.Aeson
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Types.APISuccess
import Servant
import SharedLogic.External.LocationTrackingService.Types

-- | Entity upsert for SOS — registers a person→SOS mapping in LTS,
-- optionally with broadcaster (ERSS) config bundled in.
-- Path: POST /internal/entity/rider/sos/{sosId}/upsert
data EntityInfo = EntityStart
  deriving (Generic, Show)

instance ToJSON EntityInfo where
  toJSON EntityStart = object ["entityInfo" .= ("entityStart" :: Text)]

instance FromJSON EntityInfo where
  parseJSON = withObject "EntityInfo" $ \o -> do
    tag <- o .: "entityInfo"
    case (tag :: Text) of
      "entityStart" -> pure EntityStart
      _ -> fail "Unknown entityInfo tag"

data EntityUpsertReq = EntityUpsertReq
  { personId :: Text,
    merchantId :: Text,
    entityInfo :: EntityInfo,
    broadcasterConfig :: Maybe SosErssConfigReq
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type EntityUpsertAPI =
  "internal"
    :> "entity"
    :> "rider"
    :> "sos"
    :> Capture "sosId" Text
    :> "upsert"
    :> ReqBody '[JSON] EntityUpsertReq
    :> Post '[JSON] APISuccess

entityUpsertAPI :: Proxy EntityUpsertAPI
entityUpsertAPI = Proxy

entityUpsertForSos :: Text -> EntityUpsertReq -> ET.EulerClient APISuccess
entityUpsertForSos = ET.client entityUpsertAPI
