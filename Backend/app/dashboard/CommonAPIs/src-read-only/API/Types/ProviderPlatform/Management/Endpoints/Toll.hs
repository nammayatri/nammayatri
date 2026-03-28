{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Toll where

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

data TollRes = TollRes
  { id :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    isAutoRickshawAllowed :: Kernel.Prelude.Bool,
    isTwoWheelerAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TollRes where
  hideSecrets = Kernel.Prelude.identity

data UpdateTollReq = UpdateTollReq
  { tollId :: Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateTollReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("toll" :> (GetTollList :<|> PutTollUpdate))

type GetTollList = ("list" :> Get '[JSON] [TollRes])

type PutTollUpdate = ("update" :> ReqBody '[JSON] UpdateTollReq :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

data TollAPIs = TollAPIs
  { getTollList :: EulerHS.Types.EulerClient [TollRes],
    putTollUpdate :: UpdateTollReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkTollAPIs :: (Client EulerHS.Types.EulerClient API -> TollAPIs)
mkTollAPIs tollClient = (TollAPIs {..})
  where
    getTollList :<|> putTollUpdate = tollClient

data TollUserActionType
  = GET_TOLL_LIST
  | PUT_TOLL_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON TollUserActionType where
  toJSON GET_TOLL_LIST = Data.Aeson.String "GET_TOLL_LIST"
  toJSON PUT_TOLL_UPDATE = Data.Aeson.String "PUT_TOLL_UPDATE"

instance FromJSON TollUserActionType where
  parseJSON (Data.Aeson.String "GET_TOLL_LIST") = pure GET_TOLL_LIST
  parseJSON (Data.Aeson.String "PUT_TOLL_UPDATE") = pure PUT_TOLL_UPDATE
  parseJSON _ = fail "TollUserActionType expected"

$(Data.Singletons.TH.genSingletons [''TollUserActionType])
