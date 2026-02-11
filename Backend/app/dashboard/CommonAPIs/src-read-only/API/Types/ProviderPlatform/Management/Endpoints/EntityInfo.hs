{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.EntityInfo where

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

data EntityExtraInformation = EntityExtraInformation {entityType :: Kernel.Prelude.Text, entityId :: Kernel.Prelude.Text, entityInfo :: [EntityInfoAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EntityInfoAPIEntity = EntityInfoAPIEntity {questionId :: Kernel.Prelude.Text, question :: Kernel.Prelude.Text, answer :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateEntityInfoReq = UpdateEntityInfoReq {entityType :: Kernel.Prelude.Text, entityId :: Kernel.Prelude.Text, newInfo :: [EntityInfoAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateEntityInfoReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("entityInfo" :> (GetEntityInfoList :<|> PostEntityInfoUpdate))

type GetEntityInfoList = (Capture "entityType" Kernel.Prelude.Text :> Capture "entityId" Kernel.Prelude.Text :> "list" :> Get ('[JSON]) EntityExtraInformation)

type PostEntityInfoUpdate = ("update" :> ReqBody ('[JSON]) UpdateEntityInfoReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data EntityInfoAPIs = EntityInfoAPIs
  { getEntityInfoList :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient EntityExtraInformation),
    postEntityInfoUpdate :: (UpdateEntityInfoReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkEntityInfoAPIs :: (Client EulerHS.Types.EulerClient API -> EntityInfoAPIs)
mkEntityInfoAPIs entityInfoClient = (EntityInfoAPIs {..})
  where
    getEntityInfoList :<|> postEntityInfoUpdate = entityInfoClient

data EntityInfoUserActionType
  = GET_ENTITY_INFO_LIST
  | POST_ENTITY_INFO_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''EntityInfoUserActionType)])
