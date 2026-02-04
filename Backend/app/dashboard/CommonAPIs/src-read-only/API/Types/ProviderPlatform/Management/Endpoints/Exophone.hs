{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Exophone where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Kernel.Utils.TH
import Servant
import Servant.Client

data CreateExophoneReq = CreateExophoneReq {exophonePrimaryPhone :: Kernel.Prelude.Text, exophoneBackupPhone :: Kernel.Prelude.Text, exophoneCallService :: ExophoneCallService, exophoneExophoneType :: ExophoneExophoneType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateExophoneReq where
  hideSecrets = Kernel.Prelude.identity

data ExophoneCallService
  = Exotel
  | Knowlarity
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data ExophoneExophoneType
  = CALL_RIDE
  | END_RIDE
  | CALL_DELIVERY_SENDER
  | CALL_DELIVERY_RECEIVER
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data ExophoneItem = ExophoneItem
  { exophoneId :: Kernel.Prelude.Text,
    exophonePrimaryPhone :: Kernel.Prelude.Text,
    exophoneBackupPhone :: Kernel.Prelude.Text,
    exophoneCallService :: ExophoneCallService,
    exophoneExophoneType :: ExophoneExophoneType,
    exophoneIsPrimaryDown :: Kernel.Prelude.Bool,
    exophoneMerchantId :: Kernel.Prelude.Text,
    exophoneMerchantOperatingCityId :: Kernel.Prelude.Text,
    exophoneCreatedAt :: Kernel.Prelude.UTCTime,
    exophoneUpdatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExophoneListRes = ExophoneListRes {exophones :: [ExophoneItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExophoneRes = ExophoneRes {exophone :: ExophoneItem}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateExophoneReq = UpdateExophoneReq
  { exophonePrimaryPhone :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    exophoneBackupPhone :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    exophoneIsPrimaryDown :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateExophoneReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("exophone" :> (PostExophoneCreate :<|> GetExophoneList :<|> GetExophone :<|> PostExophoneUpdate :<|> DeleteExophoneDelete))

type PostExophoneCreate = ("create" :> ReqBody '[JSON] CreateExophoneReq :> Post '[JSON] ExophoneRes)

type GetExophoneList = ("list" :> QueryParam "merchantOperatingCityId" Kernel.Prelude.Text :> Get '[JSON] ExophoneListRes)

type GetExophone = (Capture "exophoneId" Kernel.Prelude.Text :> Get '[JSON] ExophoneRes)

type PostExophoneUpdate = (Capture "exophoneId" Kernel.Prelude.Text :> "update" :> ReqBody '[JSON] UpdateExophoneReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type DeleteExophoneDelete = (Capture "exophoneId" Kernel.Prelude.Text :> "delete" :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

data ExophoneAPIs = ExophoneAPIs
  { postExophoneCreate :: CreateExophoneReq -> EulerHS.Types.EulerClient ExophoneRes,
    getExophoneList :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient ExophoneListRes,
    getExophone :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient ExophoneRes,
    postExophoneUpdate :: Kernel.Prelude.Text -> UpdateExophoneReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteExophoneDelete :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkExophoneAPIs :: (Client EulerHS.Types.EulerClient API -> ExophoneAPIs)
mkExophoneAPIs exophoneClient = (ExophoneAPIs {..})
  where
    postExophoneCreate :<|> getExophoneList :<|> getExophone :<|> postExophoneUpdate :<|> deleteExophoneDelete = exophoneClient

data ExophoneUserActionType
  = POST_EXOPHONE_CREATE
  | GET_EXOPHONE_LIST
  | GET_EXOPHONE
  | POST_EXOPHONE_UPDATE
  | DELETE_EXOPHONE_DELETE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''ExophoneCallService)

$(mkHttpInstancesForEnum ''ExophoneExophoneType)

$(Data.Singletons.TH.genSingletons [''ExophoneUserActionType])
