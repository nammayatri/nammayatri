{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.VasBanner where

import qualified Dashboard.Common
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

data VasBannerCreateReq = VasBannerCreateReq
  { title :: Kernel.Prelude.Text,
    subtitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageUrl :: Kernel.Prelude.Text,
    deepLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    whatsappTemplateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    linkType :: VasBannerLinkType,
    priority :: Kernel.Prelude.Int,
    enabled :: Kernel.Prelude.Bool,
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validTo :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VasBannerCreateReq where
  hideSecrets = Kernel.Prelude.identity

data VasBannerLinkType
  = Service
  | SmartFinance
  | WhatsApp
  | ExternalUrl
  | Other
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

instance Kernel.Types.HideSecrets.HideSecrets VasBannerLinkType where
  hideSecrets = Kernel.Prelude.identity

data VasBannerListRes = VasBannerListRes {banners :: [VasBannerRes]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VasBannerRes = VasBannerRes
  { id :: Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig,
    title :: Kernel.Prelude.Text,
    subtitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageUrl :: Kernel.Prelude.Text,
    deepLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    whatsappTemplateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    linkType :: VasBannerLinkType,
    priority :: Kernel.Prelude.Int,
    enabled :: Kernel.Prelude.Bool,
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validTo :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VasBannerUpdateReq = VasBannerUpdateReq
  { title :: Kernel.Prelude.Text,
    subtitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageUrl :: Kernel.Prelude.Text,
    deepLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    whatsappTemplateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    linkType :: VasBannerLinkType,
    priority :: Kernel.Prelude.Int,
    enabled :: Kernel.Prelude.Bool,
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validTo :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VasBannerUpdateReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("vasBanner" :> (GetVasBannerList :<|> PostVasBannerCreate :<|> PostVasBannerUpdate :<|> PostVasBannerDelete))

type GetVasBannerList = ("list" :> QueryParam "enabled" Kernel.Prelude.Bool :> Get ('[JSON]) VasBannerListRes)

type PostVasBannerCreate = ("create" :> ReqBody ('[JSON]) VasBannerCreateReq :> Post ('[JSON]) VasBannerRes)

type PostVasBannerUpdate = (Capture "bannerId" (Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig) :> "update" :> ReqBody ('[JSON]) VasBannerUpdateReq :> Post ('[JSON]) VasBannerRes)

type PostVasBannerDelete = (Capture "bannerId" (Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig) :> "delete" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data VasBannerAPIs = VasBannerAPIs
  { getVasBannerList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> EulerHS.Types.EulerClient VasBannerListRes),
    postVasBannerCreate :: (VasBannerCreateReq -> EulerHS.Types.EulerClient VasBannerRes),
    postVasBannerUpdate :: (Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig -> VasBannerUpdateReq -> EulerHS.Types.EulerClient VasBannerRes),
    postVasBannerDelete :: (Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkVasBannerAPIs :: (Client EulerHS.Types.EulerClient API -> VasBannerAPIs)
mkVasBannerAPIs vasBannerClient = (VasBannerAPIs {..})
  where
    getVasBannerList :<|> postVasBannerCreate :<|> postVasBannerUpdate :<|> postVasBannerDelete = vasBannerClient

data VasBannerUserActionType
  = GET_VAS_BANNER_LIST
  | POST_VAS_BANNER_CREATE
  | POST_VAS_BANNER_UPDATE
  | POST_VAS_BANNER_DELETE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum (''VasBannerLinkType))

$(Data.Singletons.TH.genSingletons [(''VasBannerUserActionType)])
