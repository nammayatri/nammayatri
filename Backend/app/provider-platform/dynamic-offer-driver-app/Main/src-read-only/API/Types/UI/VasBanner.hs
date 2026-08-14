{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.VasBanner where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.VasBannerConfig
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data VasBannerAPIEntity = VasBannerAPIEntity
  { deepLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.VasBannerConfig.VasBannerConfig,
    imageUrl :: Kernel.Prelude.Text,
    linkType :: Domain.Types.VasBannerConfig.VasBannerLinkType,
    priority :: Kernel.Prelude.Int,
    subtitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text,
    whatsappTemplateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VasBannerListRes = VasBannerListRes {banners :: [API.Types.UI.VasBanner.VasBannerAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
