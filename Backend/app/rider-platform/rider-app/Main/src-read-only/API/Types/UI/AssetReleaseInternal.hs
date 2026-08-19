{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.AssetReleaseInternal where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.AssetRelease
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Servant
import Tools.Auth

data AssetPublishReq = AssetPublishReq
  { assetType :: Domain.Types.AssetRelease.AssetType,
    city :: Kernel.Types.Beckn.Context.City,
    merchantShortId :: Kernel.Prelude.Text,
    sha256 :: Kernel.Prelude.Text,
    sizeBytes :: Kernel.Prelude.Int,
    sourceRef :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    url :: Kernel.Prelude.Text,
    version :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AssetPublishResp = AssetPublishResp {changed :: Kernel.Prelude.Bool, releaseId :: Kernel.Prelude.Text, sha256 :: Kernel.Prelude.Text, version :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AssetReleaseResp = AssetReleaseResp
  { assetType :: Domain.Types.AssetRelease.AssetType,
    publishedAt :: Kernel.Prelude.UTCTime,
    releaseId :: Kernel.Prelude.Text,
    sha256 :: Kernel.Prelude.Text,
    sizeBytes :: Kernel.Prelude.Int,
    sourceRef :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    url :: Kernel.Prelude.Text,
    version :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AssetRollbackReq = AssetRollbackReq {targetReleaseId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AssetRollbackResp = AssetRollbackResp {releaseId :: Kernel.Prelude.Text, rolledBackReleaseId :: Kernel.Prelude.Text, sha256 :: Kernel.Prelude.Text, version :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
