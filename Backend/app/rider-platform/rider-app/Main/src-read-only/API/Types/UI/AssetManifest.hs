{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.AssetManifest where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.AssetRelease
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data AssetEntry = AssetEntry {assetType :: Domain.Types.AssetRelease.AssetType, publishedAt :: Kernel.Prelude.UTCTime, sha256 :: Kernel.Prelude.Text, url :: Kernel.Prelude.Text, version :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AssetManifestResp = AssetManifestResp {assets :: [AssetEntry]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
