{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PortalConfigs where

import Kernel.Prelude
import qualified Kernel.Types.Id

data PortalConfigs = PortalConfigs
  { configName :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.PortalConfigs.PortalConfigs,
    updatedAt :: Kernel.Prelude.UTCTime,
    value :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

{-
	DSL Source Link: file://./../../../spec/Storage/suspect.yaml
-}
