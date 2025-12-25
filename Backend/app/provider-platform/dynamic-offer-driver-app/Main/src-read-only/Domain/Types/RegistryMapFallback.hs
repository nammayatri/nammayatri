{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RegistryMapFallback where

import Data.Aeson
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data RegistryMapFallback = RegistryMapFallback {registryUrl :: Kernel.Prelude.BaseUrl, subscriberId :: Kernel.Prelude.Text, uniqueId :: Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
