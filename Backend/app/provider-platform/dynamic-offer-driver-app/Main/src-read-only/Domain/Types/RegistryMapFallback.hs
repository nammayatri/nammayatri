{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RegistryMapFallback where
import Kernel.Prelude
import Data.Aeson
import qualified Tools.Beam.UtilsTH



data RegistryMapFallback
    = RegistryMapFallback {registryUrl :: Kernel.Prelude.BaseUrl, subscriberId :: Kernel.Prelude.Text, uniqueId :: Kernel.Prelude.Text}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



