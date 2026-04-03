{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.PortalConfigs where
import Kernel.Prelude
import qualified Kernel.Types.Id



data PortalConfigs
    = PortalConfigs {configName :: Kernel.Prelude.Text,
                     createdAt :: Kernel.Prelude.UTCTime,
                     id :: Kernel.Types.Id.Id Domain.Types.PortalConfigs.PortalConfigs,
                     updatedAt :: Kernel.Prelude.UTCTime,
                     value :: Kernel.Prelude.Text}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



