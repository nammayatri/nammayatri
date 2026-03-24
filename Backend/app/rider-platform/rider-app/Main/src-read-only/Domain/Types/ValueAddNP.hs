{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.ValueAddNP where
import Kernel.Prelude
import Data.Aeson
import qualified Tools.Beam.UtilsTH



data ValueAddNP
    = ValueAddNP {enabled :: Kernel.Prelude.Bool, subscriberId :: Kernel.Prelude.Text, createdAt :: Kernel.Prelude.UTCTime, updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



