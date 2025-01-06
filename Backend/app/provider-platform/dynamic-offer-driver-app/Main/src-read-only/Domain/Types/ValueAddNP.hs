{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ValueAddNP where

import Data.Aeson
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data ValueAddNP = ValueAddNP {enabled :: Kernel.Prelude.Bool, subscriberId :: Kernel.Prelude.Text, createdAt :: Kernel.Prelude.UTCTime, updatedAt :: Kernel.Prelude.UTCTime}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
