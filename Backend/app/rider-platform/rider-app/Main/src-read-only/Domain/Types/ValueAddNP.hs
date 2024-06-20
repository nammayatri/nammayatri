{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ValueAddNP where

import Data.Aeson
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data ValueAddNP = ValueAddNP {subscriberId :: Kernel.Prelude.Text, enabled :: Kernel.Prelude.Bool, createdAt :: Kernel.Prelude.UTCTime, updatedAt :: Kernel.Prelude.UTCTime}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
