{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.NammaTagTrigger where

import Data.Aeson
import Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data NammaTagTrigger = NammaTagTrigger {event :: Lib.Yudhishthira.Types.ApplicationEvent, tagName :: Kernel.Prelude.Text, createdAt :: Kernel.Prelude.UTCTime, updatedAt :: Kernel.Prelude.UTCTime}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
