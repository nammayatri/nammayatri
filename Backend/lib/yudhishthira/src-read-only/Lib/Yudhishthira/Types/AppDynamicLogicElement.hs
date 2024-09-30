{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.AppDynamicLogicElement where

import Data.Aeson
import qualified Data.Text
import Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data AppDynamicLogicElement = AppDynamicLogicElement
  { description :: Kernel.Prelude.Maybe Data.Text.Text,
    domain :: Lib.Yudhishthira.Types.LogicDomain,
    logic :: Data.Aeson.Value,
    order :: Kernel.Prelude.Int,
    version :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
