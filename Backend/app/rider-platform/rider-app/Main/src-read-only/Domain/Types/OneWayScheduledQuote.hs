{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.OneWayScheduledQuote where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.GenericPretty
import qualified Tools.Beam.UtilsTH

data OneWayScheduledQuote = OneWayScheduledQuote
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote,
    quoteId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (Kernel.Utils.GenericPretty.PrettyShow))
