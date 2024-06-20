{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SpecialZoneQuote where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.GenericPretty
import qualified Tools.Beam.UtilsTH

data SpecialZoneQuote = SpecialZoneQuote {id :: Kernel.Types.Id.Id Domain.Types.SpecialZoneQuote.SpecialZoneQuote, quoteId :: Kernel.Prelude.Text, createdAt :: Kernel.Prelude.UTCTime, updatedAt :: Kernel.Prelude.UTCTime}
  deriving (Generic, Show, Kernel.Utils.GenericPretty.PrettyShow)
