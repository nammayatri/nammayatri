{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TripTerms where

import Data.Aeson
import qualified Data.Text
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.GenericPretty
import qualified Tools.Beam.UtilsTH

data TripTerms = TripTerms {createdAt :: Kernel.Prelude.UTCTime, descriptions :: [Data.Text.Text], id :: Kernel.Types.Id.Id Domain.Types.TripTerms.TripTerms, updatedAt :: Kernel.Prelude.UTCTime}
  deriving (Generic, (Show), (FromJSON), (ToJSON), (Kernel.Utils.GenericPretty.PrettyShow))
