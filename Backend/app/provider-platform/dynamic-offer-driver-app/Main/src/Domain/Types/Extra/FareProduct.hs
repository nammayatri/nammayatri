{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.FareProduct
  ( BoundedPeaks (..),
    TimeBound (..),
  )
where

import Data.Time
import Kernel.Prelude
import Kernel.Utils.GenericPretty
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data BoundedPeaks = BoundedPeaks
  { monday :: [(TimeOfDay, TimeOfDay)],
    tuesday :: [(TimeOfDay, TimeOfDay)],
    wednesday :: [(TimeOfDay, TimeOfDay)],
    thursday :: [(TimeOfDay, TimeOfDay)],
    friday :: [(TimeOfDay, TimeOfDay)],
    saturday :: [(TimeOfDay, TimeOfDay)],
    sunday :: [(TimeOfDay, TimeOfDay)]
  }
  deriving (Eq, Ord, Generic, Show, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable BoundedPeaks

data TimeBound
  = BoundedByWeekday BoundedPeaks
  | BoundedByDay [(Day, [(TimeOfDay, TimeOfDay)])]
  | Unbounded
  deriving (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable TimeBound

instance Show TimeBound where
  show Unbounded = "Unbounded"
  show (BoundedByWeekday peaks) = show peaks
  show (BoundedByDay days) = show days

instance Read TimeBound where
  readsPrec _ str
    | str == "Unbounded" = [(Unbounded, "")]
    | otherwise =
      case (readMaybe str :: Maybe BoundedPeaks) of
        Just bound -> [(BoundedByWeekday bound, "")]
        Nothing ->
          case (readMaybe str :: Maybe [(Day, [(TimeOfDay, TimeOfDay)])]) of
            Just bound -> [(BoundedByDay bound, "")]
            Nothing -> [(Unbounded, "")]

$(mkBeamInstancesForEnum ''TimeBound)
