{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.TripTerms where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Utils.GenericPretty
import qualified Data.Text
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH



data TripTerms
    = TripTerms {createdAt :: Kernel.Prelude.UTCTime, descriptions :: [Data.Text.Text], id :: Kernel.Types.Id.Id Domain.Types.TripTerms.TripTerms, updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Show), ( FromJSON), ( ToJSON), ( Kernel.Utils.GenericPretty.PrettyShow))



