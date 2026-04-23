{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DiscountTierTranslation where

import Data.Aeson
import qualified Domain.Types.DiscountTier
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DiscountTierTranslation = DiscountTierTranslation
  { description :: Kernel.Prelude.Text,
    language :: Kernel.External.Types.Language,
    name :: Kernel.Prelude.Text,
    tierId :: Kernel.Types.Id.Id Domain.Types.DiscountTier.DiscountTier,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
