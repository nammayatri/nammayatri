{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DiscountTranslation where

import Data.Aeson
import qualified Domain.Types.Discount
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DiscountTranslation = DiscountTranslation
  { description :: Kernel.Prelude.Text,
    discountId :: Kernel.Types.Id.Id Domain.Types.Discount.Discount,
    language :: Kernel.External.Types.Language,
    name :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
