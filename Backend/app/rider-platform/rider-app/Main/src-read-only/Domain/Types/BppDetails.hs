{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BppDetails where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data BppDetails = BppDetails
  { description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    domain :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.BppDetails.BppDetails,
    logoUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    subscriberId :: Kernel.Prelude.Text,
    supportNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
