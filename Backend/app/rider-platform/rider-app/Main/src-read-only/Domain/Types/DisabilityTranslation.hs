{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DisabilityTranslation where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DisabilityTranslation = DisabilityTranslation
  { disabilityId :: Kernel.Types.Id.Id Domain.Types.DisabilityTranslation.DisabilityTranslation,
    language :: Kernel.Prelude.Text,
    disabilityTag :: Kernel.Prelude.Text,
    translation :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
