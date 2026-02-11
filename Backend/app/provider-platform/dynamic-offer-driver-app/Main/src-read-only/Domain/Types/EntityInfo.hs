{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.EntityInfo where

import Data.Aeson
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data EntityInfo = EntityInfo {answer :: Kernel.Prelude.Text, entityId :: Kernel.Prelude.Text, entityType :: Kernel.Prelude.Text, question :: Kernel.Prelude.Text, questionId :: Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
