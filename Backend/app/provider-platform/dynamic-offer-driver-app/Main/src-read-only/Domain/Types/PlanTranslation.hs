{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.PlanTranslation where
import Kernel.Prelude
import Data.Aeson
import qualified Data.Text
import qualified Kernel.External.Types
import qualified Kernel.Types.Id
import qualified Domain.Types.Plan
import qualified Tools.Beam.UtilsTH



data PlanTranslation
    = PlanTranslation {description :: Data.Text.Text,
                       language :: Kernel.External.Types.Language,
                       name :: Data.Text.Text,
                       planId :: Kernel.Types.Id.Id Domain.Types.Plan.Plan,
                       createdAt :: Kernel.Prelude.UTCTime,
                       updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



