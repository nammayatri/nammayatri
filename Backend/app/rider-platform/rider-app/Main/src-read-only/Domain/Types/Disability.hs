{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.Disability where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH



data Disability
    = Disability {description :: Kernel.Prelude.Text, id :: Kernel.Types.Id.Id Domain.Types.Disability.Disability, tag :: Kernel.Prelude.Text}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



