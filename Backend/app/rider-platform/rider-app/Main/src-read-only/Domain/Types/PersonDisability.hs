{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.PersonDisability where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Tools.Beam.UtilsTH



data PersonDisability
    = PersonDisability {createdAt :: Kernel.Prelude.UTCTime,
                        description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                        disabilityId :: Kernel.Prelude.Text,
                        personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                        tag :: Kernel.Prelude.Text,
                        updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



