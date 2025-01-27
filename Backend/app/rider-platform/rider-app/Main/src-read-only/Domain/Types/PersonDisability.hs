{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PersonDisability where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PersonDisability = PersonDisability
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disabilityId :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    tag :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
