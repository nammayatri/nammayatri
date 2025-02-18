{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AadhaarVerification where

import Data.Aeson
import qualified Domain.Types.Person
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AadhaarVerification = AadhaarVerification
  { aadhaarNumberHash :: Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash,
    createdAt :: Kernel.Prelude.UTCTime,
    isVerified :: Kernel.Prelude.Bool,
    personDob :: Kernel.Prelude.Text,
    personGender :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    personImagePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    personName :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, Show)
