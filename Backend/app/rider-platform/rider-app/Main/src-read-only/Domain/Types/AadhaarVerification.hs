{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AadhaarVerification where

import Data.Aeson
import qualified Domain.Types.Person
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AadhaarVerification = AadhaarVerification
  { personName :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    personGender :: Kernel.Prelude.Text,
    personDob :: Kernel.Prelude.Text,
    personImagePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    aadhaarNumberHash :: Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash,
    isVerified :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, Show)
