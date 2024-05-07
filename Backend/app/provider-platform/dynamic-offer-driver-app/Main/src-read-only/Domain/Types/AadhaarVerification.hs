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
  { aadhaarNumberHash :: Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash,
    driverDob :: Kernel.Prelude.Text,
    driverGender :: Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverImagePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    isVerified :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
