{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverReferral where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverReferral = DriverReferral
  { driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    linkedAt :: Kernel.Prelude.UTCTime,
    referralCode :: Kernel.Types.Id.Id Domain.Types.DriverReferral.DriverReferral,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
