{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverProfileQuestions where

import Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverProfileQuestions = DriverProfileQuestions
  { aboutMe :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    aspirations :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    drivingSince :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    hometown :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    pledges :: [Kernel.Prelude.Text],
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleTags :: Kernel.Prelude.Maybe [Kernel.Prelude.Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
