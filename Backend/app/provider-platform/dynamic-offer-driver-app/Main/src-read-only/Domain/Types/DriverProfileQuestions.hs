{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverProfileQuestions where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverProfileQuestions = DriverProfileQuestions
  { aspirations :: [Kernel.Prelude.Text],
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    expertAt :: [Kernel.Prelude.Text],
    hometown :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    pledges :: [Kernel.Prelude.Text],
    updatedAt :: Kernel.Prelude.UTCTime,
    whyNY :: [Kernel.Prelude.Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
