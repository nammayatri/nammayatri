{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSSavedPassenger where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSSavedPassenger = FRFSSavedPassenger
  { age :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    gender :: Domain.Types.Person.Gender,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
