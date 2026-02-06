{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Organization where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Organization = Organization
  { createdAt :: Data.Time.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.Organization.Organization,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    organizationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    organizationName :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    updatedAt :: Data.Time.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
