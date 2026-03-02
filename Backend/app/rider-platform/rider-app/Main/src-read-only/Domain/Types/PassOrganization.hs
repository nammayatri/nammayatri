{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PassOrganization where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PassOrganization = PassOrganization
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Data.Time.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    passEnum :: Domain.Types.PassType.PassEnum,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    updatedAt :: Data.Time.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
