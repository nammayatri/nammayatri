{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Insurance where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Insurance = Insurance
  { category :: Kernel.Prelude.Text,
    certificateUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    customerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    customerName :: Kernel.Prelude.Text,
    customerPhone :: Kernel.Prelude.Text,
    driverInsuredAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverPhone :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    entityType :: Domain.Types.Insurance.EntityType,
    fairBreakup :: Kernel.Prelude.Maybe Data.Aeson.Value,
    id :: Kernel.Types.Id.Id Domain.Types.Insurance.Insurance,
    insuredAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    partnerId :: Kernel.Prelude.Text,
    plan :: Kernel.Prelude.Text,
    policyId :: Kernel.Prelude.Text,
    policyNumber :: Kernel.Prelude.Text,
    startDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EntityType = RIDE | TICKET deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''EntityType)
