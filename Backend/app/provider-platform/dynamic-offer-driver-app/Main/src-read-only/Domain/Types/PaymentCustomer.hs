{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PaymentCustomer where

import Data.Aeson
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PaymentCustomer = PaymentCustomer
  { clientAuthToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientAuthTokenExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    customerId :: Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    paymentServiceName :: Domain.Types.MerchantServiceConfig.ServiceName,
    serviceName :: Domain.Types.Extra.Plan.ServiceNames,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (Eq), (ToJSON), (FromJSON))
