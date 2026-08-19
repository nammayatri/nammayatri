{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSBookingGroup where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data FRFSBookingGroup = FRFSBookingGroup
  { id :: Kernel.Types.Id.Id Domain.Types.FRFSBookingGroup.FRFSBookingGroup,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    paymentOrderShortId :: Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder),
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    status :: Domain.Types.FRFSBookingGroup.FRFSBookingGroupStatus,
    totalPrice :: Kernel.Types.Common.Price,
    totalSlots :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FRFSBookingGroupStatus = NEW | INITIATED | PAYMENT_PENDING | CONFIRMED | PARTIALLY_FAILED | FAILED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FRFSBookingGroupStatus))
