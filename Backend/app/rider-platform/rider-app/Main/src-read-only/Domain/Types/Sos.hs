{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Sos where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Sos = Sos
  { flow :: Domain.Types.Sos.SosType,
    id :: Kernel.Types.Id.Id Domain.Types.Sos.Sos,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    status :: Domain.Types.Sos.SosStatus,
    ticketId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EmergencyContactId = EmergencyContactId Kernel.Prelude.Text
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosStatus = Resolved | NotResolved | Pending
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosType = Police | CustomerCare | EmergencyContact Domain.Types.Sos.EmergencyContactId | SafetyFlow
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''EmergencyContactId)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SosStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SosType)
