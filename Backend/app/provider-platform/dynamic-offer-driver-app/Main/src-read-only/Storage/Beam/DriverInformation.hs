{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverInformation where

import qualified Database.Beam as B
import qualified Domain.Types.DriverInformation
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverInformationT f = DriverInformationT
  { aadhaarVerified :: B.C f Kernel.Prelude.Bool,
    acRestrictionLiftCount :: B.C f Kernel.Prelude.Int,
    acUsageRestrictionType :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverInformation.AirConditionedRestrictionType),
    active :: B.C f Kernel.Prelude.Bool,
    adminId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    airConditionScore :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    autoPayStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverInformation.DriverAutoPayStatus),
    availableUpiApps :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    blockExpiryTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    blockStateModifier :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    blocked :: B.C f Kernel.Prelude.Bool,
    blockedReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    canDowngradeToHatchback :: B.C f Kernel.Prelude.Bool,
    canDowngradeToSedan :: B.C f Kernel.Prelude.Bool,
    canDowngradeToTaxi :: B.C f Kernel.Prelude.Bool,
    canSwitchToInterCity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    canSwitchToRental :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    compAadhaarImagePath :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverDob :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverId :: B.C f Kernel.Prelude.Text,
    enabled :: B.C f Kernel.Prelude.Bool,
    enabledAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    forwardBatchingEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    hasAdvanceBooking :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    lastACStatusCheckedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    lastEnabledOn :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    mode :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverInformation.DriverMode),
    numOfLocks :: B.C f Kernel.Prelude.Int,
    onRide :: B.C f Kernel.Prelude.Bool,
    payerVpa :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentPending :: B.C f Kernel.Prelude.Bool,
    referralCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referredByDriverId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    subscribed :: B.C f Kernel.Prelude.Bool,
    tollRelatedIssueCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    totalReferred :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    verified :: B.C f Kernel.Prelude.Bool,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverInformationT where
  data PrimaryKey DriverInformationT f = DriverInformationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverInformationId . driverId

type DriverInformation = DriverInformationT Identity

$(enableKVPG ''DriverInformationT ['driverId] [])

$(mkTableInstances ''DriverInformationT "driver_information")
