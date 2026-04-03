{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FleetOwnerInformation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.External.Encryption
import qualified Domain.Types.FleetOwnerInformation
import qualified Tools.Error
import qualified Kernel.Types.Common
import qualified Data.Aeson
import qualified Database.Beam as B



data FleetOwnerInformationT f
    = FleetOwnerInformationT {aadhaarBackImageId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              aadhaarFrontImageId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              aadhaarNumberEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              aadhaarNumberHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
                              aadhaarNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              autoPayStatus :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FleetOwnerInformation.DriverAutoPayStatus)),
                              blockReasonFlag :: (B.C f (Kernel.Prelude.Maybe Tools.Error.BlockReasonFlag)),
                              blocked :: (B.C f Kernel.Prelude.Bool),
                              businessLicenseImageId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              businessLicenseNumberEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              businessLicenseNumberHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
                              businessLicenseNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              dailyCancellationRateBlockingCooldown :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                              enabled :: (B.C f Kernel.Prelude.Bool),
                              fleetDob :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                              fleetName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              fleetOwnerPersonId :: (B.C f Kernel.Prelude.Text),
                              fleetType :: (B.C f Domain.Types.FleetOwnerInformation.FleetType),
                              gstImageId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              gstNumberEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              gstNumberHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
                              gstNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              isBlockedForReferralPayout :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                              isBlockedForScheduledPayout :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                              isEligibleForSubscription :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                              merchantId :: (B.C f Kernel.Prelude.Text),
                              merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                              panImageId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              panNumberEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              panNumberHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
                              panNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              paymentPending :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                              payoutRegAmountRefunded :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                              payoutRegistrationOrderId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              payoutVpa :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              payoutVpaBankAccount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              payoutVpaStatus :: (B.C f (Kernel.Prelude.Maybe Domain.Types.FleetOwnerInformation.PayoutVpaStatus)),
                              referredByOperatorId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              registeredAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                              stripeAddress :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                              stripeIdNumberEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              stripeIdNumberHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
                              subscribed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                              tdsRate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                              ticketPlaceId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              tollRouteBlockedTill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                              vatNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              verified :: (B.C f Kernel.Prelude.Bool),
                              weeklyCancellationRateBlockingCooldown :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FleetOwnerInformationT
    where data PrimaryKey FleetOwnerInformationT f = FleetOwnerInformationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FleetOwnerInformationId . fleetOwnerPersonId
type FleetOwnerInformation = FleetOwnerInformationT Identity

$(enableKVPG (''FleetOwnerInformationT) [('fleetOwnerPersonId)] [])

$(mkTableInstances (''FleetOwnerInformationT) "fleet_owner_information")

