{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.FleetOwnerInformation where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FleetOwnerInformation
import qualified Storage.Beam.FleetOwnerInformation as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Storage.Queries.Transformers.FleetOwnerInformation
import qualified Kernel.Utils.JSON



instance FromTType' Beam.FleetOwnerInformation Domain.Types.FleetOwnerInformation.FleetOwnerInformation
    where fromTType' (Beam.FleetOwnerInformationT {..}) = do pure $ Just Domain.Types.FleetOwnerInformation.FleetOwnerInformation{aadhaarBackImageId = aadhaarBackImageId,
                                                                                                                                  aadhaarFrontImageId = aadhaarFrontImageId,
                                                                                                                                  aadhaarNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem aadhaarNumberEncrypted aadhaarNumberHash,
                                                                                                                                  aadhaarNumberDec = aadhaarNumber,
                                                                                                                                  autoPayStatus = autoPayStatus,
                                                                                                                                  blockReasonFlag = blockReasonFlag,
                                                                                                                                  blocked = blocked,
                                                                                                                                  businessLicenseImageId = businessLicenseImageId,
                                                                                                                                  businessLicenseNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem businessLicenseNumberEncrypted businessLicenseNumberHash,
                                                                                                                                  businessLicenseNumberDec = businessLicenseNumber,
                                                                                                                                  dailyCancellationRateBlockingCooldown = dailyCancellationRateBlockingCooldown,
                                                                                                                                  enabled = enabled,
                                                                                                                                  fleetDob = fleetDob,
                                                                                                                                  fleetName = fleetName,
                                                                                                                                  fleetOwnerPersonId = Kernel.Types.Id.Id fleetOwnerPersonId,
                                                                                                                                  fleetType = fleetType,
                                                                                                                                  gstImageId = gstImageId,
                                                                                                                                  gstNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem gstNumberEncrypted gstNumberHash,
                                                                                                                                  gstNumberDec = gstNumber,
                                                                                                                                  isBlockedForReferralPayout = isBlockedForReferralPayout,
                                                                                                                                  isBlockedForScheduledPayout = isBlockedForScheduledPayout,
                                                                                                                                  isEligibleForSubscription = fromMaybe True isEligibleForSubscription ,
                                                                                                                                  merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                                  merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                                                                  panImageId = panImageId,
                                                                                                                                  panNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem panNumberEncrypted panNumberHash,
                                                                                                                                  panNumberDec = panNumber,
                                                                                                                                  paymentPending = paymentPending,
                                                                                                                                  payoutRegAmountRefunded = payoutRegAmountRefunded,
                                                                                                                                  payoutRegistrationOrderId = payoutRegistrationOrderId,
                                                                                                                                  payoutVpa = payoutVpa,
                                                                                                                                  payoutVpaBankAccount = payoutVpaBankAccount,
                                                                                                                                  payoutVpaStatus = payoutVpaStatus,
                                                                                                                                  referredByOperatorId = referredByOperatorId,
                                                                                                                                  registeredAt = registeredAt,
                                                                                                                                  stripeAddress = stripeAddress >>= Kernel.Utils.JSON.valueToMaybe,
                                                                                                                                  stripeIdNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem stripeIdNumberEncrypted stripeIdNumberHash,
                                                                                                                                  subscribed = subscribed,
                                                                                                                                  tdsRate = tdsRate,
                                                                                                                                  ticketPlaceId = ticketPlaceId,
                                                                                                                                  tollRouteBlockedTill = tollRouteBlockedTill,
                                                                                                                                  vatNumber = vatNumber,
                                                                                                                                  verified = verified,
                                                                                                                                  weeklyCancellationRateBlockingCooldown = weeklyCancellationRateBlockingCooldown,
                                                                                                                                  createdAt = createdAt,
                                                                                                                                  updatedAt = updatedAt}
instance ToTType' Beam.FleetOwnerInformation Domain.Types.FleetOwnerInformation.FleetOwnerInformation
    where toTType' (Domain.Types.FleetOwnerInformation.FleetOwnerInformation {..}) = do Beam.FleetOwnerInformationT{Beam.aadhaarBackImageId = aadhaarBackImageId,
                                                                                                                    Beam.aadhaarFrontImageId = aadhaarFrontImageId,
                                                                                                                    Beam.aadhaarNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted aadhaarNumber,
                                                                                                                    Beam.aadhaarNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash aadhaarNumber,
                                                                                                                    Beam.aadhaarNumber = Nothing,
                                                                                                                    Beam.autoPayStatus = autoPayStatus,
                                                                                                                    Beam.blockReasonFlag = blockReasonFlag,
                                                                                                                    Beam.blocked = blocked,
                                                                                                                    Beam.businessLicenseImageId = businessLicenseImageId,
                                                                                                                    Beam.businessLicenseNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted businessLicenseNumber,
                                                                                                                    Beam.businessLicenseNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash businessLicenseNumber,
                                                                                                                    Beam.businessLicenseNumber = Nothing,
                                                                                                                    Beam.dailyCancellationRateBlockingCooldown = dailyCancellationRateBlockingCooldown,
                                                                                                                    Beam.enabled = enabled,
                                                                                                                    Beam.fleetDob = fleetDob,
                                                                                                                    Beam.fleetName = fleetName,
                                                                                                                    Beam.fleetOwnerPersonId = Kernel.Types.Id.getId fleetOwnerPersonId,
                                                                                                                    Beam.fleetType = fleetType,
                                                                                                                    Beam.gstImageId = gstImageId,
                                                                                                                    Beam.gstNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted gstNumber,
                                                                                                                    Beam.gstNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash gstNumber,
                                                                                                                    Beam.gstNumber = Nothing,
                                                                                                                    Beam.isBlockedForReferralPayout = isBlockedForReferralPayout,
                                                                                                                    Beam.isBlockedForScheduledPayout = isBlockedForScheduledPayout,
                                                                                                                    Beam.isEligibleForSubscription = Just isEligibleForSubscription ,
                                                                                                                    Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                                    Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                                                                    Beam.panImageId = panImageId,
                                                                                                                    Beam.panNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted panNumber,
                                                                                                                    Beam.panNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash panNumber,
                                                                                                                    Beam.panNumber = Nothing,
                                                                                                                    Beam.paymentPending = paymentPending,
                                                                                                                    Beam.payoutRegAmountRefunded = payoutRegAmountRefunded,
                                                                                                                    Beam.payoutRegistrationOrderId = payoutRegistrationOrderId,
                                                                                                                    Beam.payoutVpa = payoutVpa,
                                                                                                                    Beam.payoutVpaBankAccount = payoutVpaBankAccount,
                                                                                                                    Beam.payoutVpaStatus = payoutVpaStatus,
                                                                                                                    Beam.referredByOperatorId = referredByOperatorId,
                                                                                                                    Beam.registeredAt = registeredAt,
                                                                                                                    Beam.stripeAddress = Kernel.Prelude.fmap toJSON stripeAddress,
                                                                                                                    Beam.stripeIdNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted stripeIdNumber,
                                                                                                                    Beam.stripeIdNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash stripeIdNumber,
                                                                                                                    Beam.subscribed = subscribed,
                                                                                                                    Beam.tdsRate = tdsRate,
                                                                                                                    Beam.ticketPlaceId = ticketPlaceId,
                                                                                                                    Beam.tollRouteBlockedTill = tollRouteBlockedTill,
                                                                                                                    Beam.vatNumber = vatNumber,
                                                                                                                    Beam.verified = verified,
                                                                                                                    Beam.weeklyCancellationRateBlockingCooldown = weeklyCancellationRateBlockingCooldown,
                                                                                                                    Beam.createdAt = createdAt,
                                                                                                                    Beam.updatedAt = updatedAt}



