{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FleetOwnerInformation where
import Kernel.Prelude
import Kernel.Utils.TH
import Data.Aeson
import Kernel.External.Encryption
import qualified Tools.Error
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Types.Common
import qualified Kernel.External.Payment.Stripe.Types
import qualified Tools.Beam.UtilsTH



data FleetOwnerInformationE e
    = FleetOwnerInformation {aadhaarBackImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             aadhaarFrontImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             aadhaarNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
                             aadhaarNumberDec :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             autoPayStatus :: Kernel.Prelude.Maybe Domain.Types.FleetOwnerInformation.DriverAutoPayStatus,
                             blockReasonFlag :: Kernel.Prelude.Maybe Tools.Error.BlockReasonFlag,
                             blocked :: Kernel.Prelude.Bool,
                             businessLicenseImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             businessLicenseNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
                             businessLicenseNumberDec :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             dailyCancellationRateBlockingCooldown :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                             enabled :: Kernel.Prelude.Bool,
                             fleetDob :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                             fleetName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             fleetOwnerPersonId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                             fleetType :: Domain.Types.FleetOwnerInformation.FleetType,
                             gstImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             gstNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
                             gstNumberDec :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             isBlockedForReferralPayout :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                             isBlockedForScheduledPayout :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                             isEligibleForSubscription :: Kernel.Prelude.Bool,
                             merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                             merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                             panImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             panNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
                             panNumberDec :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             paymentPending :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                             payoutRegAmountRefunded :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                             payoutRegistrationOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             payoutVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             payoutVpaBankAccount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             payoutVpaStatus :: Kernel.Prelude.Maybe Domain.Types.FleetOwnerInformation.PayoutVpaStatus,
                             referredByOperatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             registeredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                             stripeAddress :: Kernel.Prelude.Maybe Kernel.External.Payment.Stripe.Types.Address,
                             stripeIdNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
                             subscribed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                             tdsRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
                             ticketPlaceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             tollRouteBlockedTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                             vatNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             verified :: Kernel.Prelude.Bool,
                             weeklyCancellationRateBlockingCooldown :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                             createdAt :: Kernel.Prelude.UTCTime,
                             updatedAt :: Kernel.Prelude.UTCTime}
    deriving Generic
type FleetOwnerInformation = FleetOwnerInformationE ('AsEncrypted)
type DecryptedFleetOwnerInformation = FleetOwnerInformationE ('AsUnencrypted)
instance EncryptedItem FleetOwnerInformation
    where type Unencrypted FleetOwnerInformation = (DecryptedFleetOwnerInformation, HashSalt)
          encryptItem (entity, salt) = do {aadhaarNumber_ <- encryptItem $ (, salt) <$> aadhaarNumber entity;
                                           businessLicenseNumber_ <- encryptItem $ (, salt) <$> businessLicenseNumber entity;
                                           gstNumber_ <- encryptItem $ (, salt) <$> gstNumber entity;
                                           panNumber_ <- encryptItem $ (, salt) <$> panNumber entity;
                                           stripeIdNumber_ <- encryptItem $ (, salt) <$> stripeIdNumber entity;
                                           pure FleetOwnerInformation{aadhaarBackImageId = aadhaarBackImageId entity,
                                                                      aadhaarFrontImageId = aadhaarFrontImageId entity,
                                                                      aadhaarNumber = aadhaarNumber_,
                                                                      aadhaarNumberDec = aadhaarNumberDec entity,
                                                                      autoPayStatus = autoPayStatus entity,
                                                                      blockReasonFlag = blockReasonFlag entity,
                                                                      blocked = blocked entity,
                                                                      businessLicenseImageId = businessLicenseImageId entity,
                                                                      businessLicenseNumber = businessLicenseNumber_,
                                                                      businessLicenseNumberDec = businessLicenseNumberDec entity,
                                                                      dailyCancellationRateBlockingCooldown = dailyCancellationRateBlockingCooldown entity,
                                                                      enabled = enabled entity,
                                                                      fleetDob = fleetDob entity,
                                                                      fleetName = fleetName entity,
                                                                      fleetOwnerPersonId = fleetOwnerPersonId entity,
                                                                      fleetType = fleetType entity,
                                                                      gstImageId = gstImageId entity,
                                                                      gstNumber = gstNumber_,
                                                                      gstNumberDec = gstNumberDec entity,
                                                                      isBlockedForReferralPayout = isBlockedForReferralPayout entity,
                                                                      isBlockedForScheduledPayout = isBlockedForScheduledPayout entity,
                                                                      isEligibleForSubscription = isEligibleForSubscription entity,
                                                                      merchantId = merchantId entity,
                                                                      merchantOperatingCityId = merchantOperatingCityId entity,
                                                                      panImageId = panImageId entity,
                                                                      panNumber = panNumber_,
                                                                      panNumberDec = panNumberDec entity,
                                                                      paymentPending = paymentPending entity,
                                                                      payoutRegAmountRefunded = payoutRegAmountRefunded entity,
                                                                      payoutRegistrationOrderId = payoutRegistrationOrderId entity,
                                                                      payoutVpa = payoutVpa entity,
                                                                      payoutVpaBankAccount = payoutVpaBankAccount entity,
                                                                      payoutVpaStatus = payoutVpaStatus entity,
                                                                      referredByOperatorId = referredByOperatorId entity,
                                                                      registeredAt = registeredAt entity,
                                                                      stripeAddress = stripeAddress entity,
                                                                      stripeIdNumber = stripeIdNumber_,
                                                                      subscribed = subscribed entity,
                                                                      tdsRate = tdsRate entity,
                                                                      ticketPlaceId = ticketPlaceId entity,
                                                                      tollRouteBlockedTill = tollRouteBlockedTill entity,
                                                                      vatNumber = vatNumber entity,
                                                                      verified = verified entity,
                                                                      weeklyCancellationRateBlockingCooldown = weeklyCancellationRateBlockingCooldown entity,
                                                                      createdAt = createdAt entity,
                                                                      updatedAt = updatedAt entity}}
          decryptItem entity = do {aadhaarNumber_ <- fmap fst <$> decryptItem (aadhaarNumber entity);
                                   businessLicenseNumber_ <- fmap fst <$> decryptItem (businessLicenseNumber entity);
                                   gstNumber_ <- fmap fst <$> decryptItem (gstNumber entity);
                                   panNumber_ <- fmap fst <$> decryptItem (panNumber entity);
                                   stripeIdNumber_ <- fmap fst <$> decryptItem (stripeIdNumber entity);
                                   pure (FleetOwnerInformation{aadhaarBackImageId = aadhaarBackImageId entity,
                                                               aadhaarFrontImageId = aadhaarFrontImageId entity,
                                                               aadhaarNumber = aadhaarNumber_,
                                                               aadhaarNumberDec = aadhaarNumberDec entity,
                                                               autoPayStatus = autoPayStatus entity,
                                                               blockReasonFlag = blockReasonFlag entity,
                                                               blocked = blocked entity,
                                                               businessLicenseImageId = businessLicenseImageId entity,
                                                               businessLicenseNumber = businessLicenseNumber_,
                                                               businessLicenseNumberDec = businessLicenseNumberDec entity,
                                                               dailyCancellationRateBlockingCooldown = dailyCancellationRateBlockingCooldown entity,
                                                               enabled = enabled entity,
                                                               fleetDob = fleetDob entity,
                                                               fleetName = fleetName entity,
                                                               fleetOwnerPersonId = fleetOwnerPersonId entity,
                                                               fleetType = fleetType entity,
                                                               gstImageId = gstImageId entity,
                                                               gstNumber = gstNumber_,
                                                               gstNumberDec = gstNumberDec entity,
                                                               isBlockedForReferralPayout = isBlockedForReferralPayout entity,
                                                               isBlockedForScheduledPayout = isBlockedForScheduledPayout entity,
                                                               isEligibleForSubscription = isEligibleForSubscription entity,
                                                               merchantId = merchantId entity,
                                                               merchantOperatingCityId = merchantOperatingCityId entity,
                                                               panImageId = panImageId entity,
                                                               panNumber = panNumber_,
                                                               panNumberDec = panNumberDec entity,
                                                               paymentPending = paymentPending entity,
                                                               payoutRegAmountRefunded = payoutRegAmountRefunded entity,
                                                               payoutRegistrationOrderId = payoutRegistrationOrderId entity,
                                                               payoutVpa = payoutVpa entity,
                                                               payoutVpaBankAccount = payoutVpaBankAccount entity,
                                                               payoutVpaStatus = payoutVpaStatus entity,
                                                               referredByOperatorId = referredByOperatorId entity,
                                                               registeredAt = registeredAt entity,
                                                               stripeAddress = stripeAddress entity,
                                                               stripeIdNumber = stripeIdNumber_,
                                                               subscribed = subscribed entity,
                                                               tdsRate = tdsRate entity,
                                                               ticketPlaceId = ticketPlaceId entity,
                                                               tollRouteBlockedTill = tollRouteBlockedTill entity,
                                                               vatNumber = vatNumber entity,
                                                               verified = verified entity,
                                                               weeklyCancellationRateBlockingCooldown = weeklyCancellationRateBlockingCooldown entity,
                                                               createdAt = createdAt entity,
                                                               updatedAt = updatedAt entity},
                                         "")}
instance EncryptedItem' FleetOwnerInformation
    where type UnencryptedItem FleetOwnerInformation = DecryptedFleetOwnerInformation
          toUnencrypted a salt = (a, salt)
          fromUnencrypted = fst
data DriverAutoPayStatus
    = PENDING | ACTIVE | SUSPENDED | PAUSED_PSP | CANCELLED_PSP | MANDATE_FAILED | MANDATE_EXPIRED
    deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
data FleetType = RENTAL_FLEET | NORMAL_FLEET | BUSINESS_FLEET deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
data PayoutVpaStatus = VIA_WEBHOOK | MANUALLY_ADDED | VERIFIED_BY_USER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''DriverAutoPayStatus))

$(mkHttpInstancesForEnum (''DriverAutoPayStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FleetType))

$(mkHttpInstancesForEnum (''FleetType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PayoutVpaStatus))

