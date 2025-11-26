{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetOwnerInformation where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Stripe.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetOwnerInformationE e = FleetOwnerInformation
  { aadhaarBackImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    aadhaarFrontImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    aadhaarNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    aadhaarNumberDec :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blocked :: Kernel.Prelude.Bool,
    businessLicenseImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    businessLicenseNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    businessLicenseNumberDec :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    fleetDob :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fleetOwnerPersonId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    fleetType :: Domain.Types.FleetOwnerInformation.FleetType,
    gstImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    gstNumberDec :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isEligibleForSubscription :: Kernel.Prelude.Bool,
    lienAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    panImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    panNumberDec :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planExpiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    prepaidSubscriptionBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    referredByOperatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registeredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    stripeAddress :: Kernel.Prelude.Maybe Kernel.External.Payment.Stripe.Types.Address,
    stripeIdNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    ticketPlaceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verified :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type FleetOwnerInformation = FleetOwnerInformationE 'AsEncrypted

type DecryptedFleetOwnerInformation = FleetOwnerInformationE 'AsUnencrypted

instance EncryptedItem FleetOwnerInformation where
  type Unencrypted FleetOwnerInformation = (DecryptedFleetOwnerInformation, HashSalt)
  encryptItem (entity, salt) = do
    aadhaarNumber_ <- encryptItem $ (,salt) <$> aadhaarNumber entity
    businessLicenseNumber_ <- encryptItem $ (,salt) <$> businessLicenseNumber entity
    gstNumber_ <- encryptItem $ (,salt) <$> gstNumber entity
    panNumber_ <- encryptItem $ (,salt) <$> panNumber entity
    stripeIdNumber_ <- encryptItem $ (,salt) <$> stripeIdNumber entity
    pure
      FleetOwnerInformation
        { aadhaarBackImageId = aadhaarBackImageId entity,
          aadhaarFrontImageId = aadhaarFrontImageId entity,
          aadhaarNumber = aadhaarNumber_,
          aadhaarNumberDec = aadhaarNumberDec entity,
          blocked = blocked entity,
          businessLicenseImageId = businessLicenseImageId entity,
          businessLicenseNumber = businessLicenseNumber_,
          businessLicenseNumberDec = businessLicenseNumberDec entity,
          enabled = enabled entity,
          fleetDob = fleetDob entity,
          fleetOwnerPersonId = fleetOwnerPersonId entity,
          fleetType = fleetType entity,
          gstImageId = gstImageId entity,
          gstNumber = gstNumber_,
          gstNumberDec = gstNumberDec entity,
          isEligibleForSubscription = isEligibleForSubscription entity,
          lienAmount = lienAmount entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          panImageId = panImageId entity,
          panNumber = panNumber_,
          panNumberDec = panNumberDec entity,
          planExpiryDate = planExpiryDate entity,
          prepaidSubscriptionBalance = prepaidSubscriptionBalance entity,
          referredByOperatorId = referredByOperatorId entity,
          registeredAt = registeredAt entity,
          stripeAddress = stripeAddress entity,
          stripeIdNumber = stripeIdNumber_,
          ticketPlaceId = ticketPlaceId entity,
          verified = verified entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    aadhaarNumber_ <- fmap fst <$> decryptItem (aadhaarNumber entity)
    businessLicenseNumber_ <- fmap fst <$> decryptItem (businessLicenseNumber entity)
    gstNumber_ <- fmap fst <$> decryptItem (gstNumber entity)
    panNumber_ <- fmap fst <$> decryptItem (panNumber entity)
    stripeIdNumber_ <- fmap fst <$> decryptItem (stripeIdNumber entity)
    pure
      ( FleetOwnerInformation
          { aadhaarBackImageId = aadhaarBackImageId entity,
            aadhaarFrontImageId = aadhaarFrontImageId entity,
            aadhaarNumber = aadhaarNumber_,
            aadhaarNumberDec = aadhaarNumberDec entity,
            blocked = blocked entity,
            businessLicenseImageId = businessLicenseImageId entity,
            businessLicenseNumber = businessLicenseNumber_,
            businessLicenseNumberDec = businessLicenseNumberDec entity,
            enabled = enabled entity,
            fleetDob = fleetDob entity,
            fleetOwnerPersonId = fleetOwnerPersonId entity,
            fleetType = fleetType entity,
            gstImageId = gstImageId entity,
            gstNumber = gstNumber_,
            gstNumberDec = gstNumberDec entity,
            isEligibleForSubscription = isEligibleForSubscription entity,
            lienAmount = lienAmount entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            panImageId = panImageId entity,
            panNumber = panNumber_,
            panNumberDec = panNumberDec entity,
            planExpiryDate = planExpiryDate entity,
            prepaidSubscriptionBalance = prepaidSubscriptionBalance entity,
            referredByOperatorId = referredByOperatorId entity,
            registeredAt = registeredAt entity,
            stripeAddress = stripeAddress entity,
            stripeIdNumber = stripeIdNumber_,
            ticketPlaceId = ticketPlaceId entity,
            verified = verified entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' FleetOwnerInformation where
  type UnencryptedItem FleetOwnerInformation = DecryptedFleetOwnerInformation
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data FleetType = RENTAL_FLEET | NORMAL_FLEET | BUSINESS_FLEET deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FleetType)
