{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RiderDetails where

import Data.Aeson
import qualified Domain.Types.DriverReferral
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RiderDetailsE e = RiderDetails
  { bapId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cancellationDueRides :: Kernel.Prelude.Int,
    cancellationDues :: Kernel.Types.Common.HighPrecMoney,
    cancellationDuesPaid :: Kernel.Types.Common.HighPrecMoney,
    cancelledRides :: Kernel.Prelude.Int,
    completedRides :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    disputeChancesUsed :: Kernel.Prelude.Int,
    firstRideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hasTakenValidRide :: Kernel.Prelude.Bool,
    hasTakenValidRideAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails,
    isDeviceIdExists :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isFlagConfirmed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    nightSafetyChecks :: Kernel.Prelude.Bool,
    noOfTimesCanellationDuesPaid :: Kernel.Prelude.Int,
    noOfTimesWaiveOffUsed :: Kernel.Prelude.Int,
    otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutFlagReason :: Kernel.Prelude.Maybe Domain.Types.RiderDetails.PayoutFlagReason,
    referralCode :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.DriverReferral.DriverReferral),
    referredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    referredByDriver :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    totalBookings :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    validCancellations :: Kernel.Prelude.Int,
    waivedOffAmount :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic)

type RiderDetails = RiderDetailsE 'AsEncrypted

type DecryptedRiderDetails = RiderDetailsE 'AsUnencrypted

instance EncryptedItem RiderDetails where
  type Unencrypted RiderDetails = (DecryptedRiderDetails, HashSalt)
  encryptItem (entity, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber entity, salt)
    pure
      RiderDetails
        { bapId = bapId entity,
          cancellationDueRides = cancellationDueRides entity,
          cancellationDues = cancellationDues entity,
          cancellationDuesPaid = cancellationDuesPaid entity,
          cancelledRides = cancelledRides entity,
          completedRides = completedRides entity,
          createdAt = createdAt entity,
          currency = currency entity,
          disputeChancesUsed = disputeChancesUsed entity,
          firstRideId = firstRideId entity,
          hasTakenValidRide = hasTakenValidRide entity,
          hasTakenValidRideAt = hasTakenValidRideAt entity,
          id = id entity,
          isDeviceIdExists = isDeviceIdExists entity,
          isFlagConfirmed = isFlagConfirmed entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          mobileCountryCode = mobileCountryCode entity,
          mobileNumber = mobileNumber_,
          nightSafetyChecks = nightSafetyChecks entity,
          noOfTimesCanellationDuesPaid = noOfTimesCanellationDuesPaid entity,
          noOfTimesWaiveOffUsed = noOfTimesWaiveOffUsed entity,
          otpCode = otpCode entity,
          payoutFlagReason = payoutFlagReason entity,
          referralCode = referralCode entity,
          referredAt = referredAt entity,
          referredByDriver = referredByDriver entity,
          totalBookings = totalBookings entity,
          updatedAt = updatedAt entity,
          validCancellations = validCancellations entity,
          waivedOffAmount = waivedOffAmount entity
        }
  decryptItem entity = do
    mobileNumber_ <- fst <$> decryptItem (mobileNumber entity)
    pure
      ( RiderDetails
          { bapId = bapId entity,
            cancellationDueRides = cancellationDueRides entity,
            cancellationDues = cancellationDues entity,
            cancellationDuesPaid = cancellationDuesPaid entity,
            cancelledRides = cancelledRides entity,
            completedRides = completedRides entity,
            createdAt = createdAt entity,
            currency = currency entity,
            disputeChancesUsed = disputeChancesUsed entity,
            firstRideId = firstRideId entity,
            hasTakenValidRide = hasTakenValidRide entity,
            hasTakenValidRideAt = hasTakenValidRideAt entity,
            id = id entity,
            isDeviceIdExists = isDeviceIdExists entity,
            isFlagConfirmed = isFlagConfirmed entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            mobileCountryCode = mobileCountryCode entity,
            mobileNumber = mobileNumber_,
            nightSafetyChecks = nightSafetyChecks entity,
            noOfTimesCanellationDuesPaid = noOfTimesCanellationDuesPaid entity,
            noOfTimesWaiveOffUsed = noOfTimesWaiveOffUsed entity,
            otpCode = otpCode entity,
            payoutFlagReason = payoutFlagReason entity,
            referralCode = referralCode entity,
            referredAt = referredAt entity,
            referredByDriver = referredByDriver entity,
            totalBookings = totalBookings entity,
            updatedAt = updatedAt entity,
            validCancellations = validCancellations entity,
            waivedOffAmount = waivedOffAmount entity
          },
        ""
      )

instance EncryptedItem' RiderDetails where
  type UnencryptedItem RiderDetails = DecryptedRiderDetails
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data PayoutFlagReason
  = ExceededMaxReferral
  | MinRideDistanceInvalid
  | MinPickupDistanceInvalid
  | CustomerExistAsDriver
  | MultipleDeviceIdExists
  | RideConstraintInvalid
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutFlagReason)
