{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetBookingInformation where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetBookingInformationE e = FleetBookingInformation
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bookedSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bookingId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    customerMobileNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOwnerId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    id :: Kernel.Types.Id.Id Domain.Types.FleetBookingInformation.FleetBookingInformation,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    paymentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    placeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketBookingServiceShortId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketBookingShortId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketPlaceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    visitDate :: Kernel.Prelude.Maybe Data.Time.Day
  }
  deriving (Generic)

type FleetBookingInformation = FleetBookingInformationE 'AsEncrypted

type DecryptedFleetBookingInformation = FleetBookingInformationE 'AsUnencrypted

instance EncryptedItem FleetBookingInformation where
  type Unencrypted FleetBookingInformation = (DecryptedFleetBookingInformation, HashSalt)
  encryptItem (entity, salt) = do
    customerMobileNumber_ <- encryptItem $ (,salt) <$> customerMobileNumber entity
    pure
      FleetBookingInformation
        { amount = amount entity,
          bookedSeats = bookedSeats entity,
          bookingId = bookingId entity,
          createdAt = createdAt entity,
          customerMobileNumber = customerMobileNumber_,
          customerName = customerName entity,
          fleetOwnerId = fleetOwnerId entity,
          id = id entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          paymentMethod = paymentMethod entity,
          personId = personId entity,
          placeName = placeName entity,
          serviceId = serviceId entity,
          serviceName = serviceName entity,
          status = status entity,
          ticketBookingServiceShortId = ticketBookingServiceShortId entity,
          ticketBookingShortId = ticketBookingShortId entity,
          ticketPlaceId = ticketPlaceId entity,
          updatedAt = updatedAt entity,
          vehicleNo = vehicleNo entity,
          visitDate = visitDate entity
        }
  decryptItem entity = do
    customerMobileNumber_ <- fmap fst <$> decryptItem (customerMobileNumber entity)
    pure
      ( FleetBookingInformation
          { amount = amount entity,
            bookedSeats = bookedSeats entity,
            bookingId = bookingId entity,
            createdAt = createdAt entity,
            customerMobileNumber = customerMobileNumber_,
            customerName = customerName entity,
            fleetOwnerId = fleetOwnerId entity,
            id = id entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            paymentMethod = paymentMethod entity,
            personId = personId entity,
            placeName = placeName entity,
            serviceId = serviceId entity,
            serviceName = serviceName entity,
            status = status entity,
            ticketBookingServiceShortId = ticketBookingServiceShortId entity,
            ticketBookingShortId = ticketBookingShortId entity,
            ticketPlaceId = ticketPlaceId entity,
            updatedAt = updatedAt entity,
            vehicleNo = vehicleNo entity,
            visitDate = visitDate entity
          },
        ""
      )

instance EncryptedItem' FleetBookingInformation where
  type UnencryptedItem FleetBookingInformation = DecryptedFleetBookingInformation
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
