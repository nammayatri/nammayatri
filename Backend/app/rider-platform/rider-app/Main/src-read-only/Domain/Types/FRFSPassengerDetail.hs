{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSPassengerDetail where

import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Seat
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSPassengerDetailE e = FRFSPassengerDetail
  { age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking),
    dropOffPointPlaceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gender :: Domain.Types.Person.Gender,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail,
    idProofLookupId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    idProofNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    isChild :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupPointPlaceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    seatId :: Kernel.Types.Id.Id Domain.Types.Seat.Seat,
    seatLabel :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type FRFSPassengerDetail = FRFSPassengerDetailE 'AsEncrypted

type DecryptedFRFSPassengerDetail = FRFSPassengerDetailE 'AsUnencrypted

instance EncryptedItem FRFSPassengerDetail where
  type Unencrypted FRFSPassengerDetail = (DecryptedFRFSPassengerDetail, HashSalt)
  encryptItem (entity, salt) = do
    idProofNumber_ <- encryptItem $ (,salt) <$> idProofNumber entity
    pure
      FRFSPassengerDetail
        { age = age entity,
          bookingId = bookingId entity,
          dropOffPointPlaceId = dropOffPointPlaceId entity,
          gender = gender entity,
          id = id entity,
          idProofLookupId = idProofLookupId entity,
          idProofNumber = idProofNumber_,
          isChild = isChild entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          name = name entity,
          pickupPointPlaceId = pickupPointPlaceId entity,
          quoteId = quoteId entity,
          seatId = seatId entity,
          seatLabel = seatLabel entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    idProofNumber_ <- fmap fst <$> decryptItem (idProofNumber entity)
    pure
      ( FRFSPassengerDetail
          { age = age entity,
            bookingId = bookingId entity,
            dropOffPointPlaceId = dropOffPointPlaceId entity,
            gender = gender entity,
            id = id entity,
            idProofLookupId = idProofLookupId entity,
            idProofNumber = idProofNumber_,
            isChild = isChild entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            name = name entity,
            pickupPointPlaceId = pickupPointPlaceId entity,
            quoteId = quoteId entity,
            seatId = seatId entity,
            seatLabel = seatLabel entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' FRFSPassengerDetail where
  type UnencryptedItem FRFSPassengerDetail = DecryptedFRFSPassengerDetail
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
