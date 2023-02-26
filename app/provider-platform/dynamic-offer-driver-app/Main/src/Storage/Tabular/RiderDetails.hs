{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RiderDetails where

import qualified Domain.Types.RiderDetails as Domain
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.DriverReferral (DriverReferralTId)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RiderDetailsT sql=rider_details
      id Text
      mobileCountryCode Text
      mobileNumberEncrypted Text
      mobileNumberHash DbHash
      createdAt UTCTime
      updatedAt UTCTime
      referralCode DriverReferralTId Maybe
      referredByDriver PersonTId Maybe
      referredAt UTCTime Maybe
      hasTakenRide Bool
      Primary id
      deriving Generic
    |]

instance TEntityKey RiderDetailsT where
  type DomainKey RiderDetailsT = Id Domain.RiderDetails
  fromKey (RiderDetailsTKey _id) = Id _id
  toKey (Id id) = RiderDetailsTKey id

instance TType RiderDetailsT Domain.RiderDetails where
  fromTType RiderDetailsT {..} = do
    return $
      Domain.RiderDetails
        { id = Id id,
          referralCode = fromKey <$> referralCode,
          mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
          referredByDriver = fromKey <$> referredByDriver,
          ..
        }
  toTType Domain.RiderDetails {..} =
    RiderDetailsT
      { id = getId id,
        referralCode = toKey <$> referralCode,
        mobileNumberEncrypted = unEncrypted mobileNumber.encrypted,
        mobileNumberHash = mobileNumber.hash,
        referredByDriver = fmap toKey referredByDriver,
        ..
      }
