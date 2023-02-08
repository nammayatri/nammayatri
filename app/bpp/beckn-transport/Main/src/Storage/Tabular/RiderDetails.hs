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
          mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
          ..
        }
  toTType Domain.RiderDetails {..} =
    RiderDetailsT
      { id = getId id,
        mobileNumberEncrypted = unEncrypted mobileNumber.encrypted,
        mobileNumberHash = mobileNumber.hash,
        ..
      }
