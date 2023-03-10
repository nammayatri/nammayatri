{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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

instance FromTType RiderDetailsT Domain.RiderDetails where
  fromTType RiderDetailsT {..} = do
    return $
      Domain.RiderDetails
        { id = Id id,
          mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
          ..
        }

instance ToTType RiderDetailsT Domain.RiderDetails where
  toTType Domain.RiderDetails {..} =
    RiderDetailsT
      { id = getId id,
        mobileNumberEncrypted = unEncrypted mobileNumber.encrypted,
        mobileNumberHash = mobileNumber.hash,
        ..
      }
