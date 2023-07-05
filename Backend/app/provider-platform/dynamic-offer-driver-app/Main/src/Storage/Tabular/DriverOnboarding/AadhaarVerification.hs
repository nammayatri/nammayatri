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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOnboarding.AadhaarVerification where

import qualified Domain.Types.DriverOnboarding.AadhaarVerification as Domain
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    AadhaarVerificationT sql=aadhaar_verification
      id Text
      driverId PersonTId
      driverName Text
      driverGender Text
      driverDob Text
      driverImage Text Maybe
      aadhaarNumberHash DbHash Maybe
      isVerified Bool
      createdAt  UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey AadhaarVerificationT where
  type DomainKey AadhaarVerificationT = Id Domain.AadhaarVerification
  fromKey (AadhaarVerificationTKey _id) = Id _id
  toKey (Id id) = AadhaarVerificationTKey id

instance FromTType AadhaarVerificationT Domain.AadhaarVerification where
  fromTType AadhaarVerificationT {..} = do
    return $
      Domain.AadhaarVerification
        { id = Id id,
          driverId = fromKey driverId,
          ..
        }

instance ToTType AadhaarVerificationT Domain.AadhaarVerification where
  toTType :: Domain.AadhaarVerification -> AadhaarVerificationT
  toTType Domain.AadhaarVerification {..} =
    AadhaarVerificationT
      { id = getId id,
        driverId = toKey driverId,
        ..
      }
