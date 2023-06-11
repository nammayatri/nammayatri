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

module Storage.Tabular.DriverOnboarding.AadhaarOtpVerify where

import qualified Domain.Types.DriverOnboarding.AadhaarOtp as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    AadhaarOtpVerifyT sql=aadhaar_otp_verify
      id Text
      driverId PersonTId
      requestId Text
      statusCode Text
      transactionId Text
      requestMessage Text
      createdAt  UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey AadhaarOtpVerifyT where
  type DomainKey AadhaarOtpVerifyT = Id Domain.AadhaarOtpVerify
  fromKey (AadhaarOtpVerifyTKey _id) = Id _id
  toKey (Id id) = AadhaarOtpVerifyTKey id

instance FromTType AadhaarOtpVerifyT Domain.AadhaarOtpVerify where
  fromTType AadhaarOtpVerifyT {..} = do
    return $
      Domain.AadhaarOtpVerify
        { id = Id id,
          driverId = fromKey driverId,
          ..
        }

instance ToTType AadhaarOtpVerifyT Domain.AadhaarOtpVerify where
  toTType Domain.AadhaarOtpVerify {..} =
    AadhaarOtpVerifyT
      { id = getId id,
        driverId = toKey driverId,
        ..
      }
