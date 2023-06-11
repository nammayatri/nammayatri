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

module Storage.Tabular.DriverOnboarding.AadhaarOtpReq where

import qualified Domain.Types.DriverOnboarding.AadhaarOtp as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    AadhaarOtpReqT sql=aadhaar_otp_req
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

instance TEntityKey AadhaarOtpReqT where
  type DomainKey AadhaarOtpReqT = Id Domain.AadhaarOtpReq
  fromKey (AadhaarOtpReqTKey _id) = Id _id
  toKey (Id id) = AadhaarOtpReqTKey id

instance FromTType AadhaarOtpReqT Domain.AadhaarOtpReq where
  fromTType AadhaarOtpReqT {..} = do
    return $
      Domain.AadhaarOtpReq
        { id = Id id,
          driverId = fromKey driverId,
          ..
        }

instance ToTType AadhaarOtpReqT Domain.AadhaarOtpReq where
  toTType Domain.AadhaarOtpReq {..} =
    AadhaarOtpReqT
      { id = getId id,
        driverId = toKey driverId,
        ..
      }
