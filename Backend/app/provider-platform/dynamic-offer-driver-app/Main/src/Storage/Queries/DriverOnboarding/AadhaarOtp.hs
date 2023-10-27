{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.AadhaarOtp where

import Domain.Types.DriverOnboarding.AadhaarOtp
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.AadhaarOtpReq as BeamAOR
import qualified Storage.Beam.DriverOnboarding.AadhaarOtpVerify as BeamAOV

createForGenerate :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => AadhaarOtpReq -> m ()
createForGenerate = createWithKV

createForVerify :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => AadhaarOtpVerify -> m ()
createForVerify = createWithKV

deleteByPersonIdForGenerate :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteByPersonIdForGenerate personId = deleteWithKV [Se.Is BeamAOR.driverId (Se.Eq (getId personId))]

deleteByPersonIdForVerify :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteByPersonIdForVerify personId = deleteWithKV [Se.Is BeamAOV.driverId (Se.Eq (getId personId))]

instance FromTType' BeamAOR.AadhaarOtpReq AadhaarOtpReq where
  fromTType' BeamAOR.AadhaarOtpReqT {..} = do
    pure $
      Just
        AadhaarOtpReq
          { id = Id id,
            driverId = Id driverId,
            requestId = requestId,
            statusCode = statusCode,
            transactionId = transactionId,
            requestMessage = requestMessage,
            createdAt = createdAt
          }

instance ToTType' BeamAOR.AadhaarOtpReq AadhaarOtpReq where
  toTType' AadhaarOtpReq {..} =
    BeamAOR.AadhaarOtpReqT
      { BeamAOR.id = getId id,
        BeamAOR.driverId = getId driverId,
        BeamAOR.requestId = requestId,
        BeamAOR.statusCode = statusCode,
        BeamAOR.transactionId = transactionId,
        BeamAOR.requestMessage = requestMessage,
        BeamAOR.createdAt = createdAt
      }

instance FromTType' BeamAOV.AadhaarOtpVerify AadhaarOtpVerify where
  fromTType' BeamAOV.AadhaarOtpVerifyT {..} = do
    pure $
      Just
        AadhaarOtpVerify
          { id = Id id,
            driverId = Id driverId,
            requestId = requestId,
            statusCode = statusCode,
            transactionId = transactionId,
            requestMessage = requestMessage,
            createdAt = createdAt
          }

instance ToTType' BeamAOV.AadhaarOtpVerify AadhaarOtpVerify where
  toTType' AadhaarOtpVerify {..} =
    BeamAOV.AadhaarOtpVerifyT
      { BeamAOV.id = getId id,
        BeamAOV.driverId = getId driverId,
        BeamAOV.requestId = requestId,
        BeamAOV.statusCode = statusCode,
        BeamAOV.transactionId = transactionId,
        BeamAOV.requestMessage = requestMessage,
        BeamAOV.createdAt = createdAt
      }
