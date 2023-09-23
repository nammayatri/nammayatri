{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.AadhaarVerification.AadhaarOtp where

import Domain.Types.AadhaarVerification.AadhaarOtp
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.AadhaarVerification.AadhaarOtpReq as BeamAOR
import qualified Storage.Beam.AadhaarVerification.AadhaarOtpVerify as BeamAOV

createForGenerate :: MonadFlow m => AadhaarOtpReq -> m ()
createForGenerate = createWithKV

createForVerify :: MonadFlow m => AadhaarOtpVerify -> m ()
createForVerify = createWithKV

deleteByPersonIdForGenerate :: MonadFlow m => Id Person -> m ()
deleteByPersonIdForGenerate personId = deleteWithKV [Se.Is BeamAOR.personId (Se.Eq (getId personId))]

deleteByPersonIdForVerify :: MonadFlow m => Id Person -> m ()
deleteByPersonIdForVerify personId = deleteWithKV [Se.Is BeamAOV.personId (Se.Eq (getId personId))]

instance FromTType' BeamAOR.AadhaarOtpReq AadhaarOtpReq where
  fromTType' BeamAOR.AadhaarOtpReqT {..} = do
    pure $
      Just
        AadhaarOtpReq
          { id = Id id,
            personId = Id personId,
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
        BeamAOR.personId = getId personId,
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
            personId = Id personId,
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
        BeamAOV.personId = getId personId,
        BeamAOV.requestId = requestId,
        BeamAOV.statusCode = statusCode,
        BeamAOV.transactionId = transactionId,
        BeamAOV.requestMessage = requestMessage,
        BeamAOV.createdAt = createdAt
      }
