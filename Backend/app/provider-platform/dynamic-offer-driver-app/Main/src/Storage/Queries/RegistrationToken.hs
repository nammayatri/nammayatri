{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RegistrationToken where

import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.RegistrationToken as DRT
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as BeamRT
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DRT.RegistrationToken -> m ()
create = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RegistrationToken -> m (Maybe RegistrationToken)
findById (Id registrationTokenId) = findOneWithKV [Se.Is BeamRT.id $ Se.Eq registrationTokenId]

setVerified :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RegistrationToken -> m ()
setVerified (Id rtId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamRT.verified True,
      Se.Set BeamRT.updatedAt now
    ]
    [Se.Is BeamRT.id (Se.Eq rtId)]

findByToken :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => RegToken -> m (Maybe RegistrationToken)
findByToken token = findOneWithKV [Se.Is BeamRT.token $ Se.Eq token]

updateAttempts :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Int -> Id RegistrationToken -> m ()
updateAttempts attempts (Id rtId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamRT.attempts attempts,
      Se.Set BeamRT.updatedAt now
    ]
    [Se.Is BeamRT.id (Se.Eq rtId)]

deleteByPersonId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.Is BeamRT.entityId (Se.Eq personId)]

deleteByPersonIdExceptNew :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id RegistrationToken -> m ()
deleteByPersonIdExceptNew (Id personId) (Id newRT) = deleteWithKV [Se.And [Se.Is BeamRT.entityId (Se.Eq personId), Se.Is BeamRT.id (Se.Not $ Se.Eq newRT)]]

findAllByPersonId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m [RegistrationToken]
findAllByPersonId personId = findAllWithKV [Se.Is BeamRT.entityId $ Se.Eq $ getId personId]

findLatestByPersonId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Maybe RegistrationToken)
findLatestByPersonId personId = findOneWithKV [Se.And [Se.Is BeamRT.entityId $ Se.Eq $ getId personId, Se.Is BeamRT.verified $ Se.Eq True]]

updateCityInfoById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RegistrationToken -> Id MerchantOperatingCity -> m ()
updateCityInfoById (Id registrationToken) (Id merchantOperatingCityId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamRT.merchantOperatingCityId (Just merchantOperatingCityId),
      Se.Set BeamRT.updatedAt now
    ]
    [Se.Is BeamRT.id (Se.Eq registrationToken)]

getAlternateNumberAttempts :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m Int
getAlternateNumberAttempts (Id personId) = findOneWithKV [Se.Is BeamRT.entityId $ Se.Eq personId] <&> maybe 5 DRT.attempts

instance FromTType' BeamRT.RegistrationToken RegistrationToken where
  fromTType' BeamRT.RegistrationTokenT {..} = do
    merchantOpCityId <-
      case merchantOperatingCityId of
        Just opCityId -> return $ Id opCityId
        Nothing -> do
          merchant <- CQM.findById (Id merchantId) >>= fromMaybeM (MerchantNotFound merchantId)
          CQMOC.getMerchantOpCityId Nothing merchant Nothing
    pure $
      Just
        RegistrationToken
          { id = Id id,
            token = token,
            attempts = attempts,
            authMedium = authMedium,
            authType = authType,
            authValueHash = authValueHash,
            verified = verified,
            authExpiry = authExpiry,
            tokenExpiry = tokenExpiry,
            entityId = entityId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId.getId,
            entityType = entityType,
            createdAt = createdAt,
            updatedAt = updatedAt,
            info = info,
            alternateNumberAttempts = alternateNumberAttempts
          }

instance ToTType' BeamRT.RegistrationToken RegistrationToken where
  toTType' RegistrationToken {..} = do
    BeamRT.RegistrationTokenT
      { BeamRT.id = getId id,
        BeamRT.token = token,
        BeamRT.attempts = attempts,
        BeamRT.authMedium = authMedium,
        BeamRT.authType = authType,
        BeamRT.authValueHash = authValueHash,
        BeamRT.verified = verified,
        BeamRT.authExpiry = authExpiry,
        BeamRT.tokenExpiry = tokenExpiry,
        BeamRT.entityId = entityId,
        BeamRT.merchantId = merchantId,
        BeamRT.entityType = entityType,
        BeamRT.createdAt = createdAt,
        BeamRT.updatedAt = updatedAt,
        BeamRT.info = info,
        BeamRT.alternateNumberAttempts = alternateNumberAttempts,
        BeamRT.merchantOperatingCityId = Just merchantOperatingCityId
      }
