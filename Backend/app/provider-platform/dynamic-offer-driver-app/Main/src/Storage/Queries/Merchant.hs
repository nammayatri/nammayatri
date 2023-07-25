{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DM
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), findAllWithKV, findOneWithKV, findOneWithKvInReplica, updateOneWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant as BeamM

-- findById :: Transactionable m => Id Merchant -> m (Maybe Merchant)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id Merchant -> m (Maybe Merchant)
findById (Id merchantId) = findOneWithKV [Se.Is BeamM.id $ Se.Eq merchantId]

-- findBySubscriberId :: Transactionable m => ShortId Subscriber -> m (Maybe Merchant)
-- findBySubscriberId subscriberId = Esq.findOne $ do
--   org <- from $ table @MerchantT
--     org ^. MerchantSubscriberId ==. val (subscriberId.getShortId)
--   return org

findBySubscriberId :: (L.MonadFlow m, Log m) => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId (ShortId subscriberId) = findOneWithKV [Se.Is BeamM.subscriberId $ Se.Eq subscriberId]

-- findByShortId :: Transactionable m => ShortId Merchant -> m (Maybe Merchant)
-- findByShortId shortId = Esq.findOne $ do
--   org <- from $ table @MerchantT
--     org ^. MerchantShortId ==. val (shortId.getShortId)
--   return org

findByShortId :: (L.MonadFlow m, Log m) => ShortId Merchant -> m (Maybe Merchant)
findByShortId (ShortId shortId) = findOneWithKV [Se.Is BeamM.shortId $ Se.Eq shortId]

findByShortIdInReplica :: (L.MonadFlow m, Log m) => ShortId Merchant -> m (Maybe Merchant)
findByShortIdInReplica (ShortId shortId) = findOneWithKvInReplica [Se.Is BeamM.shortId $ Se.Eq shortId]

-- loadAllProviders :: Transactionable m => m [Merchant]
-- loadAllProviders =
--   Esq.findAll $ do
--     org <- from $ table @MerchantT
--       org ^. MerchantStatus ==. val DM.APPROVED
--         &&. org ^. MerchantEnabled
--     return org

loadAllProviders :: (L.MonadFlow m, Log m) => m [Merchant]
loadAllProviders = do
  findAllWithKV [Se.And [Se.Is BeamM.status $ Se.Eq DM.APPROVED, Se.Is BeamM.enabled $ Se.Eq True]]

-- findAll :: Transactionable m => m [Merchant]
-- findAll =
--   Esq.findAll $ do from $ table @MerchantT

findAll :: (L.MonadFlow m, Log m) => m [Merchant]
findAll = findAllWithKV [Se.Is BeamM.id $ Se.Not $ Se.Eq $ getId ""]

-- update :: Merchant -> SqlDB ()
-- update org = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ MerchantName =. val org.name,
--         MerchantDescription =. val org.description,
--         MerchantHeadCount =. val org.headCount,
--         MerchantEnabled =. val org.enabled,
--         MerchantUpdatedAt =. val now,
--         MerchantFromTime =. val org.fromTime
--       ]

update :: (L.MonadFlow m, MonadTime m, Log m) => Merchant -> m ()
update org = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamM.name org.name,
      Se.Set BeamM.description org.description,
      Se.Set BeamM.headCount org.headCount,
      Se.Set BeamM.enabled org.enabled,
      Se.Set BeamM.updatedAt now,
      Se.Set BeamM.fromTime org.fromTime
    ]
    [Se.Is BeamM.id (Se.Eq (getId org.id))]

instance FromTType' BeamM.Merchant Merchant where
  fromTType' BeamM.MerchantT {..} = do
    pure $
      Just
        Merchant
          { id = Id id,
            name = name,
            description = description,
            subscriberId = ShortId subscriberId,
            uniqueKeyId = uniqueKeyId,
            shortId = ShortId shortId,
            city = city,
            mobileNumber = mobileNumber,
            mobileCountryCode = mobileCountryCode,
            gstin = gstin,
            fromTime = fromTime,
            toTime = toTime,
            headCount = headCount,
            geoHashPrecisionValue = geoHashPrecisionValue,
            aadhaarVerificationRequired = aadhaarVerificationRequired,
            status = status,
            verified = verified,
            enabled = enabled,
            internalApiKey = internalApiKey,
            createdAt = createdAt,
            updatedAt = updatedAt,
            geofencingConfig = GeofencingConfig originRestriction destinationRestriction,
            info = info
          }

instance ToTType' BeamM.Merchant Merchant where
  toTType' Merchant {..} = do
    BeamM.MerchantT
      { BeamM.id = getId id,
        BeamM.name = name,
        BeamM.description = description,
        BeamM.subscriberId = getShortId subscriberId,
        BeamM.uniqueKeyId = uniqueKeyId,
        BeamM.shortId = getShortId shortId,
        BeamM.city = city,
        BeamM.mobileNumber = mobileNumber,
        BeamM.mobileCountryCode = mobileCountryCode,
        BeamM.gstin = gstin,
        BeamM.fromTime = fromTime,
        BeamM.toTime = toTime,
        BeamM.headCount = headCount,
        BeamM.geoHashPrecisionValue = geoHashPrecisionValue,
        BeamM.aadhaarVerificationRequired = aadhaarVerificationRequired,
        BeamM.status = status,
        BeamM.verified = verified,
        BeamM.enabled = enabled,
        BeamM.internalApiKey = internalApiKey,
        BeamM.createdAt = createdAt,
        BeamM.updatedAt = updatedAt,
        BeamM.originRestriction = origin geofencingConfig,
        BeamM.destinationRestriction = destination geofencingConfig,
        BeamM.info = info
      }
