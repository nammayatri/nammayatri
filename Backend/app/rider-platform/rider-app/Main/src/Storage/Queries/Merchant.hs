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

import Domain.Types.Merchant as DOrg
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Geofencing as Geo
import Kernel.Types.Id
import Kernel.Types.Registry (Subscriber)
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant as BeamM

findById :: MonadFlow m => Id Merchant -> m (Maybe Merchant)
findById (Id merchantId) = findOneWithKV [Se.Is BeamM.id $ Se.Eq merchantId]

findByShortId :: MonadFlow m => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId_ = findOneWithKV [Se.Is BeamM.shortId $ Se.Eq $ getShortId shortId_]

findBySubscriberId :: MonadFlow m => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId subscriberId = findOneWithKV [Se.Is BeamM.subscriberId $ Se.Eq $ getShortId subscriberId]

findAll :: MonadFlow m => m [Merchant]
findAll = findAllWithKV [Se.Is BeamM.id $ Se.Not $ Se.Eq $ getId ""]

update :: MonadFlow m => Merchant -> m ()
update org = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamM.name org.name,
      Se.Set BeamM.gatewayUrl (showBaseUrl org.gatewayUrl),
      Se.Set BeamM.registryUrl (showBaseUrl org.registryUrl),
      Se.Set BeamM.updatedAt now
    ]
    [Se.Is BeamM.id (Se.Eq (getId org.id))]

instance FromTType' BeamM.Merchant Merchant where
  fromTType' BeamM.MerchantT {..} = do
    gwUrl <- parseBaseUrl gatewayUrl
    regUrl <- parseBaseUrl registryUrl
    doBaseUrl <- parseBaseUrl driverOfferBaseUrl
    let geofencingConfig =
          Geo.GeofencingConfig
            { origin = originRestriction,
              destination = destinationRestriction
            }
    pure $
      Just $
        Merchant
          { id = Id id,
            subscriberId = ShortId subscriberId,
            shortId = ShortId shortId,
            name = name,
            city = city,
            country = country,
            bapId = bapId,
            bapUniqueKeyId = bapUniqueKeyId,
            geofencingConfig = geofencingConfig,
            gatewayUrl = gwUrl,
            registryUrl = regUrl,
            driverOfferBaseUrl = doBaseUrl,
            driverOfferApiKey = driverOfferApiKey,
            driverOfferMerchantId = driverOfferMerchantId,
            geoHashPrecisionValue = geoHashPrecisionValue,
            minimumDriverRatesCount = minimumDriverRatesCount,
            signingPublicKey = signingPublicKey,
            cipherText = cipherText,
            distanceWeightage = distanceWeightage,
            signatureExpiry = signatureExpiry,
            createdAt = createdAt,
            timeDiffFromUtc = timeDiffFromUtc,
            updatedAt = updatedAt,
            isAvoidToll = isAvoidToll,
            aadhaarVerificationTryLimit = aadhaarVerificationTryLimit,
            aadhaarKeyExpiryTime = aadhaarKeyExpiryTime,
            mediaFileSizeUpperLimit = mediaFileSizeUpperLimit,
            mediaFileUrlPattern = mediaFileUrlPattern
          }

instance ToTType' BeamM.Merchant Merchant where
  toTType' Merchant {..} = do
    let Geo.GeofencingConfig {..} = geofencingConfig
    BeamM.MerchantT
      { BeamM.id = getId id,
        BeamM.subscriberId = getShortId subscriberId,
        BeamM.shortId = getShortId shortId,
        BeamM.name = name,
        BeamM.city = city,
        BeamM.country = country,
        BeamM.bapId = bapId,
        BeamM.bapUniqueKeyId = bapUniqueKeyId,
        BeamM.originRestriction = origin,
        BeamM.destinationRestriction = destination,
        BeamM.gatewayUrl = showBaseUrl gatewayUrl,
        BeamM.registryUrl = showBaseUrl registryUrl,
        BeamM.driverOfferBaseUrl = showBaseUrl driverOfferBaseUrl,
        BeamM.driverOfferApiKey = driverOfferApiKey,
        BeamM.driverOfferMerchantId = driverOfferMerchantId,
        BeamM.geoHashPrecisionValue = geoHashPrecisionValue,
        BeamM.minimumDriverRatesCount = minimumDriverRatesCount,
        BeamM.signingPublicKey = signingPublicKey,
        BeamM.cipherText = cipherText,
        BeamM.distanceWeightage = distanceWeightage,
        BeamM.signatureExpiry = signatureExpiry,
        BeamM.createdAt = createdAt,
        BeamM.updatedAt = updatedAt,
        BeamM.timeDiffFromUtc = timeDiffFromUtc,
        BeamM.isAvoidToll = isAvoidToll,
        BeamM.aadhaarVerificationTryLimit = aadhaarVerificationTryLimit,
        BeamM.aadhaarKeyExpiryTime = aadhaarKeyExpiryTime,
        BeamM.mediaFileSizeUpperLimit = mediaFileSizeUpperLimit,
        BeamM.mediaFileUrlPattern = mediaFileUrlPattern
      }
