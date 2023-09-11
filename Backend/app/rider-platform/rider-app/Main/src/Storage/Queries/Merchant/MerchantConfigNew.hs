{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.MerchantConfigNew where

import Domain.Types.Merchant
import Domain.Types.Merchant.MerchantConfigNew
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Geofencing as Geo
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.MerchantConfigNew as BeamMC

findByMerchantId :: MonadFlow m => Id Merchant -> m (Maybe MerchantConfigNew)
findByMerchantId (Id merchantId) = findOneWithKV [Se.Is BeamMC.merchantId $ Se.Eq merchantId]

findAll :: MonadFlow m => m [MerchantConfigNew]
findAll = findAllWithKV [Se.Is BeamMC.merchantId $ Se.Not $ Se.Eq $ getId ""]

updateConfig :: MonadFlow m => MerchantConfigNew -> m ()
updateConfig cfg = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamMC.registryUrl (showBaseUrl cfg.registryUrl),
      Se.Set BeamMC.gatewayUrl (showBaseUrl cfg.gatewayUrl),
      Se.Set BeamMC.updatedAt now
    ]
    [Se.Is BeamMC.merchantId (Se.Eq (getId cfg.merchantId))]

instance FromTType' BeamMC.MerchantConfigNew MerchantConfigNew where
  fromTType' BeamMC.MerchantConfigNewT {..} = do
    regUrl <- parseBaseUrl registryUrl
    gwUrl <- parseBaseUrl gatewayUrl
    doBaseUrl <- parseBaseUrl driverOfferBaseUrl
    let geofencingConfig =
          Geo.GeofencingConfig
            { origin = originRestriction,
              destination = destinationRestriction
            }
    pure $
      Just $
        MerchantConfigNew
          { merchantId = Id merchantId,
            geofencingConfig = geofencingConfig,
            registryUrl = regUrl,
            gatewayUrl = gwUrl,
            driverOfferBaseUrl = doBaseUrl,
            driverOfferApiKey = driverOfferApiKey,
            driverOfferMerchantId = driverOfferMerchantId,
            city = city,
            name = name,
            country = country,
            geoHashPrecisionValue = geoHashPrecisionValue,
            distanceWeightage = distanceWeightage,
            minimumDriverRatesCount = minimumDriverRatesCount,
            createdAt = createdAt,
            updatedAt = updatedAt,
            timeDiffFromUtc = timeDiffFromUtc,
            dirCacheSlot = dirCacheSlot
          }

instance ToTType' BeamMC.MerchantConfigNew MerchantConfigNew where
  toTType' MerchantConfigNew {..} = do
    let Geo.GeofencingConfig {..} = geofencingConfig
    BeamMC.MerchantConfigNewT
      { BeamMC.merchantId = getId merchantId,
        BeamMC.name = name,
        BeamMC.originRestriction = origin,
        BeamMC.destinationRestriction = destination,
        BeamMC.registryUrl = showBaseUrl registryUrl,
        BeamMC.gatewayUrl = showBaseUrl gatewayUrl,
        BeamMC.driverOfferBaseUrl = showBaseUrl driverOfferBaseUrl,
        BeamMC.driverOfferApiKey = driverOfferApiKey,
        BeamMC.driverOfferMerchantId = driverOfferMerchantId,
        BeamMC.city = city,
        BeamMC.country = country,
        BeamMC.geoHashPrecisionValue = geoHashPrecisionValue,
        BeamMC.distanceWeightage = distanceWeightage,
        BeamMC.minimumDriverRatesCount = minimumDriverRatesCount,
        BeamMC.createdAt = createdAt,
        BeamMC.updatedAt = updatedAt,
        BeamMC.timeDiffFromUtc = timeDiffFromUtc,
        BeamMC.dirCacheSlot = dirCacheSlot
      }
