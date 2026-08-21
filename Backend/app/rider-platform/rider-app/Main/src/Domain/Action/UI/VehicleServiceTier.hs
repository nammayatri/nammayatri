module Domain.Action.UI.VehicleServiceTier
  ( getVehicleServiceTiers,
    clearCache,
  )
where

import qualified API.Types.UI.VehicleServiceTier as API
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson
import Tools.Error

cacheExpTime :: Hedis.ExpirationTime
cacheExpTime = 3 * 60 * 60

getVehicleServiceTiers ::
  ( Maybe (Id DPerson.Person),
    Id DM.Merchant
  ) ->
  Flow [API.VehicleServiceTierAPIEntity]
getVehicleServiceTiers (mbPersonId, merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchantOpCity <-
    CQMOC.findById person.merchantOperatingCityId
      >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist $ "merchantId:- " <> merchantId.getId)
  let cacheKey = makeCacheKey merchantOpCity.id
  tiers <-
    Hedis.safeGet cacheKey >>= \case
      Just cached -> pure cached
      Nothing -> do
        fetched <- CallBPPInternal.getVehicleServiceTiers merchant merchantOpCity.city
        -- Never cache an empty catalogue. A BPP that is reachable but not yet seeded for this city
        -- would otherwise pin an empty response for the whole TTL, with no way out but clearCache.
        if null fetched
          then logWarning $ "BPP returned no vehicle service tiers for merchantOpCityId " <> merchantOpCity.id.getId
          else Hedis.setExp cacheKey fetched cacheExpTime
        pure fetched
  let result = map mkVehicleServiceTierAPIEntity tiers
  logInfo $ "VehicleServiceTiers response for merchantOpCityId " <> merchantOpCity.id.getId <> ": " <> show result
  pure result

mkVehicleServiceTierAPIEntity :: CallBPPInternal.VehicleServiceTierInfo -> API.VehicleServiceTierAPIEntity
mkVehicleServiceTierAPIEntity tier =
  API.VehicleServiceTierAPIEntity
    { serviceTierType = tier.serviceTierType,
      serviceTierName = tier.serviceTierName,
      vehicleIconUrl = tier.vehicleIconUrl
    }

clearCache :: Id DMOC.MerchantOperatingCity -> Flow ()
clearCache mocId = Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeCacheKey mocId)

makeCacheKey :: Id DMOC.MerchantOperatingCity -> Text
makeCacheKey mocId = "CachedQueries:VehicleServiceTiers:MerchantOpCityId-" <> mocId.getId
