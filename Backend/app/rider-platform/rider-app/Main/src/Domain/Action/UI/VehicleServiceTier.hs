module Domain.Action.UI.VehicleServiceTier
  ( module Domain.Action.UI.VehicleServiceTier,
    CallBPPInternal.VehicleServiceTierInfo (..),
  )
where

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

getVehicleServiceTiers :: Id DM.Merchant -> Id DPerson.Person -> Flow [CallBPPInternal.VehicleServiceTierInfo]
getVehicleServiceTiers merchantId personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchantOpCity <-
    CQMOC.findById person.merchantOperatingCityId
      >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist $ "merchantId:- " <> merchantId.getId)
  let cacheKey = makeCacheKey merchantOpCity.id
  result <-
    Hedis.safeGet cacheKey >>= \case
      Just cached -> pure cached
      Nothing -> do
        tiers <- CallBPPInternal.getVehicleServiceTiers merchant merchantOpCity.city
        -- Never cache an empty catalogue. A BPP that is reachable but not yet seeded for this city
        -- would otherwise pin an empty response for the whole TTL, with no way out but clearCache.
        if null tiers
          then logWarning $ "BPP returned no vehicle service tiers for merchantOpCityId " <> merchantOpCity.id.getId
          else Hedis.setExp cacheKey tiers cacheExpTime
        pure tiers
  logInfo $ "VehicleServiceTiers response for merchantOpCityId " <> merchantOpCity.id.getId <> ": " <> show result
  pure result

clearCache :: Id DMOC.MerchantOperatingCity -> Flow ()
clearCache mocId = Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeCacheKey mocId)

makeCacheKey :: Id DMOC.MerchantOperatingCity -> Text
makeCacheKey mocId = "CachedQueries:VehicleServiceTiers:MerchantOpCityId-" <> mocId.getId
