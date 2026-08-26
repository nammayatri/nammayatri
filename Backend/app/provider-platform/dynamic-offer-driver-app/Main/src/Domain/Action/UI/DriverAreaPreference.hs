-- | Driver-facing CRUD for the "preferred area" preference.
--
-- The driver picks their preferred area in exactly one of two ways -- painting
-- geohash cells on the map, or dropping a pin and growing a radius. The two are
-- mutually exclusive, which is enforced by the write path clearing the other
-- method's tags in the same update.
--
-- Storage: entirely tags on 'person.driverTag' (see 'SharedLogic.DriverPool.AreaPreference'
-- for the encoding)
module Domain.Action.UI.DriverAreaPreference
  ( getDriverAreaPreferenceGetInfo,
    postDriverAreaPreferenceUpdateInfo,
    getDriverAreaPreferenceList,
  )
where

import qualified API.Types.UI.DriverAreaPreference as APIT
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (any, id, length, map, notElem, null)
import Kernel.Prelude
import Kernel.Types.Common (Meters (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPool.AreaPreference as AreaPref
import qualified Storage.CachedQueries.GeohashArea as CQGA
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import Tools.Error

getDriverAreaPreferenceGetInfo ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow APIT.AreaPreferenceInfoRes
  )
getDriverAreaPreferenceGetInfo (mbPersonId, _, merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  minCells <- getMinCells merchantOpCityId
  pure $ buildInfoRes minCells person.driverTag

postDriverAreaPreferenceUpdateInfo ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    APIT.AreaPreferenceUpdateReq ->
    Environment.Flow APIT.AreaPreferenceInfoRes
  )
postDriverAreaPreferenceUpdateInfo (mbPersonId, _, merchantOpCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  minCells <- getMinCells merchantOpCityId
  mbPref <- mapM (validateSelection merchantOpCityId minCells) req.selection
  let existingTags = fromMaybe [] person.driverTag
      clearedNames = AreaPref.areaPreferenceTagNamesToClear existingTags
      retained = filter (\t -> maybe True (`notElem` clearedNames) (tagNameOf t)) existingTags
      newTags = [LYT.TagNameValueExpiry (name <> "#" <> value) | (name, value) <- maybe [] AreaPref.tagNamesForSelection mbPref]
      updatedTags = retained <> newTags
  QPerson.updateDriverTag (if null updatedTags then Nothing else Just updatedTags) personId
  pure $ buildInfoRes minCells (Just updatedTags)
  where
    tagNameOf (LYT.TagNameValueExpiry txt) = case T.splitOn "#" txt of
      (name : _) -> Just name
      _ -> Nothing

getDriverAreaPreferenceList ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow [APIT.GeohashAreaItem]
  )
getDriverAreaPreferenceList (_, _, merchantOpCityId) = do
  areas <- CQGA.findAllByMerchantOperatingCity merchantOpCityId
  pure [APIT.GeohashAreaItem {geohash = a.geohash, areaName = a.areaName} | a <- areas]

getMinCells ::
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Environment.Flow Int
getMinCells merchantOpCityId = do
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  pure $ fromMaybe AreaPref.minSelectableCellsDefault transporterConfig.areaPreferenceMinCells

-- | Reject selections that could never match anything, rather than silently
-- storing a preference that makes the driver invisible to every ride.
validateSelection ::
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Int ->
  APIT.AreaPreferenceSelection ->
  Environment.Flow AreaPref.DriverAreaPreference
validateSelection merchantOpCityId minCells = \case
  APIT.SelectGeohashCells rawCells -> do
    let cells = filter (not . T.null) (map T.strip rawCells)
    when (length cells < minCells) $
      throwError (InvalidRequest $ "At least " <> show minCells <> " geohash cells must be selected")
    configuredAreas <- CQGA.findAllByMerchantOperatingCity merchantOpCityId
    let areaNameByGeohash = HashMap.fromList [(a.geohash, a.areaName) | a <- configuredAreas]
    when (any (`notElem` HashMap.keys areaNameByGeohash) cells) $
      throwError (InvalidRequest "One or more selected geohash cells are not configured for this city")
    pure $ AreaPref.PreferredGeohashCells [(c, HashMap.lookup c areaNameByGeohash) | c <- cells]
  APIT.SelectRadiusArea sel -> do
    when (sel.radiusMeters <= Meters 0) $ throwError (InvalidRequest "Radius must be greater than zero")
    pure $ AreaPref.PreferredRadius sel.center sel.radiusMeters

-- | Reconstruct the response purely from the driver's tag list -- no DB lookup
-- needed at read time, since cell tags already carry their area name as the
-- tag value. minCells is threaded in rather than refetched here, since both
-- callers already have it (from getMinCells).
buildInfoRes :: Int -> Maybe [LYT.TagNameValueExpiry] -> APIT.AreaPreferenceInfoRes
buildInfoRes minCells mbTags =
  case AreaPref.driverAreaPreferenceFromTags (fromMaybe [] mbTags) of
    Nothing -> APIT.AreaPreferenceInfoRes {selectedGeohashAreas = [], radiusArea = Nothing, minCells = minCells}
    Just (AreaPref.PreferredRadius center radiusMeters) ->
      APIT.AreaPreferenceInfoRes
        { selectedGeohashAreas = [],
          radiusArea = Just APIT.RadiusAreaSelection {center = center, radiusMeters = radiusMeters},
          minCells = minCells
        }
    Just (AreaPref.PreferredGeohashCells cells) ->
      APIT.AreaPreferenceInfoRes
        { selectedGeohashAreas = [APIT.SelectedGeohashArea {geohash = c, areaName = areaName} | (c, areaName) <- cells],
          radiusArea = Nothing,
          minCells = minCells
        }
