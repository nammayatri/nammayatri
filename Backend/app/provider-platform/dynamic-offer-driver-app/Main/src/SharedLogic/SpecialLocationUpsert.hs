{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SpecialLocationUpsert
  ( SpecialLocationCSVRow (..),
    upsertSpecialLocationsFromCsv,
    readCsv,
    makeSpecialLocation,
    groupSpecialLocationAndGates,
    processSpecialLocationAndGatesGroup,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as Common
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import qualified EulerHS.Language as L
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Geometry
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.GateInfoGeom as QGIG
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Queries.SpecialLocationGeom as QSLG
import qualified Lib.Queries.SpecialLocationPriority as QSLP
import qualified Lib.Types.GateInfo as DGI
import qualified Lib.Types.SpecialLocation as DSL
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Types.SpecialLocationPriority as SLP
import Tools.Error

---------------------------------------------------------------------
-- CSV Row Data Type
---------------------------------------------------------------------
data SpecialLocationCSVRow = SpecialLocationCSVRow
  { city :: Text,
    locationName :: Text,
    enabled :: Text,
    locationFileName :: Text,
    locationType :: Text,
    category :: Text,
    gateInfoName :: Text,
    gateInfoFileName :: Text,
    gateInfoLat :: Text,
    gateInfoLon :: Text,
    gateInfoDefaultDriverExtra :: Text,
    gateInfoAddress :: Text,
    gateInfoHasGeom :: Text,
    gateInfoCanQueueUpOnGate :: Text,
    gateInfoType :: Text,
    gateInfoGateTags :: Text,
    priority :: Text,
    pickupPriority :: Text,
    dropPriority :: Text,
    specialLocationId :: Text
  }
  deriving (Show)

instance FromNamedRecord SpecialLocationCSVRow where
  parseNamedRecord r =
    SpecialLocationCSVRow
      <$> r .: "city"
      <*> r .: "location_name"
      <*> r .: "enabled"
      <*> r .: "location_file_name"
      <*> r .: "location_type"
      <*> r .: "category"
      <*> r .: "gate_info_name"
      <*> r .: "gate_info_file_name"
      <*> r .: "gate_info_lat"
      <*> r .: "gate_info_lon"
      <*> r .: "gate_info_default_driver_extra"
      <*> r .: "gate_info_address"
      <*> r .: "gate_info_has_geom"
      <*> r .: "gate_info_can_queue_up_on_gate"
      <*> r .: "gate_info_type"
      <*> r .: "gate_info_tags"
      <*> r .: "priority"
      <*> r .: "pickup_priority"
      <*> r .: "drop_priority"
      <*> r .: "special_location_id"

---------------------------------------------------------------------
-- CSV Helper Functions
---------------------------------------------------------------------
cleanField :: Text -> Maybe Text
cleanField t =
  case T.strip t of
    "" -> Nothing
    s -> Just s

readCSVField :: Read a => Int -> Text -> Text -> Flow a
readCSVField idx fieldValue fieldName =
  cleanField fieldValue >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

readMaybeCSVField :: Read a => Int -> Text -> Text -> Maybe a
readMaybeCSVField _ fieldValue _ =
  cleanField fieldValue >>= readMaybe . T.unpack

cleanCSVField :: Int -> Text -> Text -> Flow Text
cleanCSVField idx fieldValue fieldName =
  cleanField fieldValue & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

cleanMaybeCSVField :: Int -> Text -> Text -> Maybe Text
cleanMaybeCSVField _ fieldValue _ = cleanField fieldValue

parseGateTags :: Text -> Maybe [Text]
parseGateTags fieldValue =
  case cleanField fieldValue of
    Nothing -> Nothing
    Just tags ->
      let tagList = filter (not . T.null) $ map T.strip $ T.splitOn "," tags
       in if null tagList then Nothing else Just tagList

---------------------------------------------------------------------
-- Main Upsert Function
---------------------------------------------------------------------
upsertSpecialLocationsFromCsv ::
  Context.City ->
  DMOC.MerchantOperatingCity ->
  FilePath ->
  [(Text, FilePath)] ->
  [(Text, FilePath)] ->
  Flow Common.APISuccessWithUnprocessedEntities
upsertSpecialLocationsFromCsv opCity merchantOpCity csvFile locationGeoms gateGeoms = do
  flatSpecialLocationAndGateInfo <- readCsv csvFile locationGeoms gateGeoms merchantOpCity
  let groupedSpecialLocationAndGateInfo = groupSpecialLocationAndGates flatSpecialLocationAndGateInfo
  unprocessedEntities <-
    foldlM
      ( \unprocessedEntities specialLocationAndGates -> do
          withTryCatch
            "processSpecialLocationAndGatesGroup"
            (processSpecialLocationAndGatesGroup opCity merchantOpCity specialLocationAndGates)
            >>= \case
              Left err -> return $ unprocessedEntities <> ["Unable to add special location : " <> show err]
              Right _ -> return unprocessedEntities
      )
      []
      groupedSpecialLocationAndGateInfo
  return $ Common.APISuccessWithUnprocessedEntities unprocessedEntities

---------------------------------------------------------------------
-- Read CSV
---------------------------------------------------------------------
readCsv ::
  FilePath ->
  [(Text, FilePath)] ->
  [(Text, FilePath)] ->
  DMOC.MerchantOperatingCity ->
  Flow [(Context.City, Text, (DSL.SpecialLocation, DGI.GateInfo), Int, Int, Maybe Text)]
readCsv csvFile locationGeomFiles gateGeomFiles merchantOpCity = do
  csvData <- L.runIO $ BS.readFile csvFile
  case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector SpecialLocationCSVRow)) of
    Left err -> throwError (InvalidRequest $ show err)
    Right (_, v) -> V.imapM (makeSpecialLocation locationGeomFiles gateGeomFiles merchantOpCity) v >>= (pure . V.toList)

---------------------------------------------------------------------
-- Make Special Location
---------------------------------------------------------------------
makeSpecialLocation ::
  [(Text, FilePath)] ->
  [(Text, FilePath)] ->
  DMOC.MerchantOperatingCity ->
  Int ->
  SpecialLocationCSVRow ->
  Flow (Context.City, Text, (DSL.SpecialLocation, DGI.GateInfo), Int, Int, Maybe Text)
makeSpecialLocation locationGeomFiles gateGeomFiles merchantOpCity idx row = do
  now <- getCurrentTime
  city :: Context.City <- readCSVField idx row.city "City"
  locationName :: Text <- cleanCSVField idx row.locationName "Location Name"
  locationFileName :: Text <- cleanCSVField idx row.locationFileName "Location File Name"
  (_, locationGeomFile) <- find (\(geomFileName, _) -> locationFileName == geomFileName) locationGeomFiles & fromMaybeM (InvalidRequest $ "KML file missing for location: " <> locationName)
  locationGeom <- getGeomFromKML locationGeomFile >>= fromMaybeM (InvalidRequest $ "Not able to convert the given KML to PostGis geom for location: " <> locationName)
  category :: Text <- cleanCSVField idx row.category "Category"
  let locationType :: Maybe SL.SpecialLocationType = readMaybeCSVField idx row.locationType "Location Type"
      mbSpecialLocationId :: Maybe Text = cleanField row.specialLocationId
  enabled :: Bool <- readCSVField idx row.enabled "Enabled"
  let priority :: Maybe Int = readMaybeCSVField idx row.priority "Priority"
  pickupPriority :: Int <- readCSVField idx row.pickupPriority "Pickup Priority"
  dropPriority :: Int <- readCSVField idx row.dropPriority "Drop Priority"
  gateInfoId <- generateGUID
  gateInfoName :: Text <- cleanCSVField idx row.gateInfoName "Gate Info (name)"
  gateInfoLat :: Double <- readCSVField idx row.gateInfoLat "Gate Info (latitude)"
  gateInfoLon :: Double <- readCSVField idx row.gateInfoLon "Gate Info (longitude)"
  let gateInfoDefaultDriverExtra :: Maybe Int = readMaybeCSVField idx row.gateInfoDefaultDriverExtra "Gate Info (default_driver_extra)"
      gateInfoAddress :: Maybe Text = cleanMaybeCSVField idx row.gateInfoAddress "Gate Info (address)"
      gateInfoGateTags :: Maybe [Text] = parseGateTags row.gateInfoGateTags
  gateInfoType :: DGI.GateType <- readCSVField idx row.gateInfoType "Gate Info (type)"
  gateInfoHasGeom :: Bool <- readCSVField idx row.gateInfoHasGeom "Gate Info (geom)"
  gateInfoCanQueueUpOnGate :: Bool <- readCSVField idx row.gateInfoCanQueueUpOnGate "Gate Info (can_queue_up_on_gate)"
  gateInfoGeom <- do
    if gateInfoHasGeom
      then do
        gateInfoFileName :: Text <- cleanCSVField idx row.gateInfoFileName "Gate Info (file_name)"
        (_, gateInfoGeomFile) <- find (\(gateFileName, _) -> gateInfoFileName == gateFileName) gateGeomFiles & fromMaybeM (InvalidRequest $ "KML file missing for gateInfo: " <> gateInfoName)
        gateGeom <- getGeomFromKML gateInfoGeomFile >>= fromMaybeM (InvalidRequest $ "Not able to convert the given KML to PostGis geom for gateInfo: " <> gateInfoName)
        return $ Just gateGeom
      else return Nothing
  let specialLocation =
        DSL.SpecialLocation
          { id = Id locationName,
            enabled = enabled,
            locationName = locationName,
            category = category,
            merchantId = Just (cast merchantOpCity.merchantId),
            merchantOperatingCityId = Just (cast merchantOpCity.id),
            linkedLocationsIds = [],
            gates = [],
            priority = fromMaybe 0 priority,
            locationType = fromMaybe SL.Open locationType,
            geom = Just $ T.pack locationGeom,
            createdAt = now,
            updatedAt = now
          }
      gateInfo =
        DGI.GateInfo
          { id = gateInfoId,
            point = LatLong {lat = gateInfoLat, lon = gateInfoLon},
            specialLocationId = Id locationName,
            defaultDriverExtra = gateInfoDefaultDriverExtra,
            name = gateInfoName,
            address = gateInfoAddress,
            geom = T.pack <$> gateInfoGeom,
            canQueueUpOnGate = gateInfoCanQueueUpOnGate,
            gateType = gateInfoType,
            merchantId = Just (cast merchantOpCity.merchantId),
            merchantOperatingCityId = Just (cast merchantOpCity.id),
            createdAt = now,
            updatedAt = now,
            gateTags = gateInfoGateTags
          }
  return (city, locationName, (specialLocation, gateInfo), pickupPriority, dropPriority, mbSpecialLocationId)

---------------------------------------------------------------------
-- Group Special Locations and Gates
---------------------------------------------------------------------
groupSpecialLocationAndGates ::
  [(Context.City, Text, (DSL.SpecialLocation, DGI.GateInfo), Int, Int, Maybe Text)] ->
  [[(Context.City, Text, (DSL.SpecialLocation, DGI.GateInfo), Int, Int, Maybe Text)]]
groupSpecialLocationAndGates = DL.groupBy (\a b -> fst2 a == fst2 b) . DL.sortBy (compare `on` fst2)
  where
    fst2 (c, l, _, _, _, _) = (c, l)

---------------------------------------------------------------------
-- Run Validation on Special Location and Gates Group
---------------------------------------------------------------------
runValidationOnSpecialLocationAndGatesGroup ::
  Context.City ->
  DMOC.MerchantOperatingCity ->
  [(Context.City, Text, (DSL.SpecialLocation, DGI.GateInfo), Int, Int, Maybe Text)] ->
  Flow ()
runValidationOnSpecialLocationAndGatesGroup _ _ [] = throwError $ InvalidRequest "Empty Special Location Group"
runValidationOnSpecialLocationAndGatesGroup opCity merchantOpCity (x : _) = do
  let (city, _locationName, (specialLocation, _), pickupPriority, dropPriority, _) = x
  if city /= opCity
    then throwError $ InvalidRequest ("Can't process special location for different city: " <> show city <> ", please login with this city in dashboard")
    else do
      -- TODO :: Add Validation for Overlapping Geometries
      QSLP.findByMerchantOpCityIdAndCategory merchantOpCity.id.getId specialLocation.category
        >>= \case
          Just _ -> return ()
          Nothing -> do
            when (pickupPriority < 0 || dropPriority < 0) $ throwError $ InvalidRequest ("Pickup and Drop Priority must be greater than or equal to 0 for category: " <> specialLocation.category <> " and merchantOpCity: " <> merchantOpCity.id.getId)
            priorityId <- generateGUID
            let newPriority =
                  SLP.SpecialLocationPriority
                    { id = Id priorityId,
                      merchantId = merchantOpCity.merchantId.getId,
                      merchantOperatingCityId = merchantOpCity.id.getId,
                      category = specialLocation.category,
                      pickupPriority = pickupPriority,
                      dropPriority = dropPriority
                    }
            void $ runTransaction $ QSLP.create newPriority
            return ()

---------------------------------------------------------------------
-- Process Special Location and Gates Group
---------------------------------------------------------------------
processSpecialLocationAndGatesGroup ::
  Context.City ->
  DMOC.MerchantOperatingCity ->
  [(Context.City, Text, (DSL.SpecialLocation, DGI.GateInfo), Int, Int, Maybe Text)] ->
  Flow ()
processSpecialLocationAndGatesGroup _ _ [] = throwError $ InvalidRequest "Empty Special Location Group"
processSpecialLocationAndGatesGroup opCity merchantOpCity specialLocationAndGates@(x : _) = do
  void $ runValidationOnSpecialLocationAndGatesGroup opCity merchantOpCity specialLocationAndGates
  let (_city, locationName, (specialLocation, _), _, _, mbSpecialLocationIdFromCsv) = x
  -- CSV specialLocationId takes precedence over parameter
  specialLocationId <-
    case mbSpecialLocationIdFromCsv of
      Just splId -> return $ Id splId -- Use the specialLocationId from CSV or parameter if provided
      Nothing ->
        QSL.findByLocationNameAndCity locationName merchantOpCity.id.getId
          |<|>| QSL.findByLocationName locationName
            >>= \case
              Just spl -> do
                void $
                  runTransaction $ do
                    QSL.deleteById spl.id
                    QGI.deleteAll spl.id
                return $ spl.id
              Nothing -> generateGUID
  -- When specialLocationId is provided from CSV or parameter, still check and delete existing
  when (isJust mbSpecialLocationIdFromCsv) $ do
    QSL.findByLocationNameAndCity locationName merchantOpCity.id.getId
      |<|>| QSL.findByLocationName locationName
        >>= \case
          Just spl -> do
            void $
              runTransaction $ do
                QSL.deleteById spl.id
                QGI.deleteAll spl.id
          Nothing -> return ()
  void $ runTransaction $ QSLG.create $ specialLocation {DSL.id = specialLocationId}
  mapM_
    (\(_, _, (_, gateInfo), _, _, _) -> runTransaction $ QGIG.create $ gateInfo {DGI.specialLocationId = specialLocationId})
    specialLocationAndGates
