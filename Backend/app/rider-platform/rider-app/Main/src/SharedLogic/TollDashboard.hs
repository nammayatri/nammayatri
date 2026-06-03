{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.TollDashboard
  ( tollToAPIEntity,
    mkTollFromUpsertReq,
    tollGateFromAPI,
    tollGateToAPI,
    invalidateTollCache,
    applyPagination,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common.Merchant as DM
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Toll.Domain.Types.Toll as Toll
import Toll.Domain.Types.TollGate
  ( TollGate (..),
    geoPolygonToText,
    lineStringToGeoJsonText,
  )
import qualified Toll.Storage.CachedQueries.Toll as CQToll

tollToAPIEntity :: Toll.Toll -> DM.TollAPIEntity
tollToAPIEntity Toll.Toll {..} =
  DM.TollAPIEntity
    { id = getId id,
      name = name,
      price = price.amount,
      currency = price.currency,
      tollStartGates = map tollGateToAPI tollStartGates,
      tollEndGates = map tollGateToAPI tollEndGates,
      isAutoRickshawAllowed = isAutoRickshawAllowed,
      isTwoWheelerAllowed = isTwoWheelerAllowed,
      merchantId = merchantId,
      merchantOperatingCityId = merchantOperatingCityId
    }

tollGateToAPI :: TollGate -> DM.TollGateAPIEntity
tollGateToAPI = \case
  LineGate ls -> DM.LineStringGateAPIEntity $ lineStringToGeoJsonText ls
  PolyGate gp -> DM.PolyGateAPIEntity $ geoPolygonToText gp

tollGateFromAPI :: DM.TollGateAPIEntity -> TollGate
tollGateFromAPI = \case
  DM.LineStringGateAPIEntity geoJson ->
    case Aeson.decodeStrict (TE.encodeUtf8 geoJson) :: Maybe TollGate of
      Just gate -> gate
      Nothing ->
        error $
          "Invalid LineString toll gate in API request: "
            <> show geoJson
  DM.PolyGateAPIEntity geoJson ->
    case Aeson.decodeStrict (TE.encodeUtf8 geoJson) :: Maybe TollGate of
      Just (PolyGate gp) -> PolyGate gp
      _ ->
        error $
          "Invalid Polygon toll gate in API request: "
            <> show geoJson

mkTollFromUpsertReq ::
  DM.UpsertTollReq ->
  Id Toll.Toll ->
  UTCTime ->
  Text ->
  Text ->
  Maybe Toll.Toll ->
  Toll.Toll
mkTollFromUpsertReq req tollId now merchantOpCityId merchantId mbExisting =
  Toll.Toll
    { id = tollId,
      name = req.name,
      price = mkPrice (Just req.currency) req.price,
      tollStartGates = map tollGateFromAPI req.tollStartGates,
      tollEndGates = map tollGateFromAPI req.tollEndGates,
      isAutoRickshawAllowed = req.isAutoRickshawAllowed,
      isTwoWheelerAllowed = req.isTwoWheelerAllowed,
      merchantId = Just merchantId,
      merchantOperatingCityId = Just merchantOpCityId,
      createdAt = maybe now (.createdAt) mbExisting,
      updatedAt = now
    }

invalidateTollCache :: (CacheFlow m r) => Text -> m ()
invalidateTollCache merchantOpCityId =
  Hedis.del $ CQToll.makeTollsKeyByMerchantOperatingCityId merchantOpCityId

applyPagination :: Maybe Int -> Maybe Int -> [a] -> [a]
applyPagination mbLimit mbOffset items =
  let offset = max 0 $ fromMaybe 0 mbOffset
      limited = case mbLimit of
        Nothing -> drop offset items
        Just limit -> take (max 0 limit) $ drop offset items
   in limited
