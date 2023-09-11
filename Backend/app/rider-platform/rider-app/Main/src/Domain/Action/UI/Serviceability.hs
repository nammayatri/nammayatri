{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Serviceability
  ( checkServiceability,
    ServiceabilityRes (..),
  )
where

import API.UI.HotSpot
import qualified Domain.Types.HotSpot as DHotSpot
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Person as Person
import Kernel.Beam.Functions
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.SpecialLocation as DSpecialLocation
import qualified Storage.CachedQueries.Merchant.MerchantConfigNew as QMCN
import Storage.Queries.Geometry (someGeometriesContain)
import Tools.Error

data ServiceabilityRes = ServiceabilityRes
  { serviceable :: Bool,
    specialLocation :: Maybe DSpecialLocation.SpecialLocation,
    geoJson :: Maybe Text,
    hotSpotInfo :: [DHotSpot.HotSpotInfo],
    blockRadius :: Maybe Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

checkServiceability ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  LatLong ->
  m ServiceabilityRes
checkServiceability settingAccessor (_, merchantId) location = do
  let merchId = merchantId -- why this?
  geoConfig <- fmap (.geofencingConfig) $ QMCN.findByMerchantId merchId >>= fromMaybeM (MerchantNotFound merchId.getId)
  let geoRestriction = settingAccessor geoConfig
  DHotSpot.HotSpotResponse {..} <- getHotspot location merchantId
  case geoRestriction of
    Unrestricted -> do
      let serviceable = True
      specialLocationBody <- QSpecialLocation.findSpecialLocationByLatLong location
      pure ServiceabilityRes {serviceable = serviceable, specialLocation = fst <$> specialLocationBody, geoJson = snd <$> specialLocationBody, ..}
    Regions regions -> do
      serviceable <- runInReplica $ someGeometriesContain location regions
      if serviceable
        then do
          specialLocationBody <- QSpecialLocation.findSpecialLocationByLatLong location
          pure ServiceabilityRes {serviceable = serviceable, specialLocation = fst <$> specialLocationBody, geoJson = snd <$> specialLocationBody, ..}
        else pure ServiceabilityRes {serviceable = serviceable, specialLocation = Nothing, geoJson = Nothing, ..}
