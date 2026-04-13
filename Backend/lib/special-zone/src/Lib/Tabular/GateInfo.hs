{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Tabular.GateInfo where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Lib.Tabular.SpecialLocation (SpecialLocationTId)
import qualified Lib.Types.GateInfo as Domain

derivePersistField "LatLong"

derivePersistField "Domain.GateType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    GateInfoT sql=gate_info
      id Text
      point LatLong
      specialLocationId SpecialLocationTId
      defaultDriverExtra Int Maybe
      name Text
      merchantId Text Maybe
      merchantOperatingCityId Text Maybe
      address Text Maybe
      canQueueUpOnGate Bool
      createdAt UTCTime
      updatedAt UTCTime
      gateType Domain.GateType
      gateTags [Text] Maybe
      walkDescription Text Maybe
      entryFeeAmount Double Maybe
      minDriverThresholdsJson Text Maybe
      maxDriverThresholdsJson Text Maybe
      demandThresholdsJson Text Maybe
      defaultMinDriverThreshold Int Maybe
      defaultMaxDriverThreshold Int Maybe
      defaultDemandThreshold Int Maybe
      notificationCooldownInSec Int Maybe
      maxRideSkipsBeforeQueueRemoval Int Maybe
      pickupZoneArrivalTimeoutInSec Int Maybe
      pickupRequestResponseTimeoutInSec Int Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey GateInfoT where
  type DomainKey GateInfoT = Id Domain.GateInfo
  fromKey (GateInfoTKey _id) = Id _id
  toKey (Id id) = GateInfoTKey id

instance FromTType GateInfoT Domain.GateInfo where
  fromTType GateInfoT {..} =
    return $
      Domain.GateInfo
        { id = Id id,
          specialLocationId = fromKey specialLocationId,
          geom = Nothing,
          merchantId = Id <$> merchantId,
          merchantOperatingCityId = Id <$> merchantOperatingCityId,
          minDriverThresholds = decodeThresholdMap minDriverThresholdsJson,
          maxDriverThresholds = decodeThresholdMap maxDriverThresholdsJson,
          demandThresholds = decodeThresholdMap demandThresholdsJson,
          ..
        }

-- | Decode the JSON text column into a per-variant threshold map.
--   Returns Nothing on missing/invalid JSON so we can fall back to defaults.
decodeThresholdMap :: Maybe Text -> Maybe (Map.Map Text Int)
decodeThresholdMap Nothing = Nothing
decodeThresholdMap (Just t) = A.decode (BL.fromStrict (TE.encodeUtf8 t))

-- | Encode a per-variant threshold map back to JSON text for storage.
encodeThresholdMap :: Maybe (Map.Map Text Int) -> Maybe Text
encodeThresholdMap = fmap (TE.decodeUtf8 . BL.toStrict . A.encode)
