{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.VehicleTripOwner
  ( VehicleTripOwner (..),
    TripOwnerTarget (..),
    activeOwnerTtl,
    vehicleTripOwnerKey,
    withVehicleTripOwnerRedis,
    noActiveTripTtl,
    noActiveTripOwner,
    runOwnerUpdate,
    setActiveTripOwner,
    markNoActiveTrip,
  )
where

import Data.List (nub)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Person
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id, getId)
import Kernel.Utils.Common (getCurrentTime, logError, logWarning, withTryCatch)
import qualified Storage.Queries.Person as QPerson

data VehicleTripOwner = VehicleTripOwner
  { activeTrip :: Bool,
    waybillNo :: Maybe Text,
    tripNumber :: Maybe Int,
    personIds :: [Text],
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TripOwnerTarget = TripOwnerTarget
  { targetWaybillNo :: Text,
    targetVehicleNumber :: Text,
    crewBadges :: [Text]
  }
  deriving (Show)

vehicleTripOwnerKey :: Text -> Text -> Text
vehicleTripOwnerKey gtfsId vehicleNum = "lts:vehicle_trip_owner:" <> T.strip gtfsId <> ":" <> T.strip vehicleNum

withVehicleTripOwnerRedis :: Flow a -> Flow a
withVehicleTripOwnerRedis = Hedis.withLTSRedis . Hedis.withCrossAppRedis

activeOwnerTtl :: Int
activeOwnerTtl = 86400

noActiveTripTtl :: Int
noActiveTripTtl = 3600

runOwnerUpdate :: Text -> Flow () -> Flow ()
runOwnerUpdate label action =
  withTryCatch ("VehicleTripOwner:" <> label) action >>= \case
    Right () -> pure ()
    Left err -> logError $ "VehicleTripOwner: " <> label <> " failed - " <> show err

noActiveTripOwner :: UTCTime -> VehicleTripOwner
noActiveTripOwner now = VehicleTripOwner {activeTrip = False, waybillNo = Nothing, tripNumber = Nothing, personIds = [], updatedAt = now}

setActiveTripOwner ::
  DIBC.IntegratedBPPConfig ->
  Text ->
  [Id Domain.Types.Person.Person] ->
  TripOwnerTarget ->
  Int ->
  Int ->
  Flow (Maybe VehicleTripOwner)
setActiveTripOwner integratedBPPConfig gtfsId callerIds target tripNo ttl = do
  let ownerKey = vehicleTripOwnerKey gtfsId target.targetVehicleNumber
  now <- getCurrentTime
  crew <- forM (filter (not . T.null) . map T.strip $ target.crewBadges) $ \badgeToken ->
    QPerson.findByOperatorBadgeTokenAndMerchantId (Just badgeToken) integratedBPPConfig.merchantId
  let crewIds = nub $ map getId callerIds <> map (getId . (.id)) (catMaybes crew)
  if null crewIds
    then do
      logWarning $ "VehicleTripOwner: no crew person resolved, key cleared - " <> ownerKey
      withVehicleTripOwnerRedis $ Hedis.del ownerKey
      pure Nothing
    else do
      let owner = VehicleTripOwner {activeTrip = True, waybillNo = Just target.targetWaybillNo, tripNumber = Just tripNo, personIds = crewIds, updatedAt = now}
      withVehicleTripOwnerRedis $ Hedis.setExp ownerKey owner ttl
      pure (Just owner)

markNoActiveTrip :: Text -> Text -> Text -> Flow ()
markNoActiveTrip gtfsId busNo wbNo = do
  let ownerKey = vehicleTripOwnerKey gtfsId busNo
  now <- getCurrentTime
  withVehicleTripOwnerRedis $ do
    mbOwner <- Hedis.get ownerKey
    let heldByThisWaybill owner = not owner.activeTrip || owner.waybillNo == Just wbNo
    when (maybe True heldByThisWaybill (mbOwner :: Maybe VehicleTripOwner)) $
      Hedis.setExp ownerKey (noActiveTripOwner now) noActiveTripTtl
