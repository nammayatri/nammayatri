{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.FleetVehiclesAssociation where

import qualified Dashboard.Common as Common
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Domain.Types.FleetBookingAssignments as DFBA
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Environment
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FleetBookingAssignments as QFBA
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOIE
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCExtra
import Tools.Error

-- Custom response types tailored for boat fleet listing
newtype BoatFleetVehicleListRes = BoatFleetVehicleListRes {vehicles :: [BoatFleetVehicle]}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data BoatFleetVehicle = BoatFleetVehicle
  { fleetOwnerId :: Text,
    fleetOwnerName :: Text,
    rcId :: Text,
    vehicleNo :: Maybe Text,
    vehicleType :: Maybe Common.VehicleVariant,
    driverId :: Maybe Text,
    driverName :: Maybe Text,
    isActive :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- Helper data structure to hold RC with optional latest assignment
data RCWithAssignment = RCWithAssignment
  { rc :: DVRC.VehicleRegistrationCertificate,
    latestAssignment :: Maybe DFBA.FleetBookingAssignments,
    vehicleNumber :: Text
  }

getFleetVehicleAssociation :: Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Flow BoatFleetVehicleListRes
getFleetVehicleAssociation apiKey placeId mbLimit mbOffset mbSearchString = do
  fleetOwners <- QFOIE.getFleetOwnerByTicketPlaceId (Just placeId)
  let fleetOwnerIds = map (.fleetOwnerPersonId.getId) fleetOwners

  if null fleetOwnerIds
    then pure $ BoatFleetVehicleListRes []
    else do
      -- Use the first fleet owner's merchant for the RC query
      let firstOwnerId = head fleetOwnerIds
      firstOwnerPerson <- QP.findById (Id firstOwnerId) >>= fromMaybeM (PersonNotFound firstOwnerId)
      let merchantId = firstOwnerPerson.merchantId

      merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      unless (Just merchant.internalApiKey == apiKey) $
        throwError $ AuthBlocked "Invalid internal API key"

      persons <- mapM (\foId -> QP.findById (Id foId)) fleetOwnerIds
      let fleetOwnerNameMap =
            Map.fromList $
              zipWith
                (\foId mbP -> (foId, maybe "" (\p -> p.firstName <> fromMaybe "" ((" " <>) <$> p.lastName)) mbP))
                fleetOwnerIds
                persons

      mbRegNumberStringHash <- mapM getDbHash mbSearchString
      let limit = fromIntegral $ fromMaybe 1000 mbLimit
          offset = fromIntegral $ fromMaybe 0 mbOffset

      rcs <- VRCExtra.findAllValidRcByFleetOwnerIdsAndSearchStringWithoutVerificationStatusMF limit offset merchantId fleetOwnerIds mbSearchString mbRegNumberStringHash
      items <- mapM (buildItem fleetOwnerNameMap) rcs
      pure $ BoatFleetVehicleListRes items

getFleetVehicleAssociationV2 :: Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Flow BoatFleetVehicleListRes
getFleetVehicleAssociationV2 apiKey placeId mbLimit mbOffset mbSearchString = do
  fleetOwners <- QFOIE.getFleetOwnerByTicketPlaceId (Just placeId)
  let fleetOwnerIds = map (.fleetOwnerPersonId.getId) fleetOwners

  if null fleetOwnerIds
    then pure $ BoatFleetVehicleListRes []
    else do
      -- Use the first fleet owner's merchant for the RC query
      let firstOwnerId = head fleetOwnerIds
      firstOwnerPerson <- QP.findById (Id firstOwnerId) >>= fromMaybeM (PersonNotFound firstOwnerId)
      let merchantId = firstOwnerPerson.merchantId

      merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      unless (Just merchant.internalApiKey == apiKey) $
        throwError $ AuthBlocked "Invalid internal API key"

      persons <- mapM (\foId -> QP.findById (Id foId)) fleetOwnerIds
      let fleetOwnerNameMap =
            Map.fromList $
              zipWith
                (\foId mbP -> (foId, maybe "" (\p -> p.firstName <> fromMaybe "" ((" " <>) <$> p.lastName)) mbP))
                fleetOwnerIds
                persons

      mbRegNumberStringHash <- mapM getDbHash mbSearchString

      -- Fetch all RCs without pagination first
      rcs <- VRCExtra.findAllValidRcByFleetOwnerIdsAndSearchStringWithoutVerificationStatusMF 10000 0 merchantId fleetOwnerIds mbSearchString mbRegNumberStringHash

      -- Decrypt vehicle numbers and get all assignments
      rcsWithVehicleNos <- mapM (\r -> (r,) <$> decrypt r.certificateNumber) rcs
      let vehicleNos = map snd rcsWithVehicleNos

      -- Get all fleet booking assignments for these vehicles
      allAssignments <- if null vehicleNos then pure [] else QFBA.findAllByVehicleNos vehicleNos

      -- Get current time for filtering
      now <- getCurrentTime

      -- Build map of vehicle number to latest assignment
      let assignmentMap = buildLatestAssignmentMap allAssignments

      -- Join RCs with their latest assignments and filter/sort
      let rcsWithAssignments = map (\(r, vNo) -> RCWithAssignment r (Map.lookup vNo assignmentMap) vNo) rcsWithVehicleNos
          filteredAndSorted = sortRCsByAssignment now rcsWithAssignments

      -- Apply pagination
      let limit = fromMaybe 1000 mbLimit
          offset = fromMaybe 0 mbOffset
          paginatedRcs = take limit $ drop offset $ filteredAndSorted

      items <- mapM (buildItem fleetOwnerNameMap) (map (.rc) paginatedRcs)
      pure $ BoatFleetVehicleListRes items

-- Build a map of vehicle number to latest assignment based on assignment_end_time
buildLatestAssignmentMap :: [DFBA.FleetBookingAssignments] -> Map.Map Text DFBA.FleetBookingAssignments
buildLatestAssignmentMap assignments =
  Map.fromListWith selectLatest [(a.vehicleNo, a) | a <- assignments]
  where
    selectLatest a1 a2 =
      case (a1.assignmentEndTime, a2.assignmentEndTime) of
        (Just t1, Just t2) -> if t1 >= t2 then a1 else a2
        (Just _, Nothing) -> a1
        (Nothing, Just _) -> a2
        (Nothing, Nothing) -> a1 -- Both null, pick first

-- Filter and sort RCs based on assignment status
sortRCsByAssignment :: UTCTime -> [RCWithAssignment] -> [RCWithAssignment]
sortRCsByAssignment now rcsWithAssignments =
  let -- Filter: keep only assignment_end_time < now OR no assignment
      filtered = filter (shouldInclude now) rcsWithAssignments
      -- Sort: never assigned first, then by assignment_end_time ascending
      sorted = List.sortBy compareByAssignment filtered
   in sorted
  where
    shouldInclude :: UTCTime -> RCWithAssignment -> Bool
    shouldInclude currentTime rcWithAssignment =
      case rcWithAssignment.latestAssignment of
        Nothing -> True -- Never assigned
        Just assignment ->
          case assignment.assignmentEndTime of
            Nothing -> False -- Has assignment but no end time, exclude
            Just endTime -> endTime < currentTime -- Include only if ended
    compareByAssignment :: RCWithAssignment -> RCWithAssignment -> Ordering
    compareByAssignment rc1 rc2 =
      case (rc1.latestAssignment, rc2.latestAssignment) of
        (Nothing, Nothing) -> EQ -- Both never assigned
        (Nothing, Just _) -> LT -- Never assigned comes first
        (Just _, Nothing) -> GT -- Assigned comes after never assigned
        (Just a1, Just a2) ->
          -- Both have assignments, compare by assignment_end_time
          case (a1.assignmentEndTime, a2.assignmentEndTime) of
            (Just t1, Just t2) -> compare t1 t2
            (Just _, Nothing) -> LT
            (Nothing, Just _) -> GT
            (Nothing, Nothing) -> EQ

buildItem :: Map.Map Text Text -> DVRC.VehicleRegistrationCertificate -> Flow BoatFleetVehicle
buildItem nameMap rc = do
  let mFleetOwnerIdTxt = rc.fleetOwnerId
      fleetOwnerIdTxt = fromMaybe "" mFleetOwnerIdTxt
      fleetOwnerName = fromMaybe "" (Map.lookup fleetOwnerIdTxt nameMap)
  vehicleNumber <- decrypt rc.certificateNumber
  pure
    BoatFleetVehicle
      { fleetOwnerId = fleetOwnerIdTxt,
        fleetOwnerName = fleetOwnerName,
        rcId = rc.id.getId,
        vehicleNo = Just vehicleNumber,
        vehicleType = rc.vehicleVariant,
        driverId = Nothing,
        driverName = Nothing,
        isActive = False
      }
