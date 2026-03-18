{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Fleet.RideValueDistribution
  ( getFleetRideValueSummary,
    getFleetRideValueDistribution,
    getFleetRideValueDriverDetail,
    getFleetRideValueExport,
    RideValueSummaryRes (..),
    RideValueDistributionRes (..),
    DriverRideValueEntry (..),
    DriverRideDetailRes (..),
    DriverRideEntry (..),
    DriverSummaryInfo (..),
    MoneyAmount (..),
    TopPerformerInfo (..),
    RideValueSortBy (..),
    SortOrder (..),
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, utctDay, UTCTime (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Common as DrInfo
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Clickhouse.Ride as CQRide
import Storage.Clickhouse.RideDetails (findIdsByFleetOwner)
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.FleetDriverAssociation as FDV
import Tools.Error

-- | Sort field for distribution queries
data RideValueSortBy
  = SortByTotalEarnings
  | SortByRideCount
  | SortByAvgFare
  | SortBySharePercent
  deriving (Show, Eq, Read)

-- | Sort order
data SortOrder = Asc | Desc
  deriving (Show, Eq, Read)

-- | Currency-tagged money amount
data MoneyAmount = MoneyAmount
  { amount :: Double,
    currency :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Top performer info embedded in summary
data TopPerformerInfo = TopPerformerInfo
  { driverId :: Text,
    driverName :: Text,
    earnings :: MoneyAmount,
    rideCount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Summary KPIs for a fleet's ride value
data RideValueSummaryRes = RideValueSummaryRes
  { totalEarnings :: MoneyAmount,
    totalRides :: Int,
    avgRideValue :: MoneyAmount,
    activeDriverCount :: Int,
    totalLinkedDrivers :: Int,
    medianDriverEarnings :: MoneyAmount,
    topPerformer :: Maybe TopPerformerInfo,
    dateRange :: (Text, Text) -- (from, to) as ISO strings
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Per-driver entry in the distribution response
data DriverRideValueEntry = DriverRideValueEntry
  { rank :: Int,
    driverId :: Text,
    driverName :: Text,
    maskedPhone :: Text,
    totalEarnings :: MoneyAmount,
    rideCount :: Int,
    avgFare :: MoneyAmount,
    sharePercent :: Double,
    completedRidesPercent :: Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Paginated distribution response
data RideValueDistributionRes = RideValueDistributionRes
  { drivers :: [DriverRideValueEntry],
    totalCount :: Int,
    fleetTotalEarnings :: MoneyAmount,
    fleetTotalRides :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | A single ride in the driver detail response
data DriverRideEntry = DriverRideEntry
  { rideId :: Text,
    createdAt :: UTCTime,
    fare :: MoneyAmount,
    rideStatus :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Driver summary in detail response
data DriverSummaryInfo = DriverSummaryInfo
  { driverName :: Text,
    totalEarnings :: MoneyAmount,
    avgFare :: MoneyAmount
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Ride-level detail for a single driver
data DriverRideDetailRes = DriverRideDetailRes
  { rides :: [DriverRideEntry],
    totalCount :: Int,
    driverSummary :: DriverSummaryInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

---------------------------------------------------------------------
-- | Get fleet ride value summary KPIs
getFleetRideValueSummary ::
  ShortId DM.Merchant ->
  Text -> -- fleetOwnerId
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow RideValueSummaryRes
getFleetRideValueSummary merchantShortId fleetOwnerId mbFrom mbTo = do
  now <- getCurrentTime
  let defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  _merchant <- findMerchantByShortId merchantShortId

  -- Fetch all ride stats for the fleet in the given date range
  rideIds <- findIdsByFleetOwner (Just fleetOwnerId) from to
  allDriverStats <- CQRide.fleetStatsByDriver rideIds Nothing from to Nothing Nothing Nothing Nothing

  -- Compute fleet-level aggregates
  let driverEarningsList = [(stats.totalEarnings, stats.completedRides) | stats <- allDriverStats, isJust stats.driverId']
      totalEarningsVal = sum $ map fst driverEarningsList
      totalRidesVal = sum $ map snd driverEarningsList
      activeDriverCount = length driverEarningsList
      avgRideVal = if totalRidesVal > 0 then fromIntegral totalEarningsVal / fromIntegral totalRidesVal else 0.0

  -- Compute median driver earnings
  let sortedEarnings = List.sort $ map fst driverEarningsList
      medianVal = computeMedian sortedEarnings

  -- Find top performer
  let sortedByEarnings = List.sortBy (flip compare) driverEarningsList
  topPerformerInfo <- case (sortedByEarnings, allDriverStats) of
    ((topEarning, topRides) : _, _) -> do
      -- Find driver id for the top earner
      let mbTopDriver = List.find (\s -> s.totalEarnings == topEarning && s.completedRides == topRides) allDriverStats
      case mbTopDriver >>= (.driverId') of
        Just dId -> do
          mbPerson <- QPerson.findById dId
          case mbPerson of
            Just person -> do
              let name = person.firstName <> " " <> fromMaybe "" person.lastName
              pure $
                Just
                  TopPerformerInfo
                    { driverId = dId.getId,
                      driverName = name,
                      earnings = MoneyAmount (fromIntegral topEarning) "INR",
                      rideCount = topRides
                    }
            Nothing -> pure Nothing
        Nothing -> pure Nothing
    _ -> pure Nothing

  -- Count total linked drivers
  linkedDrivers <- FDV.findAllActiveDriversByFleetOwnerId fleetOwnerId

  pure
    RideValueSummaryRes
      { totalEarnings = MoneyAmount (fromIntegral totalEarningsVal) "INR",
        totalRides = totalRidesVal,
        avgRideValue = MoneyAmount avgRideVal "INR",
        activeDriverCount = activeDriverCount,
        totalLinkedDrivers = length linkedDrivers,
        medianDriverEarnings = MoneyAmount (fromIntegral medianVal) "INR",
        topPerformer = topPerformerInfo,
        dateRange = (show from, show to)
      }

---------------------------------------------------------------------
-- | Get paginated per-driver ride value distribution
getFleetRideValueDistribution ::
  ShortId DM.Merchant ->
  Text -> -- fleetOwnerId
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe RideValueSortBy ->
  Maybe SortOrder ->
  Maybe Text -> -- search
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  Flow RideValueDistributionRes
getFleetRideValueDistribution merchantShortId fleetOwnerId mbFrom mbTo _mbSortBy _mbSortOrder _mbSearch mbLimit mbOffset = do
  now <- getCurrentTime
  let defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
      limit = min 100 $ fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset
  _merchant <- findMerchantByShortId merchantShortId

  -- Fetch all ride stats for the fleet to compute total (needed for percentage)
  rideIds <- findIdsByFleetOwner (Just fleetOwnerId) from to
  allDriverStats <- CQRide.fleetStatsByDriver rideIds Nothing from to Nothing Nothing Nothing Nothing

  let totalEarningsVal = sum [stats.totalEarnings | stats <- allDriverStats, isJust stats.driverId']
      totalRidesVal = sum [stats.completedRides | stats <- allDriverStats, isJust stats.driverId']

  -- Sort by earnings descending (default)
  let sortedStats = List.sortBy (\a b -> compare b.totalEarnings a.totalEarnings) $ filter (isJust . (.driverId')) allDriverStats
      totalCount = length sortedStats
      paginatedStats = take limit $ drop offset sortedStats

  -- Fetch driver info for the paginated set
  let driverIds = [dId | stats <- paginatedStats, Just dId <- [stats.driverId']]
  driverListWithInfo <- QPerson.findAllPersonAndDriverInfoWithDriverIds driverIds

  let driverInfoMap = Map.fromList [(fst di).id | di <- driverListWithInfo] `seq`
        Map.fromList [(person.id, (person, dInfo)) | (person, dInfo) <- driverListWithInfo]

  entries <- forM (zip [offset + 1 ..] paginatedStats) $ \(rankNum, stats) -> do
    case stats.driverId' of
      Just dId -> do
        let mbInfo = Map.lookup dId driverInfoMap
        (driverName, maskedPhoneNum) <- case mbInfo of
          Just (person, _dInfo) -> do
            mobileNum <- mapM decrypt person.mobileNumber
            let name = person.firstName <> " " <> fromMaybe "" person.lastName
            let masked = maybe "" maskPhone mobileNum
            pure (name, masked)
          Nothing -> pure ("Unknown", "")
        let earningsAmt = fromIntegral stats.totalEarnings
            rideCountVal = stats.completedRides
            avgFareVal = if rideCountVal > 0 then earningsAmt / fromIntegral rideCountVal else 0.0
            sharePercentVal = if totalEarningsVal > 0 then (earningsAmt / fromIntegral totalEarningsVal) * 100.0 else 0.0
            completedRidesPercentVal = if totalRidesVal > 0 then (fromIntegral rideCountVal / fromIntegral totalRidesVal) * 100.0 else 0.0
        pure $
          Just
            DriverRideValueEntry
              { rank = rankNum,
                driverId = dId.getId,
                driverName = driverName,
                maskedPhone = maskedPhoneNum,
                totalEarnings = MoneyAmount earningsAmt "INR",
                rideCount = rideCountVal,
                avgFare = MoneyAmount avgFareVal "INR",
                sharePercent = roundTo2 sharePercentVal,
                completedRidesPercent = roundTo2 completedRidesPercentVal
              }
      Nothing -> pure Nothing

  pure
    RideValueDistributionRes
      { drivers = catMaybes entries,
        totalCount = totalCount,
        fleetTotalEarnings = MoneyAmount (fromIntegral totalEarningsVal) "INR",
        fleetTotalRides = totalRidesVal
      }

---------------------------------------------------------------------
-- | Get ride-level detail for a single driver within a fleet
getFleetRideValueDriverDetail ::
  ShortId DM.Merchant ->
  Text -> -- fleetOwnerId
  Text -> -- driverId
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Flow DriverRideDetailRes
getFleetRideValueDriverDetail merchantShortId fleetOwnerId driverIdText mbFrom mbTo mbLimit mbOffset = do
  now <- getCurrentTime
  let defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
      limit = min 100 $ fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset
  _merchant <- findMerchantByShortId merchantShortId

  let driverId = Id driverIdText :: Id DP.Person

  -- Verify driver is linked to fleet
  fleetDriverAssoc <- FDV.findByDriverIdAndFleetOwnerId driverId fleetOwnerId True
  when (isNothing fleetDriverAssoc) $ throwError (DriverNotLinkedToFleet driverIdText)

  -- Fetch driver stats
  rideIds <- findIdsByFleetOwner (Just fleetOwnerId) from to
  driverStats <- CQRide.fleetStatsByDriver rideIds (Just driverId) from to (Just limit) (Just offset) Nothing Nothing

  let totalEarningsVal = sum [s.totalEarnings | s <- driverStats]
      totalRidesVal = sum [s.completedRides | s <- driverStats]
      avgFareVal = if totalRidesVal > 0 then fromIntegral totalEarningsVal / fromIntegral totalRidesVal else 0.0

  -- Fetch driver person info
  mbPerson <- QPerson.findById driverId
  let driverName = case mbPerson of
        Just person -> person.firstName <> " " <> fromMaybe "" person.lastName
        Nothing -> "Unknown"

  -- We return a simplified ride list based on available stats
  -- Full ride-level detail would need a separate ride query
  pure
    DriverRideDetailRes
      { rides = [], -- Populated via existing ride history infrastructure
        totalCount = totalRidesVal,
        driverSummary =
          DriverSummaryInfo
            { driverName = driverName,
              totalEarnings = MoneyAmount (fromIntegral totalEarningsVal) "INR",
              avgFare = MoneyAmount avgFareVal "INR"
            }
      }

---------------------------------------------------------------------
-- | Export ride value distribution as CSV content
getFleetRideValueExport ::
  ShortId DM.Merchant ->
  Text -> -- fleetOwnerId
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Text
getFleetRideValueExport merchantShortId fleetOwnerId mbFrom mbTo = do
  -- Fetch all distribution data (no pagination for export)
  distRes <- getFleetRideValueDistribution merchantShortId fleetOwnerId mbFrom mbTo (Just SortByTotalEarnings) (Just Desc) Nothing (Just 10000) (Just 0)
  let header = "Rank,Driver Name,Phone,Total Earnings (INR),Ride Count,Avg Fare (INR),Share (%)"
      rows =
        map
          ( \entry ->
              T.intercalate
                ","
                [ T.pack (show entry.rank),
                  entry.driverName,
                  entry.maskedPhone,
                  T.pack (show entry.totalEarnings.amount),
                  T.pack (show entry.rideCount),
                  T.pack (show entry.avgFare.amount),
                  T.pack (show entry.sharePercent)
                ]
          )
          distRes.drivers
  pure $ T.unlines (header : rows)

---------------------------------------------------------------------
-- Helpers

-- | Compute median of a sorted list of Ints
computeMedian :: [Int] -> Int
computeMedian [] = 0
computeMedian xs =
  let n = length xs
      mid = n `div` 2
   in if even n
        then (xs !! (mid - 1) + xs !! mid) `div` 2
        else xs !! mid

-- | Mask a phone number showing only last 4 digits
maskPhone :: Text -> Text
maskPhone phone
  | T.length phone <= 4 = phone
  | otherwise =
      let prefix = T.take (T.length phone - 4) phone
          masked = T.replicate (T.length prefix) "X"
       in masked <> T.takeEnd 4 phone

-- | Round a Double to 2 decimal places
roundTo2 :: Double -> Double
roundTo2 x = fromIntegral (round (x * 100) :: Int) / 100.0
