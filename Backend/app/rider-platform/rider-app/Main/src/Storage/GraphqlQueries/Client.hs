{-
 Copyright 2025-26, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Storage.GraphqlQueries.Client
  ( RouteStopTimeTableQueryVars (..),
    RouteStopTimeTableResponse (..),
    TimetableEntry (..),
    TripData (..),
    RouteData (..),
    StopData (..),
    OTPResponse (..),
    executeRouteStopTimeTableQuery,
    mapToServiceTierType,
  )
where

import qualified BecknV2.FRFS.Enums
import Data.Aeson.Types (parseEither)
import Data.List (isInfixOf)
import Data.Morpheus.Client
import Data.Morpheus.Client.CodeGen.Internal
import Data.Proxy
import Data.Text (pack)
import qualified Data.Text as Text
import qualified Data.Time.LocalTime as LocalTime
import Domain.Types.IntegratedBPPConfig
import Kernel.Prelude
import Kernel.Types.Time
import Kernel.Utils.Common
import SharedLogic.External.Nandi.Flow (postGtfsGraphQL)
import SharedLogic.External.Nandi.Types (GtfsGraphQLRequest (..))
import Storage.GraphqlQueries.Types
import Tools.MultiModal as MM

executeRouteStopTimeTableQuery ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) =>
  IntegratedBPPConfig ->
  RouteStopTimeTableQueryVars ->
  Bool ->
  m (Either String RouteStopTimeTableResponse)
executeRouteStopTimeTableQuery integratedBPPConfig vars needOnlyOneTrip = do
  let query = (Data.Proxy.Proxy :: Data.Proxy.Proxy RouteStopTimeTableQuery)
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  let request' =
        GtfsGraphQLRequest
          { query = pack (__query query),
            variables = Just (toJSON vars),
            operation_name = Nothing,
            city = Just integratedBPPConfig.merchantOperatingCityId.getId, -- todo: remove this
            feedId = integratedBPPConfig.feedKey
          }
  result <- postGtfsGraphQL baseUrl request'
  case parseEither parseJSON result of
    Left err -> return $ Left err
    Right (response :: OTPResponse) -> do
      Right . RouteStopTimeTableResponse <$> transformToTimeTableEntries vars.routeIds response needOnlyOneTrip

-- Helper function to convert OTP response to our domain model
transformToTimeTableEntries :: MonadFlow m => [Text] -> OTPResponse -> Bool -> m [TimetableEntry]
transformToTimeTableEntries routeIds otpResponse needOnlyOneTrip = do
  now <- getCurrentTime
  let desiredStopTimes = filter (\entry -> (fromMaybe entry.trip.route.gtfsId $ lastMay $ Text.splitOn ":" entry.trip.route.gtfsId) `elem` routeIds) otpResponse.stop.stoptimesWithoutPatterns
  let result = map (transformEntry otpResponse.stop now) $ if needOnlyOneTrip then take 1 desiredStopTimes else desiredStopTimes
  logDebug $ "result from transformToTimeTableEntries is " <> show result <> "full data: " <> show otpResponse <> " and the routeCodes are: " <> show routeIds
  pure result

transformEntry :: StopData -> UTCTime -> RouteStopTimeTableEntry -> TimetableEntry
transformEntry stopData timestamp entry = do
  TimetableEntry
    { routeCode = fromMaybe entry.trip.route.gtfsId $ lastMay $ Text.splitOn ":" entry.trip.route.gtfsId,
      serviceTierType = mapToServiceTierType entry.trip.gtfsId,
      stopCode = fromMaybe stopData.gtfsId $ lastMay $ Text.splitOn ":" stopData.gtfsId,
      stage = entry.extraInfo >>= (.fareStageNumber) >>= readMaybe . Text.unpack,
      providerStopCode = entry.extraInfo >>= (.providerStopCode),
      -- Convert seconds from midnight to HH:MM:SS
      timeOfArrival = secondsToTime entry.realtimeArrival,
      timeOfDeparture = secondsToTime entry.realtimeDeparture,
      tripId = fromMaybe entry.trip.gtfsId $ lastMay $ Text.splitOn ":" entry.trip.gtfsId,
      createdAt = timestamp,
      updatedAt = timestamp,
      platformCode = entry.stop >>= (.platformCode),
      isStageStop = entry.extraInfo >>= (.isStageStop),
      arrivalDelay = Seconds entry.arrivalDelay
    }

-- Convert seconds from midnight to HH:MM:SS format
-- Wraps around for next-day times (>= 24 hours)
secondsToTime :: Int -> LocalTime.TimeOfDay
secondsToTime seconds =
  let totalSeconds = seconds `mod` 86400 -- Wrap around after 24 hours (86400 seconds)
      hours :: Int = totalSeconds `div` 3600
      minutes :: Int = (totalSeconds `mod` 3600) `div` 60
      secs = fromIntegral $ totalSeconds `mod` 60
   in LocalTime.TimeOfDay hours minutes secs

-- Map route codes to service tier types
mapToServiceTierType :: Text -> BecknV2.FRFS.Enums.ServiceTierType
mapToServiceTierType routeCode =
  let serviceCode = extractServiceCode routeCode
      serviceTierMapping =
        [ ("Z", BecknV2.FRFS.Enums.AC),
          ("XS", BecknV2.FRFS.Enums.SPECIAL),
          ("OS", BecknV2.FRFS.Enums.NON_AC),
          ("S", BecknV2.FRFS.Enums.EXECUTIVE),
          ("X", BecknV2.FRFS.Enums.EXPRESS),
          ("O", BecknV2.FRFS.Enums.ORDINARY)
        ]
   in fromMaybe BecknV2.FRFS.Enums.NON_AC $ lookup serviceCode serviceTierMapping

extractServiceCode :: Text -> Text
extractServiceCode routeCode = maybe (fromMaybe "O" infixMatch) (\x -> x) startMatch
  where
    startPatterns =
      [ ("XS", "XS"),
        ("OS", "OS"),
        ("S", "S"),
        ("X", "X"),
        ("O", "O"),
        ("Z", "Z")
      ]

    startMatch =
      snd <$> find (\(p, _) -> p `Text.isPrefixOf` routeCode) startPatterns

    infixPatterns =
      [ ("-Z-", "Z"),
        ("-XS-", "XS"),
        ("-OS-", "OS"),
        ("-S-", "S"),
        ("-X-", "X"),
        ("-O-", "O"),
        ("Z", "Z"),
        ("XS", "XS"),
        ("OS", "OS"),
        ("S", "S"),
        ("X", "X"),
        ("O", "O")
      ]

    infixMatch =
      snd <$> find (\(p, _) -> p `isInfixOf` Text.unpack routeCode) infixPatterns
