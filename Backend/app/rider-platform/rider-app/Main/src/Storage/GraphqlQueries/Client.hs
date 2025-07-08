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
import qualified Data.Aeson
import Data.List (isInfixOf)
import Data.Morpheus.Client
import Data.Morpheus.Client.CodeGen.Internal
import Data.Proxy
import Data.Text (pack)
import qualified Data.Text as Text
import qualified Data.Time.LocalTime as LocalTime
import Domain.Types.IntegratedBPPConfig
import Kernel.Prelude
import Kernel.Utils.Common
import SharedLogic.External.Nandi.Flow (postGtfsGraphQL)
import SharedLogic.External.Nandi.Types (GtfsGraphQLRequest (..))
import Storage.GraphqlQueries.Types
import Tools.MultiModal as MM

executeRouteStopTimeTableQuery ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) =>
  IntegratedBPPConfig ->
  RouteStopTimeTableQueryVars ->
  m (Either String RouteStopTimeTableResponse)
executeRouteStopTimeTableQuery integratedBPPConfig vars = do
  let query = (Data.Proxy.Proxy :: Data.Proxy.Proxy RouteStopTimeTableQuery)
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  let request' =
        GtfsGraphQLRequest
          { query = pack (__query query),
            variables = Just (toJSON vars),
            operation_name = Just (pack (show (__name query))),
            city = Just integratedBPPConfig.merchantOperatingCityId
          }
  result <- postGtfsGraphQL baseUrl request'
  case Data.Aeson.fromJSON result of
    Data.Aeson.Error err -> return $ Left err
    Data.Aeson.Success (response :: OTPResponse) -> Right . RouteStopTimeTableResponse <$> transformToTimeTableEntries response

-- Helper function to convert OTP response to our domain model
transformToTimeTableEntries :: MonadFlow m => OTPResponse -> m [TimetableEntry]
transformToTimeTableEntries otpResponse = do
  now <- getCurrentTime
  return $ map (transformEntry otpResponse.stop now) otpResponse.stop.stoptimesWithoutPatterns

transformEntry :: StopData -> UTCTime -> RouteStopTimeTableEntry -> TimetableEntry
transformEntry stopData timestamp entry = do
  TimetableEntry
    { routeCode = fromMaybe entry.trip.route.gtfsId $ lastMay $ Text.splitOn ":" entry.trip.route.gtfsId,
      serviceTierType = mapToServiceTierType entry.trip.gtfsId,
      stopCode = fromMaybe stopData.gtfsId $ lastMay $ Text.splitOn ":" stopData.gtfsId,
      stage = entry.extraInfo >>= (.fareStageNumber) >>= readMaybe . Text.unpack,
      providerStopCode = entry.extraInfo >>= (.providerStopCode),
      -- Convert seconds from midnight to HH:MM:SS
      timeOfArrival = secondsToTime entry.scheduledArrival,
      timeOfDeparture = secondsToTime entry.scheduledDeparture,
      tripId = fromMaybe entry.trip.gtfsId $ lastMay $ Text.splitOn ":" entry.trip.gtfsId,
      createdAt = timestamp,
      updatedAt = timestamp,
      platformCode = entry.stop >>= (.platformCode)
    }

-- Convert seconds from midnight to HH:MM:SS format
secondsToTime :: Int -> LocalTime.TimeOfDay
secondsToTime seconds =
  let hours :: Int = seconds `div` 3600
      minutes :: Int = (seconds `mod` 3600) `div` 60
      secs = fromIntegral $ seconds `mod` 60
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
extractServiceCode routeCode = maybe "O" snd (find match patterns)
  where
    patterns =
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
    match (pat, _) = pat `isInfixOf` Text.unpack routeCode
