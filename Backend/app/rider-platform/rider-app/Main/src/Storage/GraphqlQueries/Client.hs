{-
 Copyright 2025-26, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import Data.Aeson (eitherDecode, encode, object)
import Data.List (isInfixOf)
import Data.Morpheus.Client
import Data.Morpheus.Client.CodeGen.Internal
import qualified Data.Text as Text
import qualified Data.Time.LocalTime as LocalTime
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Kernel.Utils.Servant.Client as KSC
import Network.HTTP.Client
import Network.HTTP.Types (statusCode)
import Storage.GraphqlQueries.Types

getOrCreateManager :: MonadFlow m => m Manager
getOrCreateManager = do
  manager <- L.getOption HttpManager
  case manager of
    Just m -> return m
    Nothing -> do
      logError "Manager not found in default creating a new one"
      manager' <- liftIO $ newManager defaultManagerSettings
      L.setOption HttpManager manager'
      return manager'

fromMaybeM' :: MonadFlow m => m a -> m (Maybe a) -> m a
fromMaybeM' defaultValue maybeValue = do
  value <- maybeValue
  maybe defaultValue return value

-- Execute the query and transform the response
executeRouteStopTimeTableQuery ::
  MonadFlow m =>
  BaseUrl ->
  RouteStopTimeTableQueryVars ->
  m (Either String RouteStopTimeTableResponse)
executeRouteStopTimeTableQuery baseUrl vars = do
  let graphqlUrl = showBaseUrl baseUrl

  logInfo $ "Executing RouteStopTimeTable GraphQL query to " <> graphqlUrl

  manager <- fromMaybeM' getOrCreateManager (L.lookupHTTPManager (Just KSC.defaultHttpManager))
  initialRequest <-
    try (parseRequest (toString graphqlUrl))
      >>= \case
        Left (err :: SomeException) -> do
          logError $ "Failed to parse request URL: " <> show err
          throwError $ InternalError $ "Failed to parse request URL: " <> show err
        Right req -> pure req

  -- Create the request body manually instead of using Morpheus Client's request function
  let reqBody =
        encode $
          object
            [ "query" .= __query (Kernel.Prelude.Proxy @RouteStopTimeTableQuery),
              "variables"
                .= object
                  [ "stopId" .= vars.stopCode
                  ]
            ]
  logDebug $ "Request body for GTFS: " <> show reqBody
  let req =
        initialRequest
          { method = "POST",
            requestHeaders = [("Content-Type", "application/json")],
            requestBody = RequestBodyLBS reqBody
          }

  response <- liftIO $ try $ httpLbs req manager
  logDebug $ "Response from grfs server: " <> show response

  case response of
    Left (err :: SomeException) -> do
      logError $ "HTTP request failed: " <> show err
      pure $ Left $ "HTTP request error: " <> show err
    Right res -> do
      let status = statusCode (responseStatus res)
      let respBody = responseBody res

      if status >= 200 && status < 300
        then case eitherDecode respBody of
          Left decodeErr -> do
            logError $ "Failed to parse GraphQL response: " <> show decodeErr
            logError $ "Response body: " <> show respBody
            pure $ Left $ "JSON parse error: " <> decodeErr
          Right (otpResponse :: OTPResponse) -> do
            -- Transform OTP response to our domain model
            entries <- transformToTimeTableEntries otpResponse
            logDebug $ "Transformed entries graphql: " <> show entries
            pure $ Right $ RouteStopTimeTableResponse entries
        else pure $ Left $ "HTTP error: " <> show status

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
      stage = entry.headsign >>= readMaybe . Text.unpack,
      -- Convert seconds from midnight to HH:MM:SS
      timeOfArrival = secondsToTime entry.scheduledArrival,
      timeOfDeparture = secondsToTime entry.scheduledDeparture,
      tripId = fromMaybe entry.trip.gtfsId $ lastMay $ Text.splitOn ":" entry.trip.gtfsId,
      createdAt = timestamp,
      updatedAt = timestamp
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
