module Beckn.External.GoogleMaps.Client where

import qualified Beckn.External.GoogleMaps.API as API
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App (TraceFlag)
import Beckn.Types.Common
import Beckn.Utils.Common (fromClientError, throwError500)
import Beckn.Utils.Logging (HasLogContext, Log (..))
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail)
import EulerHS.Prelude
import GHC.Records (HasField)
import Servant.Client.Core (BaseUrl)

autoComplete ::
  ( HasField "dbCfg" r DBConfig,
    HasField "traceFlag" r TraceFlag,
    HasLogContext r
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Integer ->
  Text ->
  FlowR r GoogleMaps.SearchLocationResp
autoComplete url apiKey input location radius components = do
  res <- callAPIWithTrail url (API.autoComplete apiKey input location radius components) "autoComplete"
  case res of
    Right x -> return x
    Left cliErr -> do
      let err = fromClientError cliErr
      logError "client Google maps API autoComplete call error" $ (err ^. #_message) ?: "Some error"
      throwError500 "Google maps API autoComplete error"

placeDetails ::
  ( HasField "dbCfg" r DBConfig,
    HasField "traceFlag" r TraceFlag,
    HasLogContext r
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  FlowR r GoogleMaps.PlaceDetailsResp
placeDetails url apiKey placeId fields = do
  res <- callAPIWithTrail url (API.placeDetails apiKey placeId fields) "placeDetails"
  case res of
    Right x -> return x
    Left cliErr -> do
      let err = fromClientError cliErr
      logError "client Google maps API placeDetails call error" $ (err ^. #_message) ?: "Some error"
      throwError500 "Google maps API placeDetails error"

getPlaceName ::
  ( HasField "dbCfg" r DBConfig,
    HasField "traceFlag" r TraceFlag,
    HasLogContext r
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  FlowR r GoogleMaps.GetPlaceNameResp
getPlaceName url latLng apiKey = do
  res <- callAPIWithTrail url (API.getPlaceName latLng apiKey) "getPlaceName"
  case res of
    Right x -> return x
    Left cliErr -> do
      let err = fromClientError cliErr
      logError "client Google maps API getPlaceName call error" $ (err ^. #_message) ?: "Some error"
      throwError500 "Google maps API getPlaceName error"
