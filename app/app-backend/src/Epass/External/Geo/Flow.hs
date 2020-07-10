module Epass.External.Geo.Flow where

import Beckn.Types.Common
import qualified Data.Text as T
import qualified Epass.External.Geo.API as API
import Epass.External.Geo.Types
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant.Client
import System.Environment

defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl Https "maps.googleapis.com" 443 ""

getLocation :: BaseUrl -> Text -> Text -> Flow (Maybe ReverseGeoResp)
getLocation url lat long = do
  key <- L.runIO $ T.pack <$> getEnv "GEOLOCATION_TOKEN"
  res <- L.callAPI url $ API.getLocation lat long key
  case res of
    Right v -> return $ Just v
    Left err -> L.logError "Unable to get the location: " (show err) $> Nothing

getPostalCode :: ReverseGeoResp -> Text
getPostalCode ReverseGeoResp {..} = do
  let filterAddress Address {..} =
        any (\x -> T.isInfixOf "postal" x || T.isInfixOf "pin" x) types
  maybe "NOT_FOUND" short_name (find filterAddress address_components)
