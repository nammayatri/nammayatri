module Utils.Context where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.TimeRFC339
import Data.String.Conversions
import Domain.Types.Organization (Organization (..))
import Servant.Client

defaultCountry :: Text
defaultCountry = "IND"

defaultCity :: Text
defaultCity = "Bangalore"

contextTemplate ::
  ( MonadTime m,
    MonadReader r m,
    HasField "coreVersion" r Text,
    HasField "nwAddress" r BaseUrl
  ) =>
  Organization ->
  Action ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Text ->
  m Context
contextTemplate org action bap_id bap_uri transaction_id message_id = do
  now <- UTCTimeRFC3339 <$> getCurrentTime
  core_version <- asks (.coreVersion)
  nwAddress <- asks (.nwAddress)
  let bpp_id = org.shortId.getShortId
  let bpp_uri = nwAddress {baseUrlPath = baseUrlPath nwAddress <> "/" <> cs org.id.getId}
  pure
    Context
      { domain = MOBILITY,
        country = defaultCountry,
        city = defaultCity,
        timestamp = now,
        bpp_id = Just bpp_id,
        bpp_uri = Just bpp_uri,
        ..
      }
