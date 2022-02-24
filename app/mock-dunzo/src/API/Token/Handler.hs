module API.Token.Handler where

import qualified API.Fixtures as Fixtures
import App.Types
import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API
import qualified "fmd-wrapper" Types.Common as Common

handler :: Maybe Text -> Maybe Text -> FlowHandler API.TokenRes
handler mClientId mClientSecret = withFlowHandlerAPI $ do
  clientId <- pure (Common.ClientId <$> mClientId) >>= fromMaybeM (InvalidRequest "client-id header is not provided")
  clientSecret <- pure (Common.ClientSecret <$> mClientSecret) >>= fromMaybeM (InvalidRequest "client-secret header is not provided")
  unless ((clientId == Fixtures.clientId) && (clientSecret == Fixtures.clientSecret)) $ throwError InvalidAuthData
  pure
    API.TokenRes
      { token = Fixtures.token
      }
