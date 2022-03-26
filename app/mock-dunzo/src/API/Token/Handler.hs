module API.Token.Handler where

import qualified API.Error as Error
import qualified API.Fixtures as Fixtures
import App.Types
import Beckn.Prelude
import Beckn.Utils.Common hiding (withFlowHandlerAPI)
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API
import Tools.FlowHandling
import qualified "fmd-wrapper" Types.Common as Common

handler :: Maybe Text -> Maybe Text -> FlowHandler API.TokenRes
handler mClientId mClientSecret = withFlowHandlerAPI $ do
  clientId <- pure (Common.ClientId <$> mClientId) >>= fromMaybeM Error.unauthorized
  clientSecret <- pure (Common.ClientSecret <$> mClientSecret) >>= fromMaybeM Error.unauthorized
  unless ((clientId == Fixtures.clientId) && (clientSecret == Fixtures.clientSecret)) $ throwError Error.unauthorized
  pure
    API.TokenRes
      { token = Fixtures.token
      }
