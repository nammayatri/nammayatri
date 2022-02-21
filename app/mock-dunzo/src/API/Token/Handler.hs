module API.Token.Handler where

import qualified API.Fixtures as Fixtures
import App.Types
import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API
import qualified "fmd-wrapper" Types.Common as Common

-- FIXME: Maybe we should use more simple structure without `withFlowHandlerAPI` wrapper in mock?
handler :: Maybe Text -> Maybe Text -> FlowHandler API.TokenRes
handler mClientId mClientSecret = withFlowHandlerAPI $ do
  if (Common.ClientId <$> mClientId) == Just Fixtures.clientId
    && (Common.ClientSecret <$> mClientSecret) == Just Fixtures.clientSecret
    then
      pure
        -- FIXME: Do we need to generate new token each time?
        API.TokenRes
          { token = Fixtures.token
          }
    else throwError InvalidAuthData
