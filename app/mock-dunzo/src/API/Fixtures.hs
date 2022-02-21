module API.Fixtures where

import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API
import qualified "fmd-wrapper" Types.Common as Common

verifyToken :: (Log m, MonadThrow m) => Maybe Common.Token -> Maybe Common.ClientId -> m ()
verifyToken mToken mClientId = do
  case mToken of
    Nothing -> throwError $ TokenNotFound ""
    Just currToken@(Common.Token txtToken) -> do
      when (currToken /= token) $ throwError (InvalidToken txtToken)
      when (mClientId /= Just clientId) $ throwError InvalidAuthData

clientId :: Common.ClientId
clientId = Common.ClientId "mock-client-id"

clientSecret :: Common.ClientSecret
clientSecret = Common.ClientSecret "mock-client-secret"

token :: Common.Token
token = Common.Token "mock-token"

eta :: API.Eta
eta =
  API.Eta
    { pickup = Just 15,
      dropoff = 35
    }

eta2 :: API.Eta
eta2 =
  API.Eta
    { pickup = Just 2.6,
      dropoff = 35.7
    }
