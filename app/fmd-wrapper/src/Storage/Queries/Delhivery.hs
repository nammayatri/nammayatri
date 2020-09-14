module Storage.Queries.Delhivery where

import App.Types
import Beckn.Storage.Redis.Queries
import EulerHS.Prelude
import Types.Common as T

insertToken :: T.ClientId -> Token -> Flow ()
insertToken clientId token =
  setExRedis ("Delhivery_token_" <> getClientId clientId) (T.getToken token) 300 -- 5 min

getToken :: T.ClientId -> Flow (Maybe Token)
getToken clientId = do
  mtext <- getKeyRedis ("Delhivery_token_" <> getClientId clientId)
  return $ Token <$> mtext
