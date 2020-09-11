module Storage.Queries.Dunzo where

import App.Types
import Beckn.Storage.Redis.Queries
import EulerHS.Prelude
import External.Dunzo.Types as T

insertToken :: T.ClientId -> Token -> Flow ()
insertToken clientId token =
  setExRedis ("Dunzo_token_" <> getClientId clientId) (T.getToken token) 3600 -- 1 hr

getToken :: T.ClientId -> Flow (Maybe Token)
getToken clientId = do
  mtext <- getKeyRedis ("Dunzo_token_" <> getClientId clientId)
  return $ Token <$> mtext
