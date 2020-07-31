module Storage.Queries.Dunzo where

import App.Types
import Beckn.Storage.Redis.Queries
import EulerHS.Prelude
import External.Dunzo.Types as T

insertToken :: Token -> Flow ()
insertToken token =
  setExRedis "Dunzo_token" (T.getToken token) 3600 -- 1 hr

getToken :: Flow (Maybe Token)
getToken = do
  mtext <- getKeyRedis "Dunzo_token"
  return $ Token <$> mtext
