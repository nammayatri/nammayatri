{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Production 'SessionStore' over Redis (@session/manager.ts@). Keyed by the
-- engine-scoped session id (the engine namespaces by merchantLabel:phone, so this
-- adapter just prefixes). @resolveSession@ create-or-refreshes (++messageCount),
-- resetting the TTL on every touch; @saveContext@ is a silent no-op when the
-- session is absent/expired (manager.ts:63-64), so callers resolveSession first.
module WhatsappBot.Adapter.SessionStore (mkSessionStore) where

import Environment (Flow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common (generateGUID, getCurrentTime)
import WhatsappBot.Handles (SessionStore (..))
import WhatsappBot.Types (Session (..), initialContext)

sessionKey :: Text -> Text
sessionKey sid = "wab:session:" <> sid

-- | @ttlSec@ = the refresh-on-access session TTL (metaSessionTtlSec).
mkSessionStore :: Int -> SessionStore Flow
mkSessionStore ttlSec =
  SessionStore
    { resolveSession = \sid -> do
        let key = sessionKey sid
        now <- getCurrentTime
        mSess <- Redis.get key
        case mSess of
          Just sess -> do
            let sess' = sess {lastActiveAt = now, messageCount = sess.messageCount + 1}
            Redis.setExp key sess' ttlSec
            pure sess'
          Nothing -> do
            gid <- generateGUID
            let sess =
                  Session
                    { sessionId = gid,
                      source = "whatsapp",
                      userId = sid,
                      createdAt = now,
                      lastActiveAt = now,
                      messageCount = 1,
                      metadata = initialContext
                    }
            Redis.setExp key sess ttlSec
            pure sess,
      getContext = \sid -> do
        mSess <- Redis.get (sessionKey sid)
        pure ((\(s :: Session) -> s.metadata) <$> mSess),
      saveContext = \sid ctx -> do
        let key = sessionKey sid
        mSess <- Redis.get key
        whenJust mSess $ \sess -> do
          now <- getCurrentTime
          Redis.setExp key (sess {metadata = ctx, lastActiveAt = now}) ttlSec
    }
