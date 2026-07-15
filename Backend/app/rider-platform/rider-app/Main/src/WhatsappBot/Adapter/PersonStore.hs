{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Production 'PersonStore' over Redis (@session/token-store.ts@): the durable
-- per-user record (resolved rider handle + language) and the one-time intro-sent
-- flag. NO TTL — these persist across sessions. Keyed by the engine's userKey
-- (whatsapp:merchantLabel:phone).
module WhatsappBot.Adapter.PersonStore (mkPersonStore) where

import Environment (Flow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import WhatsappBot.Handles (PersonStore (..))

personKey :: Text -> Text
personKey k = "wab:user:" <> k

introKey :: Text -> Text
introKey k = "wab:introsent:" <> k

mkPersonStore :: PersonStore Flow
mkPersonStore =
  PersonStore
    { getPerson = \k -> Redis.get (personKey k),
      setPerson = \k p -> Redis.set (personKey k) p,
      getIntroSent = \k -> fromMaybe False <$> Redis.get (introKey k),
      setIntroSent = \k -> Redis.set (introKey k) True
    }
