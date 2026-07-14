{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Meta (lookupMetaCfg) where

import qualified Domain.Types.MetaWebhookConfig as DMWC
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Meta.Config as Meta
import Kernel.Prelude (parseBaseUrl)

-- | Build the (encrypted) Meta WhatsApp Cloud API send-config straight off an
-- already-fetched 'DMWC.MetaWebhookConfig' row — no second DB query. Used to
-- live in @merchant_service_config@'s @Meta_CloudApi@ row (a separate table,
-- looked up by a separate key), which meant this table's @phoneNumberId@ and
-- that row's @phoneNumberId@ had to agree at runtime (drift-guard in
-- Adapter/Env.hs). accessToken/baseUrl/apiVersion moved onto this table
-- (dev/ddl-migrations/rider-app/1561-meta-config-rename-and-access-token.sql
-- + dev/feature-migrations/0047-meta-config-access-token-backfill.sql,
-- tightened to NOT NULL in 1562-meta-config-access-token-not-null.sql) so
-- there's exactly one row and nothing left to drift. The access token stays
-- @EncryptedField@-encrypted here; only @Kernel.External.Meta.Flow.sendMessage@
-- decrypts it, at call time.
lookupMetaCfg :: DMWC.MetaWebhookConfig -> Flow Meta.MetaCfg
lookupMetaCfg cfg = do
  baseUrl <- parseBaseUrl cfg.baseUrl
  pure
    Meta.MetaCfg
      { accessToken = cfg.accessToken,
        phoneNumberId = cfg.phoneNumberId,
        baseUrl = baseUrl,
        apiVersion = cfg.apiVersion
      }
