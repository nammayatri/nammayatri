{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Transformers.MetaWebhookConfig where

import qualified Data.Aeson
import Domain.Types.Extra.MetaWebhookConfig (MetaBotCfg)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (MonadFlow, fromMaybeM)
import qualified Kernel.Utils.JSON

-- | bot_config is NOT NULL, admin-inserted only (never user input) — a
-- decode failure means DB corruption, so fail closed with a typed error
-- rather than silently defaulting or crashing with a partial function.
readBotConfig :: MonadFlow m => Data.Aeson.Value -> m MetaBotCfg
readBotConfig val =
  Kernel.Utils.JSON.valueToMaybe val
    & fromMaybeM (InternalError "Failed to decode botConfig for MetaWebhookConfig")
