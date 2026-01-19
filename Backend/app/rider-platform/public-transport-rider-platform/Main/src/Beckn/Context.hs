{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Context where

import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Predicate
import Kernel.Types.TimeRFC339
import Kernel.Utils.Validation

coreConfig :: CoreConfig
coreConfig =
  CoreConfig
    { version = "0.9.3",
      domain = Context.PUBLIC_TRANSPORT,
      country = Context.India,
      city = Context.City "Kochi"
    }

buildContext ::
  (MonadTime m, MonadGuid m) =>
  Context.Action ->
  Text ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  m Context.Context
buildContext = buildContext' coreConfig

validateContext :: (MonadThrow m, Log m) => Context.Action -> Context.Context -> m ()
validateContext = validateContext' coreConfig

-- TODO We can move this common code to the lib

data CoreConfig = CoreConfig
  { version :: Text,
    domain :: Context.Domain,
    country :: Context.Country,
    city :: Context.City
  }

buildContext' ::
  (MonadTime m, MonadGuid m) =>
  CoreConfig ->
  Context.Action ->
  Text ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  m Context.Context
buildContext' config action txnId bapId bapUri bppId bppUri = do
  timestamp <- UTCTimeRFC3339 <$> getCurrentTime
  message_id <- generateGUIDText
  return
    Context.Context
      { domain = config.domain,
        country = config.country,
        city = config.city,
        action = action,
        core_version = config.version,
        bap_id = bapId,
        bap_uri = bapUri,
        bpp_id = bppId,
        bpp_uri = bppUri,
        transaction_id = Just txnId,
        message_id = message_id,
        timestamp = timestamp,
        max_callbacks = Nothing
      }

validateContext' :: (MonadThrow m, Log m) => CoreConfig -> Context.Action -> Context.Context -> m ()
validateContext' config action' = runRequestValidation validator
  where
    validator Context.Context {..} =
      sequenceA_
        [ validateField "domain" domain $ Exact config.domain,
          validateField "action" action $ Exact action',
          validateField "core_version" core_version $ Exact config.version,
          validateField "country" country $ Exact config.country
        ]
