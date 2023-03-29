{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib.Maps.MMI.MMIAuthToken where

import Data.Maybe
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common
import Lib.Encryption
import Lib.Error as ER
import Lib.Maps.MMI.Config
import Lib.Maps.MMI.MapsClient.Types as MMI
import Servant hiding (throwError)

type MMIAuthAPI =
  ReqBody '[FormUrlEncoded] MMI.AuthRequest
    :> Post '[JSON] MMI.AuthResp

data AccessToken = AccessToken
  { mmiAccessToken :: !Text,
    mmiTokenType :: !Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

mmiAuthAPI :: Proxy MMIAuthAPI
mmiAuthAPI = Proxy

mmiAuthToken ::
  ( CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    EncFlow m r
  ) =>
  MMICfg ->
  m MMI.AuthResp
mmiAuthToken mmiCfg = do
  secretKey <- decrypt mmiCfg.mmiAuthSecret
  let url = mmiCfg.mmiAuthUrl
      clientId = mmiCfg.mmiAuthId
      grantType = "client_credentials"
      authReq = MMI.AuthRequest grantType clientId secretKey
  callMMIAPI
    url
    (callMMIAuth authReq)
    "mmi-auto-suggest"
  where
    callMMIAuth authReq = ET.client mmiAuthAPI authReq

callMMIAPI :: CallAPI env a
callMMIAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_AUTH_ERROR")

redisMMIKey :: Text
redisMMIKey = "Core:mmi_token"

-- | Get MMI token
getMMIToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  MMICfg ->
  m AccessToken
getMMIToken config = do
  tokenStatus :: Maybe AccessToken <- Redis.get (config.mmiAuthId <> ":" <> redisMMIKey)
  case tokenStatus of
    Nothing -> refreshToken config
    Just token -> pure token

getTokenText ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  MMICfg ->
  m Text
getTokenText mfg = do
  token <- getMMIToken mfg
  pure $ mmiTokenType token <> " " <> mmiAccessToken token

refreshToken ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  MMICfg ->
  m AccessToken
refreshToken config = do
  res <- mmiAuthToken config
  let accessToken =
        AccessToken
          { mmiAccessToken = res.accessToken,
            mmiTokenType = res.tokenType
          }
  Redis.set (config.mmiAuthId <> ":" <> redisMMIKey) accessToken
  pure accessToken
