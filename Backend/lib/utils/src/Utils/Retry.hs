{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Utils.Retry
  ( TransientRetryConfig (..),
    defaultRetryConfig,
    withTransientRetry,
  )
where

import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common

data TransientRetryConfig = TransientRetryConfig
  { maxRetries :: Int,
    baseDelayMs :: Int,
    serviceName :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

defaultRetryConfig :: Text -> TransientRetryConfig
defaultRetryConfig name =
  TransientRetryConfig
    { maxRetries = 3,
      baseDelayMs = 200,
      serviceName = name
    }

withTransientRetry ::
  (MonadFlow m) =>
  TransientRetryConfig ->
  m a ->
  m a
withTransientRetry config action = go 0
  where
    go attempt = do
      result <- withTryCatch ("retry:" <> config.serviceName) action
      case result of
        Right val -> pure val
        Left e -> do
          let errMsg = show e
          if attempt < config.maxRetries && isTransient errMsg
            then do
              let delayMs = config.baseDelayMs * (2 ^ attempt)
              logWarning $
                "Retry [" <> config.serviceName
                  <> "] attempt "
                  <> T.pack (show (attempt + 1))
                  <> "/"
                  <> T.pack (show config.maxRetries)
                  <> " after "
                  <> T.pack (show delayMs)
                  <> "ms"
              threadDelayMilliSec (fromIntegral delayMs)
              go (attempt + 1)
            else throwError $ InternalError $ config.serviceName <> " failed: " <> T.pack errMsg

    isTransient :: String -> Bool
    isTransient err =
      let msg = T.toLower (T.pack err)
       in not (isClientError msg) && (isServerError msg || isConnectionError msg)

    isClientError :: Text -> Bool
    isClientError msg = any (`T.isInfixOf` msg) ["400", "401", "403", "404", "422"]

    isServerError :: Text -> Bool
    isServerError msg = any (`T.isInfixOf` msg) ["500", "502", "503", "504"]

    isConnectionError :: Text -> Bool
    isConnectionError msg = any (`T.isInfixOf` msg) ["timeout", "connection", "econnrefused", "econnreset"]
