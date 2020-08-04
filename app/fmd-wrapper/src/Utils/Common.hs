{-# LANGUAGE OverloadedLabels #-}

module Utils.Common where

import App.Types
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (decodeFromText, fromMaybeM500)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Servant.Client as S (BaseUrl (..), parseBaseUrl)

getClientConfig :: FromJSON a => Organization -> Flow a
getClientConfig org =
  let mconfig = org ^. #_info >>= decodeFromText
   in fromMaybeM500 "CLIENT_CONFIG_DECODE_ERROR" mconfig

parseBaseUrl :: Text -> Flow S.BaseUrl
parseBaseUrl url =
  L.runIO $
    S.parseBaseUrl $ T.unpack url

fork :: Text -> Flow () -> Flow ()
fork desc f = do
  env <- ask
  lift $ L.forkFlow desc $ runReaderT f env
