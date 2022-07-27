{-# OPTIONS_GHC -Wno-deprecations #-}

module Core.Beckn where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Data.List (lookup)
import qualified Data.Text.Encoding as T
import EulerHS.Prelude
import qualified Network.Wai.Internal as Wai
import Servant

logBecknRequest :: AppEnv -> Application -> Application
logBecknRequest appEnv f req@Wai.Request {..} respF = do
  req' <- case lookup "Authorization" requestHeaders of
    Nothing -> return req
    Just header -> do
      body <- requestBody
      bodyMvar <- newMVar body
      let logEnv = appEnv.loggerEnv
          esqDBEnv = appEnv.esqDBEnv
      Esq.runTransactionIO logEnv esqDBEnv $ do
        QBR.logBecknRequest (T.decodeUtf8 body) (T.decodeUtf8 header)
      return req {Wai.requestBody = mkRequestBody bodyMvar}
  f req' respF
  where
    mkRequestBody mvar = tryTakeMVar mvar <&> fromMaybe mempty
