module Product.AppLookup
  ( lookup,
  )
where

import Beckn.Types.Common (Flow)
import qualified Beckn.Types.Core.Context as B
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified System.Environment as SE

-- TODO: All of it
lookup :: B.Context -> Flow (String, Int)
lookup _ = do
  L.runIO $
    (,)
      <$> (fromMaybe "localhost" <$> SE.lookupEnv "APP_HOST")
      <*> (fromMaybe 8013 . (>>= readMaybe) <$> SE.lookupEnv "APP_PORT")
