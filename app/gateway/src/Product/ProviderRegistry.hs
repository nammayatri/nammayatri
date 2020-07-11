module Product.ProviderRegistry
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
      <$> (fromMaybe "localhost" <$> SE.lookupEnv "PROVIDER_HOST")
      <*> (fromMaybe 8014 . (>>= readMaybe) <$> SE.lookupEnv "PROVIDER_PORT")
