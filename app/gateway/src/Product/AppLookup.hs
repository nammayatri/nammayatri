module Product.AppLookup
  ( lookup,
  )
where

import qualified Beckn.Types.Core.Context as B
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import qualified System.Environment as SE

-- TODO: All of it
lookup :: B.Context -> EL.Flow (String, Int)
lookup _ = do
  EL.runIO $
    (,)
      <$> (fromMaybe "localhost" <$> SE.lookupEnv "APP_HOST")
      <*> (fromMaybe 8013 . (>>= readMaybe) <$> SE.lookupEnv "APP_PORT")
