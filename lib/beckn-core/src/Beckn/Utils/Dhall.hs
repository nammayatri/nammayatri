{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Utils.Dhall
  ( module Dhall,
    readDhallConfig,
    readDhallConfigDefault,
    z,
    ZL (..),
  )
where

import Data.Char (toUpper)
#if MIN_VERSION_dhall (1,33,0)
import qualified Data.Either.Validation as V
#endif
import Dhall hiding (map)
import qualified Dhall.Map as DM
import EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified Lens.Family as Lens
import Servant.Client (BaseUrl, Scheme)
import System.Environment (lookupEnv)

-- Temp crutches for old Dhalls
-- FIXME!!! Remove when build environment is updated!
#if !MIN_VERSION_dhall (1,33,0)
type ExpectedTypeErrors = ()
deriving instance Exception ()
#endif

-- Small lib to do input with Haskell types environment
injectType ::
  forall a.
  FromDhall a =>
  Proxy a ->
  Text ->
  EvaluateSettings ->
  Either ExpectedTypeErrors EvaluateSettings
injectType _ tyname es =
  case expected (auto :: Decoder a) of
#if MIN_VERSION_dhall (1,33,0)
    V.Failure e -> Left e
    V.Success
#endif
    {- O -} injType -> Right $ Lens.over substitutions (DM.insert tyname injType) es

-- Use no heavy machinery like Vinyl, quickly concoct our own
data ZL (ts :: [Type]) where
  Z :: ZL '[]
  Zs :: FromDhall t => Proxy t -> Text -> ZL ts -> ZL (t ': ts)

z :: forall t ts. FromDhall t => Text -> ZL ts -> ZL (t ': ts)
z = Zs (Proxy :: Proxy t)

class Inj a where
  inj :: a -> Either ExpectedTypeErrors EvaluateSettings

instance Inj (ZL '[]) where
  inj Z = Right defaultEvaluateSettings

instance Inj (ZL ts) => Inj (ZL (t ': ts)) where
  inj (Zs p t rest) = inj rest >>= injectType p t

-- | Reads config which lies under the given path.
--
-- Also accepts the specification of all Haskell types which Dhall should know
-- about - it will be attached to the configuration so that you don't need to
-- describe the types again in the config.
readDhallConfig :: forall ts b. (Inj (ZL ts), FromDhall b) => ZL ts -> FilePath -> IO b
readDhallConfig tyenv fname =
  either
    throwM
    (\s -> inputFileWithSettings s (auto :: Decoder b) fname)
    $ inj tyenv

-- | Reads config with a given type env. Gets application name as the second argument.
-- E.g. if @appname@ is "mock-provider-backend" the function first looks into "MOCK_PROVIDER_BACKEND_CONFIG_PATH"
-- env variable, if it's not set, it tries to read config from "./config/mock-provider-backend.dhall"
readDhallConfigDefault :: forall ts b. (Inj (ZL ts), FromDhall b) => ZL ts -> String -> IO b
readDhallConfigDefault tyenv appname = do
  fname <- fromMaybe defCfgPath <$> lookupEnv envVarName
  readDhallConfig tyenv fname
  where
    defCfgPath = "./dev/config/" ++ appname ++ ".dhall"
    envVarName = map norm appname ++ "_CONFIG_PATH"
    norm '-' = '_'
    norm c = toUpper c

-----------------------------------------------------

instance {-# OVERLAPS #-} Num a => FromDhall a where
  autoWith inn = fmap fromInteger (autoWith inn :: Decoder Integer)

instance FromDhall Word16 where
  autoWith inn = fmap fromIntegral (autoWith inn :: Decoder Natural)

deriving instance FromDhall Scheme

deriving instance FromDhall BaseUrl

deriving instance FromDhall T.PoolConfig

deriving instance FromDhall T.PostgresConfig

deriving instance FromDhall T.RedisConfig
