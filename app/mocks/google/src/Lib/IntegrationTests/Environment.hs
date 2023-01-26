module Lib.IntegrationTests.Environment where

import Beckn.External.Encryption (EncTools (..), encrypt')
import Beckn.External.Maps
import Beckn.Prelude
import Beckn.Utils.Dhall (FromDhall, readDhallConfig)
import Lib.GoogleConfig (GoogleCfgUnencrypted (..))

buildGoogleConfig :: MonadIO m => EncTools -> GoogleCfgUnencrypted -> m MapsServiceConfig
buildGoogleConfig encTools GoogleCfgUnencrypted {..} = do
  googleKeyEncrypted <- encrypt' encTools googleKey
  pure $
    GoogleConfig
      GoogleCfg
        { googleKey = googleKeyEncrypted,
          ..
        }

data AppCfg = AppCfg
  { googleCfg :: GoogleCfgUnencrypted,
    encTools :: EncTools
  }
  deriving (Generic, FromDhall)

readConfig :: String -> IO AppCfg
readConfig pathPrefix = readDhallConfig $ pathPrefix <> "../dhall-configs/dev/integration-tests.dhall"
