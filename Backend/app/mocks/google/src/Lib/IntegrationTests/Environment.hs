{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.IntegrationTests.Environment where

import Kernel.External.Encryption (EncTools (..), encrypt')
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerCfg)
import Kernel.Types.Common (HighPrecMeters)
import Kernel.Utils.Dhall (FromDhall, readDhallConfig)
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
    encTools :: EncTools,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    appPrefix :: Text,
    kafkaProducerCfg :: KafkaProducerCfg
  }
  deriving (Generic, FromDhall)

readConfig :: String -> IO AppCfg
readConfig pathPrefix = readDhallConfig $ pathPrefix <> "../dhall-configs/dev/integration-tests.dhall"
