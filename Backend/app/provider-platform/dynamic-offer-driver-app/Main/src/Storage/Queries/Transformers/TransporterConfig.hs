{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.TransporterConfig where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Version
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)

fallbackVersion :: Version
fallbackVersion =
  Version
    { major = 9999,
      minor = 9999,
      maintenance = 9999,
      preRelease = Nothing,
      build = Nothing
    }

fallBackVersionInText :: Text
fallBackVersionInText = versionToText fallbackVersion
