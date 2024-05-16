{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.AppInstalls where

import Data.Text
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Version
import Kernel.Utils.Common (MonadFlow, fromMaybeM, getCurrentTime)
import Kernel.Utils.Version

readAppVersion :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Kernel.Prelude.Maybe Kernel.Types.Version.Version)
readAppVersion appVersion = mapM readVersion (strip <$> appVersion)

readBundleVersion :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Kernel.Prelude.Maybe Kernel.Types.Version.Version)
readBundleVersion bundleVersion = mapM readVersion (strip <$> bundleVersion)
