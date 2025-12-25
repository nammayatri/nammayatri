module Storage.Queries.Transformers.AppInstalls where

import Data.Text
import Kernel.Prelude
import qualified Kernel.Types.Version
import Kernel.Utils.Common (MonadFlow)
import Kernel.Utils.Version

readAppVersion :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Kernel.Prelude.Maybe Kernel.Types.Version.Version)
readAppVersion appVersion = mapM readVersion (strip <$> appVersion)

readBundleVersion :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Kernel.Prelude.Maybe Kernel.Types.Version.Version)
readBundleVersion bundleVersion = mapM readVersion (strip <$> bundleVersion)
