{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AppInstalls where

import Data.Aeson
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Tools.Beam.UtilsTH

data AppInstalls = AppInstalls
  { appVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    bundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    createdAt :: Kernel.Prelude.UTCTime,
    deviceToken :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.AppInstalls.AppInstalls,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    platform :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    source :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
