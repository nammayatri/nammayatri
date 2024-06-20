{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AppInstalls where

import Data.Aeson
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Tools.Beam.UtilsTH

data AppInstalls = AppInstalls
  { id :: Kernel.Types.Id.Id Domain.Types.AppInstalls.AppInstalls,
    deviceToken :: Kernel.Prelude.Text,
    source :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    appVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    bundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    platform :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
