module Domain.Types.AppInstalls where

import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Version

data AppInstalls = AppInstalls
  { id :: Id AppInstalls,
    deviceToken :: Text,
    source :: Text,
    merchantId :: Id DMerchant.Merchant,
    appVersion :: Maybe Version,
    bundleVersion :: Maybe Version,
    platform :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)
