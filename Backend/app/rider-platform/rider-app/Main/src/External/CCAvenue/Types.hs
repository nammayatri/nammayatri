module External.CCAvenue.Types where

import Kernel.External.Encryption (EncKind (..), EncryptedField)
import Kernel.Prelude

data VendorSplit = VendorSplit
  { subAccId :: Text,
    percentage :: Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CCAvenueSplitPayoutConfig = CCAvenueSplitPayoutConfig
  { gatewayUrl :: BaseUrl,
    accessCode :: Text,
    workingKey :: EncryptedField 'AsEncrypted Text,
    lookbackDays :: Int,
    vendorSplits :: [VendorSplit],
    splitTdrChargeType :: Text,
    merComm :: Text,
    queryPageSize :: Maybe Int,
    runAtHourUtc :: Maybe Int,
    enabled :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
