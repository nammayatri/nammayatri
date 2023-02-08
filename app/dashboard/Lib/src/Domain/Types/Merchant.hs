module Domain.Types.Merchant where

import qualified Domain.Types.ServerName as DSN
import Kernel.Prelude
import Kernel.Types.Id

data Merchant = Merchant
  { id :: Id Merchant,
    shortId :: ShortId Merchant,
    serverName :: DSN.ServerName,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
