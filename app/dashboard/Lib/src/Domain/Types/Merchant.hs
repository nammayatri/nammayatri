module Domain.Types.Merchant where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.ServerName as DSN

data Merchant = Merchant
  { id :: Id Merchant,
    shortId :: ShortId Merchant,
    serverName :: DSN.ServerName,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
