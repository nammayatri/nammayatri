module Domain.Organization where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.DunzoCreds

data Organization = Organization
  { id :: Id Organization,
    shortId :: ShortId Organization,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    dunzoCredsId :: Id DunzoCreds
  }
