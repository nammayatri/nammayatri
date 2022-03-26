module Domain.DunzoCreds where

import Beckn.Types.Id
import Types.Common

data DunzoCreds = DunzoCreds
  { id :: Id DunzoCreds,
    clientId :: ClientId,
    clientSecret :: ClientSecret
  }
