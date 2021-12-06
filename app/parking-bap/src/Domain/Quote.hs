{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.App (BaseUrl)
import Beckn.Types.Id
import Domain.Search

data Quote = Quote
  { id :: Id Quote,
    searchId :: Id Search,
    bppId :: Text,
    bppUrl :: BaseUrl,
    parkingSpaceName :: Text,
    parkingSpaceLocationId :: Text,
    fare :: Amount,
    availableSpaces :: Int,
    createdAt :: UTCTime
  }
