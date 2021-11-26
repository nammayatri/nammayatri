{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Domain.Quote
  ( module Storage.Domain.Quote,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.App (BaseUrl)
import Beckn.Types.Id
import qualified Data.Text as T
import Data.Time (UTCTime)
import Servant.Client (parseBaseUrl, showBaseUrl)
import Storage.Domain.Search
import Storage.Tabular.Quote
import Storage.Tabular.Quote as Reexport (QuoteT)
import Storage.Tabular.Quote as Reexport hiding (QuoteT (..))

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

instance TEntityKey QuoteT Quote where
  fromKey (QuoteTKey _id) = Id _id
  toKey Quote {id} = QuoteTKey id.getId

instance TEntity QuoteT Quote where
  fromTEntity entity = do
    let id = fromKey $ entityKey entity
        QuoteT {..} = entityVal entity
    bppUrl <- parseBaseUrl $ T.unpack quoteTBppUrl
    return $
      Quote
        { id = id,
          searchId = fromKey quoteTSearchId,
          bppId = quoteTBppId,
          bppUrl = bppUrl,
          parkingSpaceName = quoteTParkingSpaceName,
          parkingSpaceLocationId = quoteTParkingSpaceLocationId,
          fare = quoteTFare,
          availableSpaces = quoteTAvailableSpaces,
          createdAt = quoteTCreatedAt
        }
  toTType Quote {..} = do
    QuoteT
      { quoteTSearchId = SearchTKey searchId.getId,
        quoteTBppId = bppId,
        quoteTBppUrl = T.pack $ showBaseUrl bppUrl,
        quoteTParkingSpaceName = parkingSpaceName,
        quoteTParkingSpaceLocationId = parkingSpaceLocationId,
        quoteTFare = fare,
        quoteTAvailableSpaces = availableSpaces,
        quoteTCreatedAt = createdAt
      }
  toTEntity a = do
    Entity (toKey a) $ toTType a
