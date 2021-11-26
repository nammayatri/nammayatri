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
    additionalInfo :: Text,
    bppId :: Text,
    bppUrl :: BaseUrl,
    parkingSpaceName :: Text,
    parkingSpaceLocationId :: Text,
    parkingSupportNumber :: Text,
    fare :: Amount,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    availableSpaces :: Int,
    maxSpaces :: Int,
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
          searchId = let (SearchTKey _id) = quoteTSearchId in Id _id,
          additionalInfo = quoteTAdditionalInfo,
          bppId = quoteTBppId,
          bppUrl = bppUrl,
          parkingSpaceName = quoteTParkingSpaceName,
          parkingSpaceLocationId = quoteTParkingSpaceLocationId,
          parkingSupportNumber = quoteTParkingSupportNumber,
          fare = quoteTFare,
          fromDate = quoteTFromDate,
          toDate = quoteTToDate,
          availableSpaces = quoteTAvailableSpaces,
          maxSpaces = quoteTMaxSpaces,
          createdAt = quoteTCreatedAt
        }
  toTType Quote {..} = do
    QuoteT
      { quoteTSearchId = SearchTKey searchId.getId,
        quoteTAdditionalInfo = additionalInfo,
        quoteTBppId = bppId,
        quoteTBppUrl = T.pack $ showBaseUrl bppUrl,
        quoteTParkingSpaceName = parkingSpaceName,
        quoteTParkingSpaceLocationId = parkingSpaceLocationId,
        quoteTParkingSupportNumber = parkingSupportNumber,
        quoteTFare = fare,
        quoteTFromDate = fromDate,
        quoteTToDate = toDate,
        quoteTAvailableSpaces = availableSpaces,
        quoteTMaxSpaces = maxSpaces,
        quoteTCreatedAt = createdAt
      }
  toTEntity a = do
    Entity (toKey a) $ toTType a
