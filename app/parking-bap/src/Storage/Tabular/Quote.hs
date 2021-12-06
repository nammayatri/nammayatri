{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Data.Text as T
import Database.Persist.TH
import qualified Domain.Quote as Domain
import Servant.Client
import Storage.Tabular.ParkingLocation (ParkingLocationTId)
import Storage.Tabular.Search (SearchTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      searchId SearchTId
      bppId Text
      bppUrl Text
      parkingSpaceName Text
      parkingLocationId ParkingLocationTId
      parkingLocationIdFromBpp Text
      fare Amount
      availableSpaces Int
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteT Domain.Quote where
  fromKey (QuoteTKey _id) = Id _id
  toKey id = QuoteTKey id.getId

instance TEntity QuoteT Domain.Quote where
  fromTEntity entity = do
    let QuoteT {..} = entityVal entity
    bppUrl_ <- parseBaseUrl $ T.unpack bppUrl
    return $
      Domain.Quote
        { id = Id id,
          searchId = fromKey searchId,
          parkingLocationId = fromKey parkingLocationId,
          bppUrl = bppUrl_,
          ..
        }
  toTType Domain.Quote {..} = do
    QuoteT
      { id = id.getId,
        searchId = toKey searchId,
        parkingLocationId = toKey parkingLocationId,
        bppUrl = T.pack $ showBaseUrl bppUrl,
        ..
      }
  toTEntity a = do
    Entity (toKey a.id) $ toTType a
