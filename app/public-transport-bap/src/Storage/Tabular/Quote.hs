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
import Storage.Tabular.FerryStation (FerryStationTId)
import Storage.Tabular.Search (SearchTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      searchId SearchTId
      bppId Text
      bppUrl Text
      description Text
      fare Amount
      departureTime UTCTime
      arrivalTime UTCTime
      departureStationId FerryStationTId
      arrivalStationId FerryStationTId
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
          bppUrl = bppUrl_,
          departureStationId = fromKey departureStationId,
          arrivalStationId = fromKey arrivalStationId,
          ..
        }
  toTType Domain.Quote {..} = do
    QuoteT
      { id = id.getId,
        searchId = toKey searchId,
        bppUrl = T.pack $ showBaseUrl bppUrl,
        departureStationId = toKey departureStationId,
        arrivalStationId = toKey arrivalStationId,
        ..
      }
  toTEntity a = do
    Entity (toKey a.id) $ toTType a
