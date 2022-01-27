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
import Database.Persist.TH
import qualified Domain.Types.Quote as Domain
import Storage.Tabular.Search (SearchTId)
import Storage.Tabular.TransportStation (TransportStationTId)

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
      departureStationId TransportStationTId
      arrivalStationId TransportStationTId
      createdAt UTCTime
      routeCode Text
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteT where
  type DomainKey QuoteT = Id Domain.Quote
  fromKey (QuoteTKey _id) = Id _id
  toKey id = QuoteTKey id.getId

instance TEntity QuoteT Domain.Quote where
  fromTEntity entity = do
    let QuoteT {..} = entityVal entity
    bppUrl_ <- parseBaseUrl bppUrl
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
        bppUrl = showBaseUrl bppUrl,
        departureStationId = toKey departureStationId,
        arrivalStationId = toKey arrivalStationId,
        ..
      }
  toTEntity a = do
    Entity (toKey a.id) $ toTType a
