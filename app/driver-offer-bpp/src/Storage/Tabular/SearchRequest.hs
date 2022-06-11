{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.SearchRequest as Domain
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.SearchReqLocation (SearchReqLocationTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestT sql=search_request
      id Text
      transactionId Text
      messageId Text
      validTill UTCTime
      providerId OrganizationTId
      fromLocationId SearchReqLocationTId
      toLocationId SearchReqLocationTId
      bapId Text
      bapUri Text
      gatewayUri Text
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestT where
  type DomainKey SearchRequestT = Id Domain.SearchRequest
  fromKey (SearchRequestTKey _id) = Id _id
  toKey (Id id) = SearchRequestTKey id

instance TEntity SearchRequestT Domain.SearchRequest where
  fromTEntity entity = do
    let SearchRequestT {..} = entityVal entity
    pUrl <- parseBaseUrl bapUri
    gUrl <- parseBaseUrl gatewayUri
    return $
      Domain.SearchRequest
        { id = Id id,
          providerId = fromKey providerId,
          fromLocationId = fromKey fromLocationId,
          toLocationId = fromKey toLocationId,
          bapUri = pUrl,
          gatewayUri = gUrl,
          ..
        }
  toTType Domain.SearchRequest {..} =
    SearchRequestT
      { id = getId id,
        providerId = toKey providerId,
        fromLocationId = toKey fromLocationId,
        toLocationId = toKey toLocationId,
        bapUri = showBaseUrl bapUri,
        gatewayUri = showBaseUrl gatewayUri,
        ..
      }

  toTEntity a =
    Entity (toKey a.id) $ toTType a
