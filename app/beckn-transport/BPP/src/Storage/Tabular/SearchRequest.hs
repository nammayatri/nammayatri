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

derivePersistField "Domain.SearchRequestStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestT sql=search_request
      id Text
      messageId Text
      startTime UTCTime
      validTill UTCTime
      providerId OrganizationTId
      fromLocationId SearchReqLocationTId
      toLocationId SearchReqLocationTId Maybe
      bapId Text
      bapUri Text
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestT where
  type DomainKey SearchRequestT = Id Domain.SearchRequest
  fromKey (SearchRequestTKey _id) = Id _id
  toKey (Id id) = SearchRequestTKey id

instance TType SearchRequestT Domain.SearchRequest where
  fromTType SearchRequestT {..} = do
    pUrl <- parseBaseUrl bapUri
    return $
      Domain.SearchRequest
        { id = Id id,
          providerId = fromKey providerId,
          fromLocationId = fromKey fromLocationId,
          toLocationId = fromKey <$> toLocationId,
          bapUri = pUrl,
          ..
        }
  toTType Domain.SearchRequest {..} =
    SearchRequestT
      { id = getId id,
        providerId = toKey providerId,
        fromLocationId = toKey fromLocationId,
        toLocationId = toKey <$> toLocationId,
        bapUri = showBaseUrl bapUri,
        ..
      }
