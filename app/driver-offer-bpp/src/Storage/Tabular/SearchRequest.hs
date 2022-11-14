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
import Beckn.Utils.Common hiding (id)
import qualified Domain.Types.SearchRequest as Domain
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.SearchRequest.SearchReqLocation (SearchReqLocationT, SearchReqLocationTId, mkDomainSearchReqLocation, mkTabularSearchReqLocation)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestT sql=search_request
      id Text
      transactionId Text
      messageId Text
      startTime UTCTime
      validTill UTCTime
      providerId OrganizationTId
      fromLocationId SearchReqLocationTId
      toLocationId SearchReqLocationTId
      bapId Text
      bapUri Text
      estimatedDistance Meters
      estimatedDuration Seconds
      createdAt UTCTime
      vehicleVariant Variant.Variant
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestT where
  type DomainKey SearchRequestT = Id Domain.SearchRequest
  fromKey (SearchRequestTKey _id) = Id _id
  toKey (Id id) = SearchRequestTKey id

instance TType (SearchRequestT, SearchReqLocationT, SearchReqLocationT) Domain.SearchRequest where
  fromTType (SearchRequestT {..}, fromLoc, toLoc) = do
    pUrl <- parseBaseUrl bapUri
    let fromLoc_ = mkDomainSearchReqLocation fromLoc
        toLoc_ = mkDomainSearchReqLocation toLoc

    return $
      Domain.SearchRequest
        { id = Id id,
          providerId = fromKey providerId,
          fromLocation = fromLoc_,
          toLocation = toLoc_,
          bapUri = pUrl,
          ..
        }
  toTType Domain.SearchRequest {..} =
    ( SearchRequestT
        { id = getId id,
          providerId = toKey providerId,
          fromLocationId = toKey fromLocation.id,
          toLocationId = toKey toLocation.id,
          bapUri = showBaseUrl bapUri,
          ..
        },
      mkTabularSearchReqLocation fromLocation,
      mkTabularSearchReqLocation toLocation
    )
