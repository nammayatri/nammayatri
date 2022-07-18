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
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.SearchRequest.SearchReqLocation (SearchReqLocationT, SearchReqLocationTId, mkDomainSearchReqLocation, mkTabularSearchReqLocation)

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
      createdAt UTCTime
      fareParametersId Fare.FareParametersTId

      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestT where
  type DomainKey SearchRequestT = Id Domain.SearchRequest
  fromKey (SearchRequestTKey _id) = Id _id
  toKey (Id id) = SearchRequestTKey id

instance TType (SearchRequestT, SearchReqLocationT, SearchReqLocationT, Fare.FareParametersT) Domain.SearchRequest where
  fromTType (SearchRequestT {..}, fromLoc, toLoc, fareParametersT) = do
    pUrl <- parseBaseUrl bapUri
    let fromLoc_ = mkDomainSearchReqLocation fromLoc
        toLoc_ = mkDomainSearchReqLocation toLoc
        fareParams = Fare.mkDomainFromTabularFareParams fareParametersT

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
    let fareParamsId = cast id
     in ( SearchRequestT
            { id = getId id,
              providerId = toKey providerId,
              fromLocationId = toKey fromLocation.id,
              toLocationId = toKey toLocation.id,
              bapUri = showBaseUrl bapUri,
              fareParametersId = toKey fareParamsId,
              ..
            },
          mkTabularSearchReqLocation fromLocation,
          mkTabularSearchReqLocation toLocation,
          Fare.mkTabularFromDomainFareParams fareParamsId fareParams
        )
