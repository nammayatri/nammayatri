{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchRequest.SearchReqLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.SearchRequest.SearchReqLocation as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchReqLocationT sql=search_request_location
      id Text
      lat Double
      lon Double
      street Text Maybe
      door Text Maybe
      city Text Maybe
      state Text Maybe
      country Text Maybe
      building Text Maybe
      areaCode Text Maybe
      area Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchReqLocationT where
  type DomainKey SearchReqLocationT = Id Domain.SearchReqLocation
  fromKey (SearchReqLocationTKey _id) = Id _id
  toKey (Id id) = SearchReqLocationTKey id

mkDomainSearchReqLocation :: SearchReqLocationT -> Domain.SearchReqLocation
mkDomainSearchReqLocation SearchReqLocationT {..} =
  Domain.SearchReqLocation
    { id = Id id,
      ..
    }

mkTabularSearchReqLocation :: Domain.SearchReqLocation -> SearchReqLocationT
mkTabularSearchReqLocation Domain.SearchReqLocation {..} =
  SearchReqLocationT
    { id = getId id,
      ..
    }
