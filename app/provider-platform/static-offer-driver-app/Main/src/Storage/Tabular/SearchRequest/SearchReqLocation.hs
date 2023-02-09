{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchRequest.SearchReqLocation where

import qualified Domain.Types.SearchRequest.SearchReqLocation as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchReqLocationT sql=search_request_location
      id Text
      lat Double
      lon Double
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchReqLocationT where
  type DomainKey SearchReqLocationT = Id Domain.SearchReqLocation
  fromKey (SearchReqLocationTKey _id) = Id _id
  toKey (Id id) = SearchReqLocationTKey id

instance TType SearchReqLocationT Domain.SearchReqLocation where
  fromTType SearchReqLocationT {..} = do
    return $
      Domain.SearchReqLocation
        { id = Id id,
          ..
        }
  toTType Domain.SearchReqLocation {..} =
    SearchReqLocationT
      { id = getId id,
        ..
      }
