{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchRequest where

import qualified Domain.Types.SearchRequest as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import qualified Storage.Tabular.Merchant as SMerchant
import qualified Storage.Tabular.Person as SP
import qualified Storage.Tabular.SearchRequest.SearchReqLocation as SLoc

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestT sql=search_request
      id Text
      startTime UTCTime
      validTill UTCTime
      riderId SP.PersonTId
      fromLocationId SLoc.SearchReqLocationTId
      toLocationId SLoc.SearchReqLocationTId Maybe
      distance Centesimal Maybe
      merchantId SMerchant.MerchantTId
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestT where
  type DomainKey SearchRequestT = Id Domain.SearchRequest
  fromKey (SearchRequestTKey _id) = Id _id
  toKey (Id id) = SearchRequestTKey id

type FullSearchRequestT = (SearchRequestT, SLoc.SearchReqLocationT, Maybe SLoc.SearchReqLocationT)

instance TType FullSearchRequestT Domain.SearchRequest where
  fromTType (SearchRequestT {..}, fromLoc, mbToLoc) = do
    fromLocation <- fromTType fromLoc
    toLocation <- mapM fromTType mbToLoc
    return $
      Domain.SearchRequest
        { id = Id id,
          riderId = fromKey riderId,
          distance = HighPrecMeters <$> distance,
          merchantId = fromKey merchantId,
          ..
        }
  toTType Domain.SearchRequest {..} = do
    let fromLoc = toTType fromLocation
        mbToLoc = toTType <$> toLocation
        searchReq =
          SearchRequestT
            { id = getId id,
              riderId = toKey riderId,
              fromLocationId = toKey fromLocation.id,
              toLocationId = toKey <$> (toLocation <&> (.id)),
              distance = getHighPrecMeters <$> distance,
              merchantId = toKey merchantId,
              ..
            }
    (searchReq, fromLoc, mbToLoc)
