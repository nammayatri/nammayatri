{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchRequest where

import qualified Domain.Types.SearchRequest as Domain
import Kernel.External.Maps (Language)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Version
import qualified Storage.Tabular.Merchant as SMerchant
import qualified Storage.Tabular.Person as SP

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestT sql=search_request
      id Text
      startTime UTCTime
      validTill UTCTime
      riderId SP.PersonTId
      distance Centesimal Maybe
      maxDistance Centesimal Maybe
      estimatedRideDuration Seconds Maybe
      device Text Maybe
      merchantId SMerchant.MerchantTId
      createdAt UTCTime
      bundleVersion Text Maybe
      clientVersion Text Maybe
      language Language Maybe
      customerExtraFee Money Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestT where
  type DomainKey SearchRequestT = Id Domain.SearchRequest
  fromKey (SearchRequestTKey _id) = Id _id
  toKey (Id id) = SearchRequestTKey id

instance ToTType SearchRequestT Domain.SearchRequestTable where
  toTType Domain.SearchRequestTable {..} = do
    SearchRequestT
      { id = getId id,
        riderId = toKey riderId,
        distance = getHighPrecMeters <$> distance,
        maxDistance = getHighPrecMeters <$> maxDistance,
        merchantId = toKey merchantId,
        bundleVersion = versionToText <$> bundleVersion,
        clientVersion = versionToText <$> clientVersion,
        ..
      }

instance FromTType SearchRequestT Domain.SearchRequestTable where
  fromTType SearchRequestT {..} = do
    bundleVersion' <- forM bundleVersion readVersion
    clientVersion' <- forM clientVersion readVersion
    return $
      Domain.SearchRequestTable
        { id = Id id,
          riderId = fromKey riderId,
          distance = HighPrecMeters <$> distance,
          maxDistance = HighPrecMeters <$> maxDistance,
          merchantId = fromKey merchantId,
          bundleVersion = bundleVersion',
          clientVersion = clientVersion',
          ..
        }
