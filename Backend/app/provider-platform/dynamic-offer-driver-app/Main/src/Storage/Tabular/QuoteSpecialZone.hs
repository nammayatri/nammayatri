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

module Storage.Tabular.QuoteSpecialZone where

import qualified Domain.Types.QuoteSpecialZone as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Meters (..))
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id
import qualified Storage.Tabular.FareParameters as Fare
import qualified Storage.Tabular.Merchant as SMerchant
import qualified Storage.Tabular.SearchRequestSpecialZone as SReq
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteSpecialZoneT sql=quote_special_zone
      id Text
      searchRequestId SReq.SearchRequestSpecialZoneTId
      providerId SMerchant.MerchantTId
      vehicleVariant Variant.Variant
      distance Meters
      validTill UTCTime
      estimatedFare Common.Money
      fareParametersId Fare.FareParametersTId
      estimatedFinishTime UTCTime
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteSpecialZoneT where
  type DomainKey QuoteSpecialZoneT = Id Domain.QuoteSpecialZone
  fromKey (QuoteSpecialZoneTKey _id) = Id _id
  toKey (Id id) = QuoteSpecialZoneTKey id

instance FromTType (QuoteSpecialZoneT, Fare.FareParametersT) Domain.QuoteSpecialZone where
  fromTType (QuoteSpecialZoneT {..}, fareParamsT) = do
    fareParams <- fromTType fareParamsT
    return $
      Domain.QuoteSpecialZone
        { id = Id id,
          searchRequestId = fromKey searchRequestId,
          providerId = fromKey providerId,
          ..
        }

instance ToTType (QuoteSpecialZoneT, Fare.FareParametersT) Domain.QuoteSpecialZone where
  toTType Domain.QuoteSpecialZone {..} =
    ( QuoteSpecialZoneT
        { id = getId id,
          searchRequestId = toKey searchRequestId,
          providerId = toKey providerId,
          fareParametersId = toKey fareParams.id,
          ..
        },
      toTType fareParams
    )
