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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Merchant where

import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.ServerName as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Beckn.City (City)
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantT sql=merchant
      id Text
      shortId Text
      serverNames [Domain.ServerName]
      is2faMandatory Bool
      defaultOperatingCity City
      supportedOperatingCities [City]
      createdAt UTCTime
      Primary id
      UniqueMerchantShortId shortId
      deriving Generic
    |]

instance TEntityKey MerchantT where
  type DomainKey MerchantT = Id Domain.Merchant
  fromKey (MerchantTKey _id) = Id _id
  toKey (Id id) = MerchantTKey id

instance FromTType MerchantT Domain.Merchant where
  fromTType MerchantT {..} = do
    return $
      Domain.Merchant
        { id = Id id,
          shortId = ShortId shortId,
          ..
        }

instance ToTType MerchantT Domain.Merchant where
  toTType Domain.Merchant {..} =
    MerchantT
      { id = getId id,
        shortId = getShortId shortId,
        ..
      }
