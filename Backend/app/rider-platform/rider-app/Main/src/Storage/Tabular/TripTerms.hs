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

module Storage.Tabular.TripTerms where

import qualified Domain.Types.TripTerms as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TripTermsT sql=trip_terms
      id Text
      descriptions Text
      Primary id
      deriving Generic
    |]

instance TEntityKey TripTermsT where
  type DomainKey TripTermsT = Id Domain.TripTerms
  fromKey (TripTermsTKey _id) = Id _id
  toKey (Id id) = TripTermsTKey id

instance FromTType TripTermsT Domain.TripTerms where
  fromTType TripTermsT {..} = do
    return $
      Domain.TripTerms
        { id = Id id,
          descriptions = Domain.splitDescriptions descriptions
        }

instance ToTType TripTermsT Domain.TripTerms where
  toTType Domain.TripTerms {..} =
    TripTermsT
      { id = getId id,
        descriptions = Domain.intercalateDescriptions descriptions
      }
