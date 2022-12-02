{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.TripTerms where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.TripTerms as Domain

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

instance TType TripTermsT Domain.TripTerms where
  fromTType TripTermsT {..} = do
    return $
      Domain.TripTerms
        { id = Id id,
          descriptions = Domain.splitDescriptions descriptions
        }
  toTType Domain.TripTerms {..} =
    TripTermsT
      { id = getId id,
        descriptions = Domain.intercalateDescriptions descriptions
      }
