{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.MerchantAccess where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.MerchantAccess as Domain
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantAccessT sql=merchant_access
      id Text
      personId PersonTId
      merchantId MerchantTId
      createdAt UTCTime
      Primary id
      Unique (MerchantAccessPersonId, MerchantAccessMerchantId)
      deriving Generic
    |]

instance TEntityKey MerchantAccessT where
  type DomainKey MerchantAccessT = Id Domain.MerchantAccess
  fromKey (MerchantAccessTKey _id) = Id _id
  toKey (Id id) = MerchantAccessTKey id

instance TType MerchantAccessT Domain.MerchantAccess where
  fromTType MerchantAccessT {..} = do
    return $
      Domain.MerchantAccess
        { id = Id id,
          merchantId = fromKey merchantId,
          personId = fromKey personId,
          ..
        }
  toTType Domain.MerchantAccess {..} =
    MerchantAccessT
      { id = getId id,
        merchantId = toKey merchantId,
        personId = toKey personId,
        ..
      }
