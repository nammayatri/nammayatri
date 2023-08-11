{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Invoice where

import qualified Domain.Types.Invoice as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.DriverFee (DriverFeeTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    InvoiceT sql=invoice
        id Text
        invoiceShortId Text
        driverFeeId DriverFeeTId
        createdAt UTCTime
        updatedAt UTCTime
        Primary id
        deriving Generic
  |]

instance TEntityKey InvoiceT where
  type DomainKey InvoiceT = Id Domain.Invoice
  fromKey (InvoiceTKey _id) = Id _id
  toKey (Id id) = InvoiceTKey id

instance FromTType InvoiceT Domain.Invoice where
  fromTType InvoiceT {..} = do
    return $
      Domain.Invoice
        { id = id,
          driverFeeId = fromKey driverFeeId,
          ..
        }

instance ToTType InvoiceT Domain.Invoice where
  toTType Domain.Invoice {..} = do
    InvoiceT
      { id = id,
        driverFeeId = toKey driverFeeId,
        ..
      }
