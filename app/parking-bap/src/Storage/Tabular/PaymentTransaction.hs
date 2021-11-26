{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Storage.Tabular.PaymentTransaction
  ( module Storage.Tabular.PaymentTransaction,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Data.Time (UTCTime)
import Database.Persist.TH
import Storage.Tabular.Booking
import Storage.Tabular.PaymentTransaction.Internal as Reexport

share
  [mkPersist sqlSettings]
  [defaultQQ|
    PaymentTransactionT sql=payment_transaction
      Id Text default=uuid_generate_v4() sqltype=varchar(36)
      bookingId BookingTId
      bknTxnId Text
      paymentGatewayTxnId Text
      fare Amount
      status PaymentStatus
      paymentUrl Text
      updatedAt UTCTime
      createdAt UTCTime
      deriving Generic
    |]
