{-# LANGUAGE TemplateHaskell #-}

module Storage.Tabular.PaymentTransaction.Internal where

import Beckn.Prelude
import Database.Persist.TH

data PaymentStatus = INITIALIZED | PENDING | FAILED | SUCCESS
  deriving (Generic, Show, Read)

derivePersistField "PaymentStatus"
