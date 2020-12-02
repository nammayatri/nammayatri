{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Billing
  ( Billing (..),
  )
where

import Beckn.Types.Core.Migration.Address (Address)
import Beckn.Types.Core.Migration.Organization (Organization)
import Beckn.Types.Core.Migration.Time (Time)
import Data.Aeson.TH (deriveJSON)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Billing = Billing
  { _name :: Text,
    _organization :: Maybe Organization,
    _address :: Maybe Address,
    _email :: Maybe Text,
    _phone :: Text,
    _time :: Maybe Time,
    _tax_number :: Maybe Text,
    _created_at :: Maybe UTCTime,
    _updated_at :: Maybe UTCTime
  }
  deriving (Generic, Show)

deriveJSON stripLensPrefixOptions ''Billing
