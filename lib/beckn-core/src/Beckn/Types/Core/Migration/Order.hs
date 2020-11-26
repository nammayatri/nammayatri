{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Order where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Billing
import Beckn.Types.Core.Migration.Fulfillment (Agent, FulfillmentDetails)
import Beckn.Types.Core.Migration.Image
import Beckn.Types.Core.Migration.ItemQuantity
import Beckn.Types.Core.Migration.Name
import Beckn.Types.Core.Migration.Payment
import Beckn.Types.Core.Migration.Quotation
import Beckn.Types.Core.Migration.State
import Beckn.Types.Core.Migration.Tags
import Beckn.Types.Core.Migration.Vehicle
import Beckn.Utils.JSON (deriveJSON)
import Data.Time
import EulerHS.Prelude hiding (State)

data Order = Order
  { _id :: Maybe Text,
    _state :: Maybe Text,
    _provider :: IdAndLocations,
    _items :: [OrderItem],
    _add_ons :: [IdObject],
    _offers :: [IdObject],
    _billing :: Billing,
    _fulfillment :: OrderFulfillment,
    _quote :: Quotation,
    _payment :: Payment,
    _created_at :: Maybe UTCTime,
    _updated_at :: Maybe UTCTime
  }
  deriving (Generic, Show)

data IdAndLocations = IdAndLocations
  { _id :: Text,
    _locations :: [IdObject]
  }
  deriving (Generic, Show)

data OrderItem = OrderItem
  { _id :: Text,
    _quantity :: ItemQuantity
  }
  deriving (Generic, Show)

-- allOf case
data OrderFulfillment = OrderFulfillment
  { _id :: Maybe Text,
    _type :: Maybe Text,
    _state :: Maybe State,
    _tracking :: Maybe Bool,
    _agent :: Maybe Agent,
    _vehicle :: Maybe Vehicle,
    _start :: Maybe FulfillmentDetails,
    _end :: Maybe FulfillmentDetails,
    _purpose :: Maybe Text,
    _tags :: Maybe Tags,
    _customer :: PersonWithContact
  }
  deriving (Generic, Show)

-- allOf case
data PersonWithContact = PersonWithContact
  { _name :: Name, -- Strict here
    _image :: Maybe Image,
    _dob :: Maybe UTCTime, -- FIXME: format: date
    _gender :: Maybe Text,
    _cred :: Maybe Text,
    -- from Contact:
    _phone :: Maybe Text,
    _email :: Maybe Text,
    _tags :: Maybe Tags
  }
  deriving (Generic, Show)

deriveJSON ''Order 'stripLensPrefixOptions
deriveJSON ''OrderItem 'stripLensPrefixOptions
deriveJSON ''OrderFulfillment 'stripLensPrefixOptions
deriveJSON ''PersonWithContact 'stripLensPrefixOptions
deriveJSON ''IdAndLocations 'stripLensPrefixOptions
