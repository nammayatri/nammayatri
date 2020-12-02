{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Intent (Intent (..)) where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Payment (PaymentType)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data Intent = Intent
  { _query_string :: Maybe Text,
    _provider :: Maybe ProviderInfo,
    _fulfillment :: Maybe FulFillmentInfo,
    _payment :: Maybe PaymentInfo,
    _category :: Maybe CategoryInfo,
    _offer :: Maybe OfferInfo,
    _item :: Maybe ItemInfo,
    _purpose :: Maybe Text,
    _tags :: [Tags] -- Fix after that https://github.com/beckn/protocol-specifications/pull/61
  }
  deriving (Generic, Show)

data ProviderInfo = ProviderInfo
  { _id :: Maybe Text,
    _descriptor :: Maybe DescriptorName,
    _locations :: [IdObject]
  }
  deriving (Generic, Show)

newtype DescriptorName = DescriptorName {_name :: Text}
  deriving (Generic, Show)

data FulFillmentInfo = FulFillmentInfo
  { _id :: Maybe Text,
    _start :: Maybe LocationAndTime,
    _end :: Maybe LocationAndTime,
    _tags :: Tags
  }
  deriving (Generic, Show)

data LocationAndTime = LocationAndTime
  { _location :: Maybe Location,
    _time :: Maybe Time
  }
  deriving (Generic, Show)

data PaymentInfo = PaymentInfo
  { _type :: Maybe PaymentType,
    _params :: Maybe PaymentParams
  }
  deriving (Generic, Show)

newtype PaymentParams = PaymentParams
  {_mode :: Maybe Text}
  deriving (Generic, Show)

data CategoryInfo = CategoryInfo
  { _id :: Maybe Text,
    _descriptor :: Maybe DescriptorName
  }
  deriving (Generic, Show)

data OfferInfo = OfferInfo
  { _id :: Maybe Text,
    _descriptor :: Maybe DescriptorName
  }
  deriving (Generic, Show)

data ItemInfo = ItemInfo
  { _id :: Maybe Text,
    _descriptor :: Maybe ItemInfoDescriptor
  }
  deriving (Generic, Show)

data ItemInfoDescriptor = ItemInfoDescriptor
  { _name :: Maybe Text,
    _tags :: Maybe Tags
  }
  deriving (Generic, Show)

{- possible solution to this multitude of types:
data IdAndDescriptor descriptor = IdAndDescriptor
  { _id :: Maybe Text,
    _descriptor :: Maybe descriptor
  }
-}

deriveJSON stripLensPrefixOptions ''PaymentParams
deriveJSON stripLensPrefixOptions ''PaymentInfo
deriveJSON stripLensPrefixOptions ''Intent
deriveJSON stripLensPrefixOptions ''ProviderInfo
deriveJSON stripLensPrefixOptions ''FulFillmentInfo
deriveJSON stripLensPrefixOptions ''LocationAndTime
deriveJSON stripAllLensPrefixOptions ''CategoryInfo
deriveJSON stripLensPrefixOptions ''DescriptorName
deriveJSON stripLensPrefixOptions ''OfferInfo
deriveJSON stripLensPrefixOptions ''ItemInfo
deriveJSON stripLensPrefixOptions ''ItemInfoDescriptor
