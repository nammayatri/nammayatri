module Types.Beckn.Provider (Provider (..), DescriptorInfo (..)) where

import EulerHS.Prelude hiding (id)
import Types.Beckn.Category (Category)
import Types.Beckn.Item (Item)

data Provider = Provider
  { descriptor :: DescriptorInfo,
    categories :: [Category],
    items :: [Item]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype DescriptorInfo = DescriptorInfo
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
