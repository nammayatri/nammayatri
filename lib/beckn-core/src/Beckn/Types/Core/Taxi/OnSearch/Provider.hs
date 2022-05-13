module Beckn.Types.Core.Taxi.OnSearch.Provider
  ( Provider (..),
  )
where

import Beckn.Types.Core.Taxi.OnSearch.Item (Item)
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (exp, id)

data Provider = Provider
  { id :: Text,
    name :: Text,
    items :: [Item], --FIXME this should be list of only RENTAL or only ONE_WAY items
    contacts :: Text,
    category_id :: Text, -- ONE_WAY or RENTAL
    rides_inprogress :: Int,
    rides_completed :: Int,
    rides_confirmed :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Provider where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Provider where
  example =
    Provider
      { id = "id",
        name = "name",
        items = [],
        contacts = "99999999999",
        category_id = "ONE_WAY",
        rides_inprogress = 12,
        rides_completed = 32,
        rides_confirmed = 3
      }
