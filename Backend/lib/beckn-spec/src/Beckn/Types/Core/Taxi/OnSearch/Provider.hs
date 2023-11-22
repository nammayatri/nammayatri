{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSearch.Provider
  ( Provider (..),
    ProviderTags (..),
  )
where

-- import Beckn.Types.Core.Taxi.Common.Payment
-- import Beckn.Types.Core.Taxi.OnSearch.Addon
-- import Beckn.Types.Core.Taxi.OnSearch.Category
import Beckn.Types.Core.Taxi.OnSearch.Descriptor
import Beckn.Types.Core.Taxi.OnSearch.Fulfillment
import Beckn.Types.Core.Taxi.OnSearch.Item
-- import Beckn.Types.Core.Taxi.OnSearch.Offer
-- import Beckn.Types.Core.Taxi.OnSearch.ProviderLocation
import Data.Aeson
import Data.OpenApi (ToSchema (..), fromAesonOptions)
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    locations :: Maybe [LatLong],
    -- categories :: [Category],
    items :: [Item], --FIXME this should be list of only RENTAL or only ONE_WAY items
    -- offers :: [Offer],
    -- add_ons :: [Addon],
    fulfillments :: [FulfillmentInfo]
    -- contacts :: Text,
    -- tags :: ProviderTags,
    -- payment :: Payment, -- TODO For backwards compatibility, remove it. Only payments field used in logic.
    -- payments :: Maybe [Payment]
  }
  deriving (Generic, Show)

instance ToJSON Provider where
  toJSON = genericToJSON providerJSONOptions

instance FromJSON Provider where
  parseJSON = genericParseJSON providerJSONOptions

instance ToSchema Provider where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions providerJSONOptions

providerJSONOptions :: Options
providerJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "contacts" -> "./komn/contacts"
        a -> a
    }

data ProviderTags = ProviderTags
  { rides_inprogress :: Int,
    rides_completed :: Int,
    rides_confirmed :: Int
  }
  deriving (Generic, Show)

instance ToJSON ProviderTags where
  toJSON = genericToJSON providerTagsJSONOptions

instance FromJSON ProviderTags where
  parseJSON = genericParseJSON providerTagsJSONOptions

instance ToSchema ProviderTags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions providerTagsJSONOptions

providerTagsJSONOptions :: Options
providerTagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "rides_inprogress" -> "./komn/rides_inprogress"
        "rides_confirmed" -> "./komn/rides_confirmed"
        "rides_completed" -> "./komn/rides_completed"
        a -> a
    }
