{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSelect.Order where

import Beckn.Types.Core.Taxi.Common.Payment
import Beckn.Types.Core.Taxi.OnSelect.Fulfillment
import Beckn.Types.Core.Taxi.OnSelect.Item
import Beckn.Types.Core.Taxi.OnSelect.Provider
-- import Beckn.Types.Core.Taxi.OnSelect.Addon
import Beckn.Types.Core.Taxi.OnSelect.Quote
import Data.OpenApi (ToSchema (..), fromAesonOptions)
import Kernel.Prelude
import Kernel.Utils.JSON (slashedRecordFields)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data OrderV2 = OrderV2
  { provider :: Provider,
    items :: [ItemV2],
    -- add_ons :: [Addon],
    fulfillments :: [FulfillmentInfoV2],
    quote :: Quote,
    payment :: PaymentV2
  }
  deriving (Generic, Show)

instance ToSchema OrderV2 where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON OrderV2 where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON OrderV2 where
  toJSON = genericToJSON slashedRecordFields

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Order = Order
  { provider :: Provider,
    items :: [Item],
    -- add_ons :: [Addon],
    fulfillment :: FulfillmentInfo,
    quote :: Quote,
    payment :: Payment
  }
  deriving (Generic, Show)

-- data Provider = Provider
--   { id :: Text,
--     descriptor :: Descriptor,
--     locations :: [ProviderLocation],
--     categories :: [Category],
--     -- items :: [Item], --FIXME this should be list of only RENTAL or only ONE_WAY items
--     -- offers :: [Offer],
--     -- add_ons :: [Addon],
--     -- fulfillment :: FulfillmentInfo,
--     contacts :: Text,
--     tags :: ProviderTags,
--     -- quote :: Quote,
--     payment :: Payment
--   }

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON Order where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Order where
  toJSON = genericToJSON slashedRecordFields
