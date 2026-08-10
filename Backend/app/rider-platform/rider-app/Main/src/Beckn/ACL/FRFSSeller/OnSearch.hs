{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Builds the @on_search@ payload a SELLER sends back to a buyer app.
--
-- The exact inverse of "Beckn.ACL.FRFS.OnSearch", which parses the @on_search@ we
-- receive as a buyer. Everything here is pure — domain values in, wire types out —
-- so it can be reasoned about without a database, a network, or a Flow.
--
-- Phase 1 publishes a minimal catalog: provider identity plus priced items. Stops,
-- fulfillments and payment terms arrive with @select@/@init@ in Phase 2.
module Beckn.ACL.FRFSSeller.OnSearch
  ( SellerCatalog (..),
    SellerItem (..),
    buildOnSearchReq,
    buildOnSearchErrorReq,
  )
where

import qualified BecknV2.FRFS.Types as Spec
import Kernel.Prelude

-- | One sellable journey option.
--
-- @itemId@ is opaque to the buyer and must round-trip unchanged through
-- @select@/@init@/@confirm@ — it is the only handle the buyer keeps on the journey.
data SellerItem = SellerItem
  { itemId :: Text,
    itemDescription :: Text,
    priceValue :: Text,
    currency :: Text,
    -- | Carried for Phase 2, where these populate @providerFulfillments@.
    fromStopCode :: Text,
    toStopCode :: Text
  }
  deriving (Show, Eq)

data SellerCatalog = SellerCatalog
  { providerId :: Text,
    providerName :: Text,
    items :: [SellerItem]
  }
  deriving (Show, Eq)

-- | Turn an inbound request context into its outbound callback context: same
-- transaction and message ids, action flipped to @on_search@.
mkCallbackContext :: Spec.Context -> Spec.Context
mkCallbackContext ctx = ctx {Spec.contextAction = Just "on_search"}

buildOnSearchReq :: Spec.Context -> SellerCatalog -> Spec.OnSearchReq
buildOnSearchReq ctx catalog =
  Spec.OnSearchReq
    { onSearchReqContext = mkCallbackContext ctx,
      onSearchReqError = Nothing,
      onSearchReqMessage = Just (mkMessage catalog)
    }

-- | The error envelope. Codes are a published contract that roughly ten live buyer
-- apps branch on — never invent one. Phase 1 emits only @30016@ (invalid signature)
-- and @31001@ (generic).
buildOnSearchErrorReq :: Spec.Context -> Text -> Text -> Spec.OnSearchReq
buildOnSearchErrorReq ctx code message =
  Spec.OnSearchReq
    { onSearchReqContext = mkCallbackContext ctx,
      onSearchReqError =
        Just
          Spec.Error
            { errorCode = Just code,
              errorMessage = Just message,
              errorPaths = Nothing
            },
      onSearchReqMessage = Nothing
    }

mkMessage :: SellerCatalog -> Spec.OnSearchReqMessage
mkMessage catalog =
  Spec.OnSearchReqMessage
    { onSearchReqMessageCatalog =
        Spec.Catalog
          { catalogDescriptor = Nothing,
            catalogProviders = Just [mkProvider catalog],
            catalogTags = Nothing
          }
    }

mkProvider :: SellerCatalog -> Spec.Provider
mkProvider catalog =
  Spec.Provider
    { providerCategories = Nothing,
      providerDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Nothing,
              descriptorImages = Nothing,
              descriptorName = Just catalog.providerName
            },
      providerFulfillments = Nothing,
      providerId = Just catalog.providerId,
      providerItems = Just (map mkItem catalog.items),
      providerPayments = Nothing,
      providerTags = Nothing,
      providerTime = Nothing
    }

mkItem :: SellerItem -> Spec.Item
mkItem item =
  Spec.Item
    { itemCategoryIds = Nothing,
      itemDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just item.itemDescription,
              descriptorImages = Nothing,
              descriptorName = Just item.itemDescription
            },
      itemFulfillmentIds = Nothing,
      itemId = Just item.itemId,
      itemPrice =
        Just
          Spec.Price
            { priceCurrency = Just item.currency,
              priceValue = Just item.priceValue,
              priceOfferedValue = Nothing
            },
      itemQuantity = Nothing,
      itemTime = Nothing
    }
