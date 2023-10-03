{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Select (buildSelectReq) where

import Beckn.ACL.Common (castVariant, mkLocation)
import qualified Beckn.Types.Core.Taxi.Select as Select
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Select as DSelect
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Common
import Tools.Error

buildSelectReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSelect.DSelectRes ->
  m (BecknReq Select.SelectMessage)
buildSelectReq dSelectRes = do
  let messageId = dSelectRes.estimate.bppEstimateId.getId
  let transactionId = dSelectRes.searchRequest.id.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack dSelectRes.merchant.id.getId)
  context <- buildContext Context.MOBILITY Context.SELECT messageId (Just transactionId) dSelectRes.merchant.bapId bapUrl (Just dSelectRes.providerId) (Just dSelectRes.providerUrl) dSelectRes.merchant.city dSelectRes.merchant.country dSelectRes.autoAssignEnabled
  order <- buildOrder dSelectRes
  pure $ BecknReq context $ Select.SelectMessage order

buildOrder :: (Monad m, Log m, MonadThrow m) => DSelect.DSelectRes -> m Select.Order
buildOrder res = do
  let start = mkLocation $ DSearchCommon.makeSearchReqLoc' res.searchRequest.fromLocation
  toLocation <- res.searchRequest.toLocation & fromMaybeM (InternalError "To location address not found")
  let end = mkLocation $ DSearchCommon.makeSearchReqLoc' toLocation
  let variant = castVariant res.variant
  let item =
        Select.OrderItem
          { id = res.estimate.itemId,
            price =
              Select.Price
                { currency = "INR",
                  value = show res.estimate.estimatedFare.getMoney
                },
            tags = if isJust res.customerExtraFee then Just $ Select.TG [mkCustomerTipTags] else Nothing
          }
  return
    Select.Order
      { items = [item],
        fulfillment =
          Select.FulfillmentInfo
            { start =
                Select.StartInfo
                  { location = start
                  },
              end =
                Select.StopInfo
                  { location = end
                  },
              id = res.estimate.bppEstimateId.getId,
              vehicle = Select.Vehicle {category = variant},
              _type = Just Select.RIDE,
              tags = Nothing,
              tracking = Nothing
            }
      }
  where
    mkCustomerTipTags =
      Select.TagGroup
        { display = False,
          code = "customer_tip_info",
          name = "Customer Tip Info",
          list =
            [ Select.Tag
                { display = (\_ -> Just False) =<< res.customerExtraFee,
                  code = (\_ -> Just "customer_tip") =<< res.customerExtraFee,
                  name = (\_ -> Just "Customer Tip") =<< res.customerExtraFee,
                  value = (\charges -> Just $ show charges.getMoney) =<< res.customerExtraFee
                }
            ]
        }
