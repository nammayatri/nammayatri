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

import qualified Beckn.Types.Core.Taxi.Select as Select
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Action.UI.Select as DSelect
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Common

buildSelectReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSelect.DSelectRes ->
  m (BecknReq Select.SelectMessage)
buildSelectReq dSelectRes = do
  let messageId = dSelectRes.estimate.bppEstimateId.getId
  let transactionId = dSelectRes.searchRequest.id.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/cab/v1/" <> T.unpack dSelectRes.merchant.id.getId)
  context <- buildTaxiContext Context.SELECT messageId (Just transactionId) dSelectRes.merchant.bapId bapUrl (Just dSelectRes.providerId) (Just dSelectRes.providerUrl) dSelectRes.merchant.city dSelectRes.merchant.country
  let order = mkOrder dSelectRes
  pure $ BecknReq context $ Select.SelectMessage order

mkOrder :: DSelect.DSelectRes -> Select.Order
mkOrder res = do
  let items =
        (: []) $
          Select.OrderItem
            { id = res.estimate.bppEstimateId.getId
            }
      breakups =
        catMaybes
          [ ( \customerExtraFee ->
                Select.BreakupItem
                  { title = "Extra fee",
                    price =
                      Select.BreakupItemPrice
                        { currency = "INR",
                          value = realToFrac customerExtraFee
                        }
                  }
            )
              <$> res.customerExtraFee
          ]
  Select.Order
    { items,
      fulfillment =
        Select.FulfillmentInfo
          { start =
              Select.StartInfo
                { time = Select.TimeTimestamp res.searchRequest.startTime
                },
            tags = Select.Tags res.autoAssignEnabled
          },
      quote =
        Select.Quote
          { breakup = breakups
          }
    }
