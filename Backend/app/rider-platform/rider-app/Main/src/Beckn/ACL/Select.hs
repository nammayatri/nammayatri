{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Beckn.ACL.Select (buildSelectReq) where

import qualified Beckn.Types.Core.Taxi.Select as Select
import qualified Domain.Action.UI.Select as DSelect
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Common

buildSelectReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DSelect.DSelectRes ->
  m (BecknReq Select.SelectMessage)
buildSelectReq dSelectRes = do
  let messageId = dSelectRes.estimate.bppEstimateId.getId
  let transactionId = dSelectRes.searchRequest.id.getId
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.SELECT messageId (Just transactionId) bapIDs.cabs bapURIs.cabs (Just dSelectRes.providerId) (Just dSelectRes.providerUrl) dSelectRes.city
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
