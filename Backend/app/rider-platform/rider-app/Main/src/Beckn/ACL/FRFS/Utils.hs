{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Utils where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Action.Beckn.FRFS.Common as Domain
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.BaseUrl

buildContext ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  Spec.Action ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  m Spec.Context
buildContext action merchantId txnId msgId mTTL = do
  let bapId = merchantId
  now <- getCurrentTime
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId) <&> showBaseUrlText
  return $
    Spec.Context
      { contextAction = Just $ encodeToText action,
        contextBapId = Just bapId,
        contextBapUri = Just bapUrl,
        contextBppId = Nothing,
        contextBppUri = Nothing,
        contextDomain = Just $ encodeToText Spec.FRFS,
        contextKey = Nothing,
        contextLocation = Nothing,
        contextMessageId = Just msgId,
        contextTimestamp = Just now,
        contextTransactionId = Just txnId,
        contextTtl = mTTL,
        contextVersion = Just "2.0.0"
      }

getStartStop :: [Spec.Stop] -> Maybe Spec.Stop
getStartStop stops = stops & find (\stop -> stop.stopType == start)
  where
    start = Just $ encodeToText Spec.START

mkFareBreakup :: (MonadFlow m) => Spec.QuotationBreakupInner -> m Domain.DFareBreakUp
mkFareBreakup fareBreakup = do
  title <- fareBreakup.quotationBreakupInnerTitle & fromMaybeM (InvalidRequest "Title not found")
  price <- fareBreakup.quotationBreakupInnerPrice >>= Utils.parseMoney & fromMaybeM (InvalidRequest "Price not found")

  breakupItem <- fareBreakup.quotationBreakupInnerItem & fromMaybeM (InvalidRequest "BreakupItem not found")
  let pricePerUnit = breakupItem.itemPrice >>= Utils.parseMoney & fromMaybe price
  let quantity = breakupItem.itemQuantity >>= (.itemQuantitySelected) >>= (.itemQuantitySelectedCount) & fromMaybe 1

  pure $
    Domain.DFareBreakUp
      { title,
        price,
        pricePerUnit,
        quantity
      }

parseTickets :: (MonadFlow m) => Spec.Item -> [Spec.Fulfillment] -> m [Domain.DTicket]
parseTickets item fulfillments = do
  fulfillmentIds <- item.itemFulfillmentIds & fromMaybeM (InvalidRequest "FulfillmentIds not found")
  when (null fulfillmentIds) $ throwError $ InvalidRequest "Empty fulfillmentIds"

  let ticketFulfillments = filterByIds fulfillmentIds
  when (null ticketFulfillments) $ throwError $ InvalidRequest "No ticket fulfillment found"

  traverse parseTicket ticketFulfillments
  where
    filterByIds fIds = filter (\f -> f.fulfillmentId `elem` (Just <$> fIds)) fulfillments

parseTicket :: (MonadFlow m) => Spec.Fulfillment -> m Domain.DTicket
parseTicket fulfillment = do
  stops <- fulfillment.fulfillmentStops & fromMaybeM (InvalidRequest "FulfillmentStops not found")
  startStopAuth <- getStartStop stops >>= (.stopAuthorization) & fromMaybeM (InvalidRequest "StartStop Auth not found")

  qrData <- startStopAuth.authorizationToken & fromMaybeM (InvalidRequest "TicketQrData not found")
  validTill <- startStopAuth.authorizationValidTo & fromMaybeM (InvalidRequest "TicketValidTill not found")
  status <- startStopAuth.authorizationStatus & fromMaybeM (InvalidRequest "TicketStatus not found")

  pure $
    Domain.DTicket
      { qrData,
        validTill,
        status
      }
