{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Status where

import qualified Beckn.Types.Core.Taxi.Status as Status
import Control.Lens ((%~))
import qualified Data.Text as T
import Domain.Types.Booking.Type (Booking)
import Domain.Types.Merchant (Merchant)
import Domain.Types.Ride (BPPRide)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import Kernel.Utils.Common

buildStatusReq ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  Id BPPRide ->
  Booking ->
  Merchant ->
  m (BecknReq Status.StatusMessage)
buildStatusReq bppRideId booking merchant = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/cab/v1/" <> T.unpack merchant.id.getId)
  context <-
    buildTaxiContext
      Context.STATUS
      messageId
      (Just booking.transactionId)
      merchant.bapId
      bapUrl
      (Just merchant.id.getId)
      (Just booking.providerUrl)
      fromMaybe
      merchant.city booking.city
      fromMaybe
      merchant.city booking.country
  pure $ BecknReq context $ Status.StatusMessage bppRideId.getId
