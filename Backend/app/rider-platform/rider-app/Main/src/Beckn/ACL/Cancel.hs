{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Cancel (buildCancelReq, buildCancelSearchReq) where

import qualified Beckn.Types.Core.Taxi.Cancel.Req as Cancel
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.BookingCancellationReason as SBCR
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Common

buildCancelReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DCancel.CancelRes ->
  m (BecknReq Cancel.CancelMessage)
buildCancelReq res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.CANCEL messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.merchant.defaultCity res.merchant.country False
  pure $ BecknReq context $ mkCancelMessage res

mkCancelMessage :: DCancel.CancelRes -> Cancel.CancelMessage
mkCancelMessage res = Cancel.CancelMessage res.bppBookingId.getId "" $ castCancellatonSource res.cancellationSource
  where
    castCancellatonSource = \case
      SBCR.ByUser -> Cancel.ByUser
      SBCR.ByDriver -> Cancel.ByDriver
      SBCR.ByMerchant -> Cancel.ByMerchant
      SBCR.ByAllocator -> Cancel.ByAllocator
      SBCR.ByApplication -> Cancel.ByApplication

buildCancelSearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DCancel.CancelSearch ->
  m (BecknReq Cancel.CancelMessage)
buildCancelSearchReq res = do
  let messageId = res.estimateId.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.CANCEL messageId (Just res.searchReqId.getId) res.merchant.bapId bapUrl (Just res.providerId) (Just res.providerUrl) res.merchant.defaultCity res.merchant.country False
  pure $ BecknReq context $ mkCancelSearchMessage res

mkCancelSearchMessage :: DCancel.CancelSearch -> Cancel.CancelMessage
mkCancelSearchMessage res = Cancel.CancelMessage "" res.searchReqId.getId Cancel.ByUser
