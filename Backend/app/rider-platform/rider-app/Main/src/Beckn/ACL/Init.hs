{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Init (buildInitReqV2) where

import qualified Beckn.OnDemand.Transformer.Init as TF
import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens ((%~))
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import qualified SharedLogic.Confirm as SConfirm

buildInitReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], EncFlow m r) =>
  SConfirm.DConfirmRes ->
  m Spec.InitReq
buildInitReqV2 res = do
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  let (fulfillmentType, mbBppFullfillmentId) = case res.quoteDetails of
        SConfirm.ConfirmOneWayDetails -> ("DELIVERY", Nothing)
        SConfirm.ConfirmRentalDetails quoteId -> ("RENTAL", Just quoteId)
        SConfirm.ConfirmInterCityDetails quoteId -> ("INTER_CITY", Just quoteId)
        SConfirm.ConfirmAutoDetails bppQuoteId -> ("DELIVERY", Just bppQuoteId)
        SConfirm.ConfirmOneWaySpecialZoneDetails quoteId -> ("RIDE_OTP", Just quoteId) --need to be  checked
  let action = Context.INIT
  let domain = Context.MOBILITY

  TF.buildInitReq res bapUrl action domain fulfillmentType mbBppFullfillmentId
