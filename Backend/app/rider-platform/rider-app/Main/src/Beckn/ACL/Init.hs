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
import qualified Beckn.OnDemand.Utils.Common as UCommon
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import Control.Lens ((%~))
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified SharedLogic.Confirm as SConfirm
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.ValueAddNP as VNP

buildInitReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], EncFlow m r, KvDbFlow m r) =>
  SConfirm.DConfirmRes ->
  m Spec.InitReq
buildInitReqV2 res = do
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle res.merchant.id "MOBILITY" (UCommon.mapVariantToVehicle res.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
  let (fulfillmentType, mbBppFullfillmentId) = case res.quoteDetails of
        SConfirm.ConfirmOneWayDetails -> (show Enums.DELIVERY, Nothing)
        SConfirm.ConfirmRentalDetails quoteId -> (show Enums.RENTAL, Just quoteId)
        SConfirm.ConfirmInterCityDetails quoteId -> (show Enums.INTER_CITY, Just quoteId)
        SConfirm.ConfirmAutoDetails bppQuoteId -> (show Enums.DELIVERY, Just bppQuoteId)
        SConfirm.ConfirmOneWaySpecialZoneDetails quoteId -> (show Enums.RIDE_OTP, Just quoteId) --need to be  checked
  let action = Context.INIT
  let domain = Context.MOBILITY
  isValueAddNP <- VNP.isValueAddNP res.providerId
  ttl <- bapConfig.initTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  TF.buildInitReq res bapUrl action domain fulfillmentType mbBppFullfillmentId isValueAddNP bapConfig ttl
