{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Init where

import qualified Beckn.OnDemand.Utils.Init
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified Data.Text
import qualified Domain.Action.Beckn.Init
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber
import Kernel.Utils.Common (type (:::))
import qualified Kernel.Utils.Common
import qualified Kernel.Utils.Text

buildDInitReq :: (Kernel.Types.App.MonadFlow m) => Kernel.Types.Registry.Subscriber.Subscriber -> BecknV2.OnDemand.Types.InitReq -> m (Domain.Action.Beckn.Init.InitReq)
buildDInitReq subscriber req = do
  let bapId_ = subscriber.subscriber_id
  let bapUri_ = subscriber.subscriber_url
  let driverId_ = req.initReqMessage.confirmReqMessageOrder.orderProvider >>= (.providerId)
  bapCity_ <- req.initReqContext.contextLocation >>= (.locationCity) >>= (.cityCode) >>= Kernel.Utils.Text.decodeFromText & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Couldn't find/parse City")
  bapCountry_ <- req.initReqContext.contextLocation >>= (.locationCountry) >>= (.countryCode) >>= Kernel.Utils.Text.decodeFromText & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Couldn't find/parse Country")
  estimateId_ <- req.initReqMessage.confirmReqMessageOrder.orderFulfillments >>= Kernel.Prelude.listToMaybe >>= (.fulfillmentId) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "FulfillmentId not found. It should either be estimateId or quoteId")
  initTypeReq_ <- req.initReqMessage.confirmReqMessageOrder.orderFulfillments >>= Kernel.Prelude.listToMaybe >>= (.fulfillmentType) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "FulfillmentType not found") >>= Beckn.OnDemand.Utils.Init.buildInitTypeReq
  maxEstimatedDistance_ <- req.initReqMessage.confirmReqMessageOrder.orderFulfillments >>= Kernel.Prelude.listToMaybe >>= (.fulfillmentTags) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Fulfillment Tags not found") >>= (return . Beckn.OnDemand.Utils.Init.getMaxEstimateDistance)
  paymentMethodInfo_ <- req.initReqMessage.confirmReqMessageOrder.orderPayments >>= Kernel.Prelude.listToMaybe & (\mpayment -> Kernel.Prelude.mapM Beckn.OnDemand.Utils.Init.mkPaymentMethodInfo mpayment) <&> Kernel.Prelude.join
  vehicleVariant_ <- req.initReqMessage.confirmReqMessageOrder.orderFulfillments >>= Kernel.Prelude.listToMaybe >>= (.fulfillmentVehicle) >>= (.vehicleCategory) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Vehicle Category not found") >>= Beckn.OnDemand.Utils.Init.castVehicleVariant
  pure $ Domain.Action.Beckn.Init.InitReq {bapCity = bapCity_, bapCountry = bapCountry_, bapId = bapId_, bapUri = bapUri_, driverId = driverId_, estimateId = estimateId_, initTypeReq = initTypeReq_, maxEstimatedDistance = maxEstimatedDistance_, paymentMethodInfo = paymentMethodInfo_, vehicleVariant = vehicleVariant_}
