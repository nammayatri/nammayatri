{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Init where

import qualified Beckn.OnDemand.Utils.Init
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified Data.Aeson as A
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

buildDInitReq :: (Kernel.Types.App.MonadFlow m) => Kernel.Types.Registry.Subscriber.Subscriber -> BecknV2.OnDemand.Types.InitReq -> m Domain.Action.Beckn.Init.InitReq
buildDInitReq subscriber req = do
  let bapId_ = subscriber.subscriber_id
  let bapUri_ = subscriber.subscriber_url
  let driverId_ = req.initReqMessage.confirmReqMessageOrder.orderProvider >>= (.providerId)
  bapCityText <- req.initReqContext.contextLocation >>= (.locationCity) >>= (.cityCode) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Couldn't find City")
  bapCity_ <- A.decode (A.encode bapCityText) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Couldn't parse City")
  bapCountryText <- req.initReqContext.contextLocation >>= (.locationCountry) >>= (.countryCode) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Couldn't find Country")
  bapCountry_ <- A.decode (A.encode bapCountryText) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Couldn't parse Country")
  fulfillmentId__ <- req.initReqMessage.confirmReqMessageOrder.orderFulfillments >>= Kernel.Prelude.listToMaybe >>= (.fulfillmentId) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "FulfillmentId not found. It should either be estimateId or quoteId")
  fulfillmentType_ <- req.initReqMessage.confirmReqMessageOrder.orderFulfillments >>= Kernel.Prelude.listToMaybe >>= (.fulfillmentType) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "FulfillmentType not found")
  let fulfillmentId_ = case fulfillmentType_ of
        "RIDE" -> Domain.Action.Beckn.Init.EstimateId (Kernel.Types.Id.Id fulfillmentId__)
        "RIDE_OTP" -> Domain.Action.Beckn.Init.QuoteId (Kernel.Types.Id.Id fulfillmentId__)
        "RENTAL" -> Domain.Action.Beckn.Init.QuoteId (Kernel.Types.Id.Id fulfillmentId__)
        _ -> Domain.Action.Beckn.Init.QuoteId (Kernel.Types.Id.Id fulfillmentId__)
  maxEstimatedDistance_ <- (req.initReqMessage.confirmReqMessageOrder.orderFulfillments >>= Kernel.Prelude.listToMaybe >>= (.fulfillmentTags) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Fulfillment Tags not found")) <&> Beckn.OnDemand.Utils.Init.getMaxEstimateDistance
  paymentMethodInfo_ <- req.initReqMessage.confirmReqMessageOrder.orderPayments >>= Kernel.Prelude.listToMaybe & Kernel.Prelude.mapM Beckn.OnDemand.Utils.Init.mkPaymentMethodInfo <&> Kernel.Prelude.join
  let vehCategory = req.initReqMessage.confirmReqMessageOrder.orderFulfillments >>= Kernel.Prelude.listToMaybe >>= (.fulfillmentVehicle) >>= (.vehicleCategory)
      vehVariant = req.initReqMessage.confirmReqMessageOrder.orderFulfillments >>= Kernel.Prelude.listToMaybe >>= (.fulfillmentVehicle) >>= (.vehicleVariant)
  vehicleVariant_ <- Beckn.OnDemand.Utils.Init.castVehicleVariant vehCategory vehVariant & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest $ "Unable to parse vehicle variant:-" <> show vehVariant <> ",vehicle category:-" <> show vehCategory)
  pure $ Domain.Action.Beckn.Init.InitReq {bapCity = bapCity_, bapCountry = bapCountry_, bapId = bapId_, bapUri = bapUri_, driverId = driverId_, fulfillmentId = fulfillmentId_, maxEstimatedDistance = maxEstimatedDistance_, paymentMethodInfo = paymentMethodInfo_, vehicleVariant = vehicleVariant_}
