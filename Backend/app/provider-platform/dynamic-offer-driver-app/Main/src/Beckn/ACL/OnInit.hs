{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils hiding (mkStops)
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.OnDemand.Utils.Payment
import qualified Data.List as L
import qualified Data.Text as T
import Domain.Action.Beckn.Init as DInit
import Domain.Types
import Domain.Types.BecknConfig as DBC
import Kernel.Prelude

mkOnInitMessageV2 :: DInit.InitRes -> DBC.BecknConfig -> Spec.ConfirmReqMessage
mkOnInitMessageV2 res becknConfig =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res becknConfig
    }

tfOrder :: DInit.InitRes -> DBC.BecknConfig -> Spec.Order
tfOrder res becknConfig =
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Just $ tfCancellationTerms becknConfig,
      orderFulfillments = tfFulfillments res,
      orderId = Just res.booking.id.getId,
      orderItems = Utils.tfItems res.booking res.transporter.shortId.getShortId Nothing,
      orderPayments = tfPayments res becknConfig,
      orderProvider = tfProvider res,
      orderQuote = Utils.tfQuotation res.booking,
      orderStatus = Nothing
    }

tfFulfillments :: DInit.InitRes -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just
    [ Spec.Fulfillment
        { fulfillmentAgent = Nothing,
          fulfillmentCustomer = Nothing,
          fulfillmentId = Just res.booking.quoteId,
          fulfillmentState = Nothing,
          fulfillmentStops = Utils.mkStops' res.booking.fromLocation res.booking.toLocation Nothing,
          fulfillmentTags = Nothing,
          fulfillmentType = Just $ Common.mkFulfillmentType res.booking.tripCategory,
          fulfillmentVehicle = tfVehicle res
        }
    ]

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DInit.InitRes -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments res bppConfig = do
  let amount = fromIntegral (res.booking.estimatedFare.getMoney)
  let mkParams :: (Maybe BknPaymentParams) = (readMaybe . T.unpack) =<< bppConfig.paymentParamsJson
  Just $ L.singleton $ mkPayment (show res.transporter.city) (show bppConfig.collectedBy) Enums.NOT_PAID (Just amount) Nothing mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee

tfProvider :: DInit.InitRes -> Maybe Spec.Provider
tfProvider res = do
  let providerId = res.bppSubscriberId
  return $
    Spec.Provider
      { providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = providerId,
        providerItems = Nothing,
        providerLocations = Nothing,
        providerPayments = Nothing
      }

tfVehicle :: DInit.InitRes -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVariant res.booking.vehicleVariant
  Just
    Spec.Vehicle
      { vehicleCategory = Just category,
        vehicleVariant = Just variant,
        vehicleColor = Nothing,
        vehicleMake = Nothing,
        vehicleModel = Nothing,
        vehicleRegistration = Nothing
      }

tfCancellationTerms :: DBC.BecknConfig -> [Spec.CancellationTerm]
tfCancellationTerms becknConfig =
  L.singleton
    Spec.CancellationTerm
      { cancellationTermCancellationFee = Utils.tfCancellationFee becknConfig.cancellationFeeAmount becknConfig.cancellationFeePercentage,
        cancellationTermFulfillmentState = Nothing,
        cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
      }
