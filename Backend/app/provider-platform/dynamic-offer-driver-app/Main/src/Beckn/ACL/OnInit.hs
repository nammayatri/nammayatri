{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.OnDemand.Utils.Constructors
import qualified BecknV2.OnDemand.Utils.Common as UtilsV2
import BecknV2.OnDemand.Utils.Payment
import qualified Data.List as L
import Domain.Action.Beckn.Init as DInit
import Domain.Types
import Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FarePolicy as FarePolicyD
import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import Kernel.Utils.Common

mkOnInitMessageV2 :: DInit.InitRes -> DBC.BecknConfig -> Maybe FarePolicyD.FullFarePolicy -> Spec.ConfirmReqMessage
mkOnInitMessageV2 res becknConfig mbFarePolicy =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res becknConfig mbFarePolicy
    }

tfOrder :: DInit.InitRes -> DBC.BecknConfig -> Maybe FarePolicyD.FullFarePolicy -> Spec.Order
tfOrder res becknConfig mbFarePolicy = do
  let farePolicy = case mbFarePolicy of
        Nothing -> Nothing
        Just fullFarePolicy -> Just $ FarePolicyD.fullFarePolicyToFarePolicy fullFarePolicy
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Just $ tfCancellationTerms res.cancellationFee,
      orderFulfillments = tfFulfillments res,
      orderId = Just res.booking.id.getId,
      orderItems = Utils.tfItems res.booking res.transporter.shortId.getShortId Nothing farePolicy (Just res.paymentId),
      orderPayments = tfPayments res becknConfig,
      orderProvider = Utils.tfProvider becknConfig,
      orderQuote = Utils.tfQuotation res.booking,
      orderStatus = Nothing,
      orderCreatedAt = Just res.booking.createdAt,
      orderUpdatedAt = Just res.booking.updatedAt
    }

mkCommissionTagGroup :: Maybe Common.HighPrecMoney -> Maybe [Spec.TagGroup]
mkCommissionTagGroup mbCommission = do
  commission <- mbCommission
  Tags.buildTagGroups
    [ Tags.COMMISSION Tags.~= Common.highPrecMoneyToText commission
    ]

tfFulfillments :: DInit.InitRes -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just
    [ emptyFulfillment
        { Spec.fulfillmentCustomer = tfCustomer res,
          Spec.fulfillmentId = Just res.booking.quoteId,
          Spec.fulfillmentStops = Utils.mkStops' res.booking.fromLocation res.booking.toLocation res.booking.stops Nothing,
          Spec.fulfillmentType = Just $ UtilsV2.tripCategoryToFulfillmentType res.booking.tripCategory,
          Spec.fulfillmentVehicle = tfVehicle res
        }
    ]

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DInit.InitRes -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments res bppConfig = do
  let mPrice = Just $ mkPrice (Just res.booking.currency) res.booking.estimatedFare
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bppConfig.paymentParamsJson
  let basePayment = mkPayment (show res.transporter.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice (Just res.paymentId) mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing
      commissionTagGroup = mkCommissionTagGroup res.booking.commission
      updatedPaymentTags = case (basePayment.paymentTags, commissionTagGroup) of
        (Just existingTags, Just commissionTags) -> Just (existingTags <> commissionTags)
        (Just existingTags, Nothing) -> Just existingTags
        (Nothing, Just commissionTags) -> Just commissionTags
        (Nothing, Nothing) -> Nothing
  Just . L.singleton $ basePayment {Spec.paymentTags = updatedPaymentTags}

tfVehicle :: DInit.InitRes -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVariant res.vehicleVariant
  Just
    emptyVehicle
      { Spec.vehicleCategory = Just category,
        Spec.vehicleVariant = Just variant
      }

tfCancellationTerms :: Maybe PriceAPIEntity -> [Spec.CancellationTerm]
tfCancellationTerms cancellationFee =
  L.singleton
    Spec.CancellationTerm
      { cancellationTermCancellationFee = Utils.tfCancellationFee cancellationFee,
        cancellationTermFulfillmentState = Utils.tfFulfillmentState Enums.RIDE_ASSIGNED,
        cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
      }

tfCustomer :: DInit.InitRes -> Maybe Spec.Customer
tfCustomer res =
  return $
    Spec.Customer
      { customerContact =
          Just Spec.Contact { contactPhone = Just res.riderPhoneNumber },
        customerPerson = do
          riderName <- res.riderName
          Just $ emptyPerson { Spec.personName = Just riderName }
      }
