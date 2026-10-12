module Lib.Finance.Invoice.VatClubbing
  ( invoiceKeys,
    isExternalLine,
    clubInvoiceLineItems,
    clubInvoiceLineItemsValue,
  )
where

import qualified Data.Aeson as A
import qualified Data.Set as Set
import Kernel.Prelude
import Lib.Finance.Invoice.Interface (InvoiceLineItem (..), LineItemDescription (..))
import RideFinance.Core.Types (FareComponent (..))
import qualified RideFinance.Core.Types as RF
import RideFinance.Display.Club (Keys (..), RowOps (..), classify, clubRows, noKeys)

invoiceKeys :: FareComponent -> Keys LineItemDescription
invoiceKeys = \case
  RIDE_FARE -> Keys {taxExclusiveKeys = [BaseFare, RideFare, RideFareRefund], taxKeys = [RideTax, RideFareRefundTax], clubbedKey = Just RideFareInclVat}
  TOLL_FARE -> Keys {taxExclusiveKeys = [TollFare, TollCharges, TollRefund], taxKeys = [TollTax, TollChargesTax, TollRefundTax], clubbedKey = Just TollFareInclVat}
  PARKING_CHARGE -> Keys {taxExclusiveKeys = [ParkingCharges, ParkingRefund], taxKeys = [ParkingChargesTax, ParkingRefundTax], clubbedKey = Just ParkingChargesInclVat}
  CANCELLATION_FARE -> Keys {taxExclusiveKeys = [CancellationFee, CustomerCancellationFee, CancellationFeeRefund], taxKeys = [CancellationFeeVat, GstOnCancellationFee, CancellationFeeRefundTax], clubbedKey = Just CancellationFeeInclVat}
  PAYMENT_CHARGE -> Keys {taxExclusiveKeys = [PaymentCharge, PaymentChargeRefund], taxKeys = [PaymentChargeTax, PaymentChargeRefundTax], clubbedKey = Nothing}
  COMMISSION -> Keys {taxExclusiveKeys = [PlatformCommission, CommissionRefund], taxKeys = [PlatformCommissionTax, CommissionRefundTax], clubbedKey = Nothing}
  CANCELLATION_COMMISSION -> Keys {taxExclusiveKeys = [CancellationCommission, CancellationCommissionRefund], taxKeys = [CancellationCommissionTax, CancellationCommissionRefundTax], clubbedKey = Nothing}
  AIRPORT_ENTRY_FEE -> noKeys

isExternalLine :: LineItemDescription -> Bool
isExternalLine = maybe False (RF.isExternalComponent . fst) . classify invoiceKeys

invoiceRowOps :: RowOps LineItemDescription InvoiceLineItem
invoiceRowOps =
  RowOps
    { rowKey = \item -> item.groupId *> item.descriptionType,
      rowGroup = (.groupId),
      addAmounts = \anchor item -> anchor {unitPrice = anchor.unitPrice + item.lineTotal, lineTotal = anchor.lineTotal + item.lineTotal},
      withKey = \key item -> item {descriptionType = Just key}
    }

clubInvoiceLineItems :: RF.ClubPolicy -> [InvoiceLineItem] -> [InvoiceLineItem]
clubInvoiceLineItems = clubRows invoiceKeys invoiceRowOps

clubInvoiceLineItemsValue :: RF.ClubPolicy -> A.Value -> A.Value
clubInvoiceLineItemsValue clubPolicy value
  | Set.null clubPolicy = value
  | otherwise = case A.fromJSON value of
    A.Success items -> A.toJSON (clubInvoiceLineItems clubPolicy items)
    A.Error _ -> value
