module Domain.Action.Dashboard.AppManagement.Pass
  ( getPassCustomerAvailablePasses,
    getPassCustomerPurchasedPasses,
    getPassCustomerTransactions,
    postPassCustomerActivateToday,
    postPassCustomerPassSelect,
    getPassCustomerPaymentStatus,
    postPassCustomerPassResetDeviceSwitchCount,
  )
where

import qualified API.Types.Dashboard.AppManagement.Pass
import qualified "this" API.Types.UI.Pass
import qualified Data.Time
import qualified "this" Domain.Action.UI.Pass as DPass
import qualified Domain.Action.UI.Payment as UIPayment
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Pass
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.PurchasedPass
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Types
import qualified Kernel.External.Types as Lang
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error
import qualified Lib.Payment.Domain.Action
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Storage.CachedQueries.Merchant as QM

getPassCustomerAvailablePasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Lang.Language -> Environment.Flow [API.Types.UI.Pass.PassInfoAPIEntity])
getPassCustomerAvailablePasses merchantShortId _opCity personId language = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.getMultimodalPassAvailablePasses (Just personId, merchant.id) language

getPassCustomerPurchasedPasses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Kernel.Prelude.Maybe (Domain.Types.PurchasedPass.StatusType) -> Environment.Flow [API.Types.UI.Pass.PurchasedPassAPIEntity])
getPassCustomerPurchasedPasses merchantShortId _opCity personId language status = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.getMultimodalPassListUtil True (Just personId, merchant.id) Nothing Nothing language Nothing Nothing status

getPassCustomerTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity])
getPassCustomerTransactions merchantShortId _opCity personId limit offset = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.getMultimodalPassTransactions (Just personId, merchant.id) limit offset

postPassCustomerActivateToday :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Data.Time.Day) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPassCustomerActivateToday merchantShortId _opCity personId passNumber startDay = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.postMultimodalPassActivateToday (Just personId, merchant.id) passNumber startDay

postPassCustomerPassSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> API.Types.Dashboard.AppManagement.Pass.PurchasedPassSelectReq -> Environment.Flow API.Types.UI.Pass.PassSelectionAPIEntity)
postPassCustomerPassSelect merchantShortId _opCity personId passId req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.postMultimodalPassSelectUtil True (Just personId, merchant.id) passId Nothing Nothing req.profilePicture req.startDay

getPassCustomerPaymentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Environment.Flow Lib.Payment.Domain.Action.PaymentStatusResp)
getPassCustomerPaymentStatus merchantShortId _opCity personId orderId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  UIPayment.getStatus (personId, merchant.id) orderId

postPassCustomerPassResetDeviceSwitchCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPassCustomerPassResetDeviceSwitchCount merchantShortId _opCity personId purchasedPassId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DPass.postMultimodalPassResetDeviceSwitchCount (Just personId, merchant.id) purchasedPassId
