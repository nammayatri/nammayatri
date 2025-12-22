{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Pass where

import qualified "this" API.Types.UI.Pass
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified "this" Domain.Types.Pass
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.PurchasedPass
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified "payment" Lib.Payment.Domain.Action
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Servant.Client

data PurchasedPassSelectReq = PurchasedPassSelectReq {startDay :: Kernel.Prelude.Maybe Data.Time.Day, profilePicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PurchasedPassSelectReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("pass" :> (GetPassCustomerAvailablePasses :<|> GetPassCustomerPurchasedPasses :<|> GetPassCustomerTransactions :<|> PostPassCustomerActivateToday :<|> PostPassCustomerPassSelect :<|> GetPassCustomerPaymentStatus :<|> PostPassCustomerPassResetDeviceSwitchCount))

type GetPassCustomerAvailablePasses =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "availablePasses"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> Get ('[JSON]) [API.Types.UI.Pass.PassInfoAPIEntity]
  )

type GetPassCustomerPurchasedPasses =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "purchasedPasses"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> QueryParam "status" Domain.Types.PurchasedPass.StatusType
      :> Get
           ('[JSON])
           [API.Types.UI.Pass.PurchasedPassAPIEntity]
  )

type GetPassCustomerTransactions =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "transactions" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get ('[JSON]) [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity]
  )

type PostPassCustomerActivateToday =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "activateToday"
      :> Capture
           "passNumber"
           Kernel.Prelude.Int
      :> QueryParam "startDay" Data.Time.Day
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostPassCustomerPassSelect =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "pass"
      :> Capture
           "passId"
           (Kernel.Types.Id.Id Domain.Types.Pass.Pass)
      :> "select"
      :> ReqBody ('[JSON]) PurchasedPassSelectReq
      :> Post ('[JSON]) API.Types.UI.Pass.PassSelectionAPIEntity
  )

type GetPassCustomerPaymentStatus =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "payment"
      :> Capture
           "orderId"
           (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder)
      :> "status"
      :> Get ('[JSON]) Lib.Payment.Domain.Action.PaymentStatusResp
  )

type PostPassCustomerPassResetDeviceSwitchCount =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "pass"
      :> Capture
           "passId"
           (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass)
      :> "resetDeviceSwitchCount"
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

data PassAPIs = PassAPIs
  { getPassCustomerAvailablePasses :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> EulerHS.Types.EulerClient [API.Types.UI.Pass.PassInfoAPIEntity]),
    getPassCustomerPurchasedPasses :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Kernel.Prelude.Maybe (Domain.Types.PurchasedPass.StatusType) -> EulerHS.Types.EulerClient [API.Types.UI.Pass.PurchasedPassAPIEntity]),
    getPassCustomerTransactions :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity]),
    postPassCustomerActivateToday :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Data.Time.Day) -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postPassCustomerPassSelect :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> PurchasedPassSelectReq -> EulerHS.Types.EulerClient API.Types.UI.Pass.PassSelectionAPIEntity),
    getPassCustomerPaymentStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> EulerHS.Types.EulerClient Lib.Payment.Domain.Action.PaymentStatusResp),
    postPassCustomerPassResetDeviceSwitchCount :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkPassAPIs :: (Client EulerHS.Types.EulerClient API -> PassAPIs)
mkPassAPIs passClient = (PassAPIs {..})
  where
    getPassCustomerAvailablePasses :<|> getPassCustomerPurchasedPasses :<|> getPassCustomerTransactions :<|> postPassCustomerActivateToday :<|> postPassCustomerPassSelect :<|> getPassCustomerPaymentStatus :<|> postPassCustomerPassResetDeviceSwitchCount = passClient

data PassUserActionType
  = GET_PASS_CUSTOMER_AVAILABLE_PASSES
  | GET_PASS_CUSTOMER_PURCHASED_PASSES
  | GET_PASS_CUSTOMER_TRANSACTIONS
  | POST_PASS_CUSTOMER_ACTIVATE_TODAY
  | POST_PASS_CUSTOMER_PASS_SELECT
  | GET_PASS_CUSTOMER_PAYMENT_STATUS
  | POST_PASS_CUSTOMER_PASS_RESET_DEVICE_SWITCH_COUNT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''PassUserActionType)])
