{-# OPTIONS_GHC -Wno-orphans #-}

module API.UI.FinanceInvoice
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Domain.Action.UI.FinanceInvoice as DFinanceInvoice
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API =
  TokenAuth
    :> "ridePayment"
    :> "invoice"
    :> Capture "rideId" (Id DRide.Ride)
    :> Get '[JSON] API.FinanceInvoiceItem

handler :: FlowServer API
handler = getRidePaymentInvoices

getRidePaymentInvoices ::
  (Id DP.Person, Id DM.Merchant) ->
  Id DRide.Ride ->
  FlowHandler API.FinanceInvoiceItem
getRidePaymentInvoices (personId, merchantId) rideId =
  withFlowHandlerAPIPersonId personId $
    withPersonIdLogTag personId $
      DFinanceInvoice.getRidePaymentInvoice (Just personId, merchantId) rideId
