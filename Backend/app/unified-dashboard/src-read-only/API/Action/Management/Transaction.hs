{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Management.Transaction
  ( API,
    handler,
  )
where

import qualified "this" API.Types.Management
import qualified "this" API.Types.Management.Transaction
import qualified Data.Text
import qualified Domain.Action.Management.Transaction
import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("/transaction" :> GetTransactionList)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getTransactionList merchantId city

type GetTransactionList = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('GET_MANAGEMENT_TRANSACTION_LIST) :> API.Types.Management.Transaction.GetTransactionList)

getTransactionList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Domain.Types.AccessMatrix.UserActionType) -> Environment.FlowHandler API.Types.Management.Transaction.ListTransactionResp)
getTransactionList merchantShortId opCity apiTokenInfo searchString limit offset requestorId driverId rideId endpoint = withFlowHandlerAPI' $ Domain.Action.Management.Transaction.getTransactionList merchantShortId opCity apiTokenInfo searchString limit offset requestorId driverId rideId endpoint
