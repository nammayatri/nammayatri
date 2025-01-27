module Domain.Action.Dashboard.RideBooking.Frontend
  ( getFrontendFlowStatus,
    postFrontendNotifyEvent,
  )
where

import qualified "this" Domain.Action.UI.Frontend
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QP

getFrontendFlowStatus ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Environment.Flow Domain.Action.UI.Frontend.GetPersonFlowStatusRes
getFrontendFlowStatus _merchantShortId _opCity personId isPolling checkForActiveBooking = do
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Domain.Action.UI.Frontend.getPersonFlowStatus personId person.merchantId isPolling checkForActiveBooking

postFrontendNotifyEvent ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Domain.Action.UI.Frontend.NotifyEventReq ->
  Environment.Flow Domain.Action.UI.Frontend.NotifyEventResp
postFrontendNotifyEvent _merchantShortId _opCity personId req = do
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Domain.Action.UI.Frontend.notifyEvent personId person.merchantId req
