module Domain.Action.Dashboard.AppManagement.Customer (postCustomerSosCreate) where

import qualified "this" API.Types.UI.Sos
import qualified Domain.Action.UI.Sos
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)

postCustomerSosCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.Sos.SosReq -> Environment.Flow API.Types.UI.Sos.SosRes)
postCustomerSosCreate merchantShortId _opCity personId req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.Sos.postSosCreate (Just personId, m.id) req
