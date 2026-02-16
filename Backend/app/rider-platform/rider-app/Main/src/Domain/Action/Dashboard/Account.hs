module Domain.Action.Dashboard.Account (putAccountUpdateRole) where

import qualified Dashboard.Common
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InternalError))
import qualified Kernel.Types.Id
import Kernel.Utils.Common (throwError)

putAccountUpdateRole :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Types.Id.Id Dashboard.Common.Role -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putAccountUpdateRole _merchantShortId _opCity _personId _roleId = throwError . InternalError $ "This function should not be called: putAccountUpdateRole"
