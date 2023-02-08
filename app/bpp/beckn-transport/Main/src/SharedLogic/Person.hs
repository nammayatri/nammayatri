module SharedLogic.Person where

import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QP
import Tools.Error

findPerson :: Id DP.Person -> Flow DP.Person
findPerson personId = do
  QP.findById personId
    >>= fromMaybeM (PersonNotFound personId.getId)
