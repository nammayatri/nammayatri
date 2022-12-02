module SharedLogic.Person where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as DP
import Environment
import qualified Storage.Queries.Person as QP
import Tools.Error

findPerson :: Id DP.Person -> Flow DP.Person
findPerson personId = do
  QP.findById personId
    >>= fromMaybeM (PersonNotFound personId.getId)
