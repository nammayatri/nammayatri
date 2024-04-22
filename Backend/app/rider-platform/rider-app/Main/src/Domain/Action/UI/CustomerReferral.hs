{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.CustomerReferral where

import API.Types.UI.CustomerReferral
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as PStats
import Tools.Auth
import Tools.Error

getCustomerRefferalCount :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Flow ReferredCustomers
getCustomerRefferalCount (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  stats <- PStats.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
  pure $ ReferredCustomers {count = stats.referralCount}
