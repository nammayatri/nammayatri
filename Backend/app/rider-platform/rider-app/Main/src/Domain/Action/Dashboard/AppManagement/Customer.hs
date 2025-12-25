{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Dashboard.AppManagement.Customer (postCustomerSosCreate, postCustomerDeletedPerson) where

import qualified API.Types.UI.DeletedPerson
import qualified "this" API.Types.UI.Sos
import qualified Domain.Action.UI.DeletedPerson
import qualified Domain.Action.UI.Sos
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)

postCustomerSosCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.Sos.SosReq -> Environment.Flow API.Types.UI.Sos.SosRes)
postCustomerSosCreate merchantShortId _opCity personId req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.Sos.postSosCreate (Just personId, m.id) req

postCustomerDeletedPerson :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.DeletedPerson.DeletedPersonReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postCustomerDeletedPerson merchantShortId _opCity personId req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.DeletedPerson.postDeletedPerson (Just personId, m.id) req

instance Kernel.Types.HideSecrets.HideSecrets API.Types.UI.Sos.SosReq where
  hideSecrets = Kernel.Prelude.identity

instance Kernel.Types.HideSecrets.HideSecrets API.Types.UI.DeletedPerson.DeletedPersonReq where
  hideSecrets = Kernel.Prelude.identity
