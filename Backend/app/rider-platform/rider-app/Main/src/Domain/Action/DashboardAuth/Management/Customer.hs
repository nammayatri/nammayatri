{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handler for the direct-dashboard customer offers lookup: the offers are shown in
-- the dashboard user's language, in the customer's when the user has none set.
module Domain.Action.DashboardAuth.Management.Customer
  ( postCustomerOffersList,
  )
where

import qualified API.Types.RiderPlatform.Management.Customer
import qualified Domain.Action.Dashboard.Customer
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person.Type as Fields
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

postCustomerOffersList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Customer.CustomerOffersListReq -> Environment.Flow [API.Types.RiderPlatform.Management.Customer.CustomerOfferEntity])
postCustomerOffersList a4 a3 a2 a1 = Domain.Action.Dashboard.Customer.postCustomerOffersList a4 a3 a2.person.language a1
