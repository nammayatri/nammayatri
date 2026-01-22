{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Management where

import qualified API.Action.Management.AccessMatrix
import qualified API.Action.Management.Merchant
import qualified API.Action.Management.Person
import qualified API.Action.Management.Registration
import qualified API.Action.Management.Role
import qualified API.Action.Management.Transaction
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.Management.AccessMatrix.API :<|> API.Action.Management.Merchant.API :<|> API.Action.Management.Person.API :<|> API.Action.Management.Registration.API :<|> API.Action.Management.Role.API :<|> API.Action.Management.Transaction.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Management.AccessMatrix.handler merchantId city :<|> API.Action.Management.Merchant.handler merchantId city :<|> API.Action.Management.Person.handler merchantId city :<|> API.Action.Management.Registration.handler merchantId city :<|> API.Action.Management.Role.handler merchantId city :<|> API.Action.Management.Transaction.handler merchantId city
