{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Provider where

import qualified API.Action.Provider.Person
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = API.Action.Provider.Person.API

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Provider.Person.handler merchantId city
