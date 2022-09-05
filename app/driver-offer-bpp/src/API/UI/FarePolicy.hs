module API.UI.FarePolicy
  ( API,
    handler,
    DFarePolicy.ListFarePolicyRes (..),
    DFarePolicy.UpdateFarePolicyReq (..),
    DFarePolicy.UpdateFarePolicyRes,
  )
where

import Beckn.Types.Id (Id (..))
import Beckn.Utils.Common
import qualified Domain.Action.UI.FarePolicy as DFarePolicy
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude
import Servant
import Utils.Auth

type API =
  "org"
    :> ( "farePolicy"
           :> ( AdminTokenAuth :> Get '[JSON] DFarePolicy.ListFarePolicyRes
                  :<|> AdminTokenAuth
                    :> Capture "farePolicyId" (Id DFP.FarePolicy)
                    :> ReqBody '[JSON] DFarePolicy.UpdateFarePolicyReq
                    :> Post '[JSON] DFarePolicy.UpdateFarePolicyRes
              )
       )

handler :: FlowServer API
handler =
  listFarePolicies
    :<|> updateFarePolicy

listFarePolicies :: SP.Person -> FlowHandler DFarePolicy.ListFarePolicyRes
listFarePolicies = withFlowHandlerAPI . DFarePolicy.listFarePolicies

updateFarePolicy :: SP.Person -> Id DFP.FarePolicy -> DFarePolicy.UpdateFarePolicyReq -> FlowHandler DFarePolicy.UpdateFarePolicyRes
updateFarePolicy admin fpId = withFlowHandlerAPI . DFarePolicy.updateFarePolicy admin fpId
