module API.UI.FarePolicy.OneWayFarePolicy (module Reexport, API, handler) where

import App.Types
import Beckn.Types.Id (Id (..))
import Domain.Action.UI.FarePolicy.OneWayFarePolicy as Reexport
  ( ListOneWayFarePolicyRes (..),
    UpdateOneWayFarePolicyReq (..),
    UpdateOneWayFarePolicyRes,
  )
import qualified Domain.Action.UI.FarePolicy.OneWayFarePolicy as OneWayFP
import Domain.Types.FarePolicy.OneWayFarePolicy
import qualified Domain.Types.FarePolicy.OneWayFarePolicy as DFarePolicy
import qualified Domain.Types.Person as SP
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common

type API =
  -- "oneWay" -- TODO: we should add this layer
  --   :> (
  AdminTokenAuth :> Get '[JSON] ListOneWayFarePolicyRes
    :<|> AdminTokenAuth
      :> Capture "farePolicyId" (Id OneWayFarePolicy)
      :> ReqBody '[JSON] UpdateOneWayFarePolicyReq
      :> Post '[JSON] UpdateOneWayFarePolicyRes

--  )

handler :: FlowServer API
handler =
  listOneWayFarePolicies
    :<|> updateOneWayFarePolicy

listOneWayFarePolicies :: SP.Person -> FlowHandler ListOneWayFarePolicyRes
listOneWayFarePolicies = withFlowHandlerAPI . OneWayFP.listOneWayFarePolicies

updateOneWayFarePolicy :: SP.Person -> Id DFarePolicy.OneWayFarePolicy -> UpdateOneWayFarePolicyReq -> FlowHandler UpdateOneWayFarePolicyRes
updateOneWayFarePolicy admin fpId = withFlowHandlerAPI . OneWayFP.updateOneWayFarePolicy admin fpId
