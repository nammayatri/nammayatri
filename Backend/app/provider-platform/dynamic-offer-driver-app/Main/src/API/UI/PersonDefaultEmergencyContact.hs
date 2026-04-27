{-# OPTIONS_GHC -Wno-orphans #-}

module API.UI.PersonDefaultEmergencyContact
  ( API,
    handler,
  )
where

import Domain.Action.UI.PersonDefaultEmergencyContact ()
-- HasEmergencyContactHandle instance
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common (withFlowHandlerAPI)
import qualified Safety.API.UI.PersonDefaultEmergencyNumber as EmergencyRoutes
import qualified Safety.Domain.Action.UI.PersonDefaultEmergencyNumber as EmergencyLib
import Servant
import Storage.Beam.Sos ()
import Tools.Auth

type API = "driver" :> EmergencyRoutes.PersonDefaultEmergencyNumberAPI TokenAuth

handler :: FlowServer API
handler =
  (withFlowHandlerAPI . EmergencyLib.getEmergencyContacts)
    :<|> (\authTuple req -> withFlowHandlerAPI $ EmergencyLib.updateEmergencyContacts authTuple req)
