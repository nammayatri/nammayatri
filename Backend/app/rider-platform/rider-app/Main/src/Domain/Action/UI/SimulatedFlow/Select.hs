module Domain.Action.UI.SimulatedFlow.Select where

import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))

simulateSelect :: (Applicative m) => Bool -> m APISuccess -> m APISuccess
simulateSelect isSimulated nonSimulatedAction = if isSimulated then pure Success else nonSimulatedAction
