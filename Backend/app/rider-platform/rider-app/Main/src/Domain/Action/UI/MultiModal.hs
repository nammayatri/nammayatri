{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.MultiModal where

import qualified API.Types.UI.MultiModal
import qualified Domain.Types.Estimate
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id

postMultiModalRouteDetails ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Estimate.Estimate ->
    Environment.Flow API.Types.UI.MultiModal.MultiModalRouteDetails
  )
postMultiModalRouteDetails = do error "Logic yet to be decided"
