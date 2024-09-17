{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.MultiModalFareComputation
  ( getMultimodalFare,
    postMultimodalFare,
  )
where

import qualified API.Types.UI.MultiModalFareComputation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MultiModalFareLegRules
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Servant
import qualified Storage.Queries.MultiModalFareLegRules as QFLR
import Tools.Auth

getMultimodalFare ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules ->
    Environment.Flow API.Types.UI.MultiModalFareComputation.MultiModalFare
  )
getMultimodalFare (_, _) fareLegRulesId = do
  fareLegRules <- QFLR.findById fareLegRulesId >>= fromMaybeM (InvalidRequest "No fareLegRules found")
  pure $
    API.Types.UI.MultiModalFareComputation.MultiModalFare
      { currency = Domain.Types.MultiModalFareLegRules.currency fareLegRules,
        fare = Domain.Types.MultiModalFareLegRules.amount fareLegRules
      }

postMultimodalFare ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.MultiModalFareComputation.GetFareReq ->
    Environment.Flow API.Types.UI.MultiModalFareComputation.MultiModalFare
  )
postMultimodalFare (_, _) getFareReq = do
  fareLeg <- QFLR.getFare getFareReq
  pure $ fareLeg
