module Domain.Action.UI.SharedCabInternal
  ( postSharedCabAllocationFCM,
    getSharedCabDriverCallNumber,
  )
where

import API.Types.UI.SharedCabInternal (SharedCabDriverCallNumberResp)
import qualified Domain.Action.Internal.SharedCabAllocation as DAllocation
import qualified Domain.Action.Internal.SharedCabCallNumber as DCallNumber
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess)
import Tools.Notifications (SharedCabAllocationEntityData)

postSharedCabAllocationFCM :: Maybe Text -> SharedCabAllocationEntityData -> Flow APISuccess
postSharedCabAllocationFCM mbToken req = DAllocation.sharedCabAllocationFCM req mbToken

getSharedCabDriverCallNumber :: Text -> Maybe Text -> Flow SharedCabDriverCallNumberResp
getSharedCabDriverCallNumber = DCallNumber.sharedCabDriverCallNumber
