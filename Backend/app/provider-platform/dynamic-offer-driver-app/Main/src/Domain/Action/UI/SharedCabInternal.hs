module Domain.Action.UI.SharedCabInternal
  ( postSharedCabAllocationFCM,
    getSharedCabDriverCallNumber,
    postSharedCabCabEvent,
  )
where

import API.Types.UI.SharedCabInternal (SharedCabDriverCallNumberResp)
import qualified Domain.Action.Internal.SharedCabAllocation as DAllocation
import qualified Domain.Action.Internal.SharedCabCabEvent as DCabEvent
import qualified Domain.Action.Internal.SharedCabCallNumber as DCallNumber
import Domain.Types.SharedCabSession (CabEventReq, CabEventResp)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess)
import Tools.Notifications (SharedCabAllocationEntityData)

postSharedCabAllocationFCM :: Maybe Text -> SharedCabAllocationEntityData -> Flow APISuccess
postSharedCabAllocationFCM mbToken req = DAllocation.sharedCabAllocationFCM req mbToken

getSharedCabDriverCallNumber :: Text -> Maybe Text -> Flow SharedCabDriverCallNumberResp
getSharedCabDriverCallNumber = DCallNumber.sharedCabDriverCallNumber

postSharedCabCabEvent :: Text -> Maybe Text -> CabEventReq -> Flow CabEventResp
postSharedCabCabEvent = DCabEvent.sharedCabCabEvent
