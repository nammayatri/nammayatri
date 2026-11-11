module Domain.Action.UI.SharedCabInternal
  ( postSharedCabAllocationFCM,
    postSharedCabCheckInFCM,
    getSharedCabDriverCallNumber,
    getSharedCabTripActive,
    postSharedCabTripExpire,
  )
where

import API.Types.UI.SharedCabInternal (SharedCabCheckInReq, SharedCabDriverCallNumberResp, SharedCabTrip)
import qualified Domain.Action.Internal.SharedCabAllocation as DAllocation
import qualified Domain.Action.Internal.SharedCabCallNumber as DCallNumber
import qualified Domain.Action.Internal.SharedCabTrip as DTrip
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id (Id)
import Tools.Notifications (SharedCabAllocationEntityData)

postSharedCabAllocationFCM :: Maybe Text -> SharedCabAllocationEntityData -> Flow APISuccess
postSharedCabAllocationFCM mbToken req = DAllocation.sharedCabAllocationFCM req mbToken

postSharedCabCheckInFCM :: Maybe Text -> SharedCabCheckInReq -> Flow APISuccess
postSharedCabCheckInFCM mbToken req = DAllocation.sharedCabCheckInFCM req mbToken

getSharedCabDriverCallNumber :: Text -> Maybe Text -> Flow SharedCabDriverCallNumberResp
getSharedCabDriverCallNumber = DCallNumber.sharedCabDriverCallNumber

getSharedCabTripActive :: Id Merchant -> Text -> Maybe Text -> Flow (Maybe SharedCabTrip)
getSharedCabTripActive = DTrip.sharedCabActiveTrip

postSharedCabTripExpire :: Id Merchant -> Text -> Maybe HighPrecMoney -> Maybe Text -> Flow APISuccess
postSharedCabTripExpire = DTrip.expireSharedCabTrip
