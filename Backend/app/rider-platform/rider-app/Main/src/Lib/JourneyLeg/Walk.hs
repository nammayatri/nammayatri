module Lib.JourneyLeg.Walk where

import qualified Lib.JourneyLeg.Types as JT


data WalkLegUpdateData = WalkLegUpdateData
  { id :: Id LegID
    mbFromLocation :: Location
    mbtoLocation :: Location
    mbStartTime :: Maybe UTCTime
    status :: JourneyLegStatus
  }

data WalkSearchData = WalkSearchData 
  { fromLocation :: Location
  , stops :: [Location]
  , merchantId :: Id Merchant
  , personId :: Id Person
  , merchantOperatingCity :: Id MerchantOperatingCity
  }

data WalkLegRequest = WalkLegRequestSearch MultiModalLeg WalkSearchData | WalkLegRequestConfirm WalkLegConfirmRequest | WalkLegRequestCancel WalkLegCancelRequest | WalkLegRequestUpdate WalkLegUpdateData | WalkLegGetState (Id LegID)

updateWalkLegById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.Walkleg.Walkleg  ->  -> Id LegID -> m ()
updateWalkLegById (Domain.Types.Walkleg.Walkleg {..}) legId = do
   -- Function implementation goes here
    -- You can now use legId along with the fields of the Walkleg record
instance JourneyLeg WalkLeg m where
  search (WalkLegRequestSearch multimodalLeg walkLegSearchData) = do
    WL.create $ mkWalkLegSearch multimodalLeg walkLegSearchData
  confirm (WalkLeg _legData) = return () -- return nothing
  update (WalkLegRequest $ WalkLegRequestUpdate walkLegUpdateData) = do
    WL.updateWalkLegById $ walkLegUpdateData walkLegUpdateData.id
    -- WalkLegUpdateRequest :: id, mbFromLocation, mbtoLocation, mbStartTime
  cancel (WalkLeg $ Cancel WalkLegCancelRequest) JourneyLegStatus = return ()
    -- update JourneyLegStatus: Cancelled
    WalkLegCancelRequest :: id

  getState (WalkLegGetState legId) = do
    legData <- findById legId 
    if legData.isSkipped == False && legData.isCancelled == False 
      then do
        if legData.legNo > 1 then return $ JT.JourneyLegState { status = InPlan, currentPosition = Nothing }
          else return $ JT.JourneyLegState { status = Ongoing, currentPosition = Nothing }
      else if legData.isSkipped == True
        then return $ JT.JourneyLegState { status = Skipped, currentPosition = Nothing }
        else return $ JT.JourneyLegState { status = Cancelled, currentPosition = Nothing }
   
  get (WalkLegGetState legId) = do
    legData <- findById legId 
    return legData
