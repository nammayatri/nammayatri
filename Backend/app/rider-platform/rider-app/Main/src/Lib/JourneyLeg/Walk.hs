module Lib.JourneyLeg.Walk where

data WalkLegUpdateData = WalkLegUpdateData
  { id : Id LegID
    mbFromLocation : Location
    mbtoLocation : Location
    mbStartTime : Maybe UTCTime
    status : JourneyLegStatus
  }

data WalkSearchData = WalkSearchData 
  { fromLocation :: Location
  , stops :: [Location]
  , merchantId :: Id Merchant
  , personId: Id Person
  , merchantOperatingCity: Id MerchantOperatingCity
  }

data WalkLegRequest = WalkLegRequestSearch MultiModalLeg WalkSearchData | WalkLegRequestConfirm WalkLegConfirmRequest | WalkLegRequestCancel WalkLegCancelRequest | WalkLegRequestUpdate WalkLegUpdateData

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
  getState (WalkLeg _legData) = return JourneyLegState -- id
  get (WalkLeg _legData) = return WalkLeg -- id
