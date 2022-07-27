{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Domain.Action.UI.Select where

import App.Types
import Beckn.Prelude
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.SearchReqLocation as SRLoc
import qualified Domain.Types.SearchRequest as DSearchReq
import Domain.Types.VehicleVariant (VehicleVariant)
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Types.Error
import Utils.Common

data DSelectReq = DSelectReq
  { searchRequest :: DSearchReq.SearchRequest,
    estimateId :: Id DEstimate.Estimate,
    fromLocation :: SRLoc.SearchReqLocation,
    toLocation :: Maybe SRLoc.SearchReqLocation,
    providerId :: Text,
    providerUrl :: BaseUrl,
    variant :: VehicleVariant
  }

select :: Id DPerson.Person -> Id DEstimate.Estimate -> Flow DSelectReq
select personId estimateId = do
  now <- getCurrentTime
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (QuoteDoesNotExist estimateId.getId) --FIXME EstimateDoesNotExist
  let searchRequestId = estimate.requestId
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist personId.getId)
  when ((searchRequest.validTill) < now) $
    throwError SearchRequestExpired
  fromLocation <- QLoc.findById searchRequest.fromLocationId >>= fromMaybeM (InternalError "fromLocation not found") --FIXME
  toLocation <-
    case searchRequest.toLocationId of
      Nothing -> pure Nothing
      Just toLocId -> (QLoc.findById toLocId >>= fromMaybeM (InternalError "toLocation not found")) <&> Just --FIXME
  pure
    DSelectReq
      { providerId = estimate.providerId,
        providerUrl = estimate.providerUrl,
        variant = estimate.vehicleVariant,
        ..
      }
