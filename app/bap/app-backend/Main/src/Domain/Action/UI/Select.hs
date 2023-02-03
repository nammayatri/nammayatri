{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Select
  ( DSelectRes (..),
    SelectListRes (..),
    select,
    selectList,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto (runInReplica)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Domain.Types.Quote (QuoteAPIEntity (..))
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchReq
import Domain.Types.VehicleVariant (VehicleVariant)
import Environment
import SharedLogic.Share (checkIfEstimateCancelled)
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error

data DSelectRes = DSelectRes
  { searchRequest :: DSearchReq.SearchRequest,
    estimateId :: Id DEstimate.Estimate,
    providerId :: Text,
    providerUrl :: BaseUrl,
    variant :: VehicleVariant
  }

newtype SelectListRes = SelectListRes
  { selectedQuotes :: [QuoteAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

select :: Id DPerson.Person -> Id DEstimate.Estimate -> Flow DSelectRes
select personId estimateId = do
  now <- getCurrentTime
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  checkIfEstimateCancelled estimate.id estimate.status
  let searchRequestId = estimate.requestId
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist personId.getId)
  when ((searchRequest.validTill) < now) $
    throwError SearchRequestExpired
  Esq.runTransaction $ do
    QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimateId, validTill = searchRequest.validTill}
    QEstimate.updateStatus estimateId $ Just DEstimate.DRIVER_QUOTE_REQUESTED
  pure
    DSelectRes
      { providerId = estimate.providerId,
        providerUrl = estimate.providerUrl,
        variant = estimate.vehicleVariant,
        ..
      }

selectList :: (EsqDBReplicaFlow m r) => Id DEstimate.Estimate -> m SelectListRes
selectList estimateId = do
  estimate <- runInReplica $ QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  checkIfEstimateCancelled estimate.id estimate.status
  selectedQuotes <- runInReplica $ QQuote.findAllByEstimateId estimateId
  pure $ SelectListRes $ map DQuote.makeQuoteAPIEntity selectedQuotes
