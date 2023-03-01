{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Select
  ( DSelectReq (..),
    DEstimateSelectReq (..),
    DSelectRes (..),
    SelectListRes (..),
    select,
    selectList,
  )
where

import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Domain.Types.Quote (QuoteAPIEntity (..))
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchReq
import Domain.Types.VehicleVariant (VehicleVariant)
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runInReplica)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Validation
import SharedLogic.Estimate (checkIfEstimateCancelled)
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error

newtype DSelectReq = DSelectReq
  { customerExtraFee :: Maybe Money
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data DEstimateSelectReq = DEstimateSelectReq
  { customerExtraFee :: Maybe Money,
    autoAssignEnabled :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateDSelectReq :: Validate DEstimateSelectReq
validateDSelectReq DEstimateSelectReq {..} =
  sequenceA_
    [ validateField "customerExtraFee" customerExtraFee $ InMaybe $ InRange @Money 1 100
    ]

data DSelectRes = DSelectRes
  { searchRequest :: DSearchReq.SearchRequest,
    estimateId :: Id DEstimate.Estimate,
    providerId :: Text,
    providerUrl :: BaseUrl,
    variant :: VehicleVariant,
    customerExtraFee :: Maybe Money,
    autoAssignEnabled :: Bool
  }

newtype SelectListRes = SelectListRes
  { selectedQuotes :: [QuoteAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

select :: Id DPerson.Person -> Id DEstimate.Estimate -> DEstimateSelectReq -> Flow DSelectRes
select personId estimateId req@DEstimateSelectReq {..} = do
  runRequestValidation validateDSelectReq req
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
