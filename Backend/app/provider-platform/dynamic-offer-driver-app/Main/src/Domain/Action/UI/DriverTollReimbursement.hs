{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverTollReimbursement
  ( listMyTollReimbursements,
    submitManualClaim,
    getTollReimbursementDetails,
    ManualClaimReq (..),
    TollReimbursementListRes (..),
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TollBooth as DTB
import qualified Domain.Types.TollReimbursement as DTRB
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common (Currency (..), HighPrecMoney)
import Kernel.Types.Error
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (generateGUID, getCurrentTime, logInfo)
import Kernel.Utils.Error (fromMaybeM)
import qualified Storage.Queries.TollReimbursement as QTollReimbursement

data ManualClaimReq = ManualClaimReq
  { rideId :: Id DRide.Ride,
    amount :: HighPrecMoney,
    currency :: Currency,
    tollBoothId :: Maybe (Id DTB.TollBooth),
    claimEvidence :: Maybe Text,
    disputeReason :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data TollReimbursementListRes = TollReimbursementListRes
  { reimbursements :: [DTRB.TollReimbursement],
    totalCount :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

listMyTollReimbursements ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m TollReimbursementListRes
listMyTollReimbursements (driverId, _merchantId, _merchantOpCityId) = do
  reimbursements <- QTollReimbursement.findAllByDriverId driverId
  pure $ TollReimbursementListRes {reimbursements, totalCount = length reimbursements}

submitManualClaim ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ManualClaimReq ->
  m DTRB.TollReimbursement
submitManualClaim (driverId, merchantId, merchantOpCityId) req = do
  now <- getCurrentTime
  reimbursementId <- generateGUID
  let reimbursement =
        DTRB.TollReimbursement
          { id = Id reimbursementId,
            driverId = driverId,
            rideId = req.rideId,
            tollBoothId = req.tollBoothId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            amount = req.amount,
            currency = req.currency,
            detectionMethod = DTRB.MANUAL_CLAIM,
            reimbursementStatus = DTRB.DETECTED,
            ledgerEntryId = Nothing,
            claimEvidence = req.claimEvidence,
            disputeReason = req.disputeReason,
            reviewedBy = Nothing,
            reviewedAt = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QTollReimbursement.create reimbursement
  logInfo $ "Driver " <> show driverId <> " submitted manual toll claim: " <> reimbursementId
  pure reimbursement

getTollReimbursementDetails ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DTRB.TollReimbursement ->
  m DTRB.TollReimbursement
getTollReimbursementDetails (driverId, _merchantId, _merchantOpCityId) reimbursementId = do
  reimbursement <- QTollReimbursement.findById reimbursementId >>= fromMaybeM (InvalidRequest "Toll reimbursement not found")
  unless (reimbursement.driverId == driverId) $
    throwError $ InvalidRequest "Toll reimbursement does not belong to this driver"
  pure reimbursement
