{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.SearchTryPoolStats
  ( BatchPoolStats (..),
    SearchTryPoolStatsResp (..),
    getSearchTryPoolStats,
  )
where

import qualified Data.List as DL
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.SearchTry as DST
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverPool as DP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestForDriverExtra as QSRD
import qualified Storage.Queries.SearchTry as QST
import Tools.Error

data BatchPoolStats = BatchPoolStats
  { batchNum :: Int,
    driversSent :: Int,
    accepts :: Int,
    rejects :: Int,
    pulled :: Int,
    unanswered :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SearchTryPoolStatsResp = SearchTryPoolStatsResp
  { searchTryId :: Text,
    currentBatchNum :: Int,
    cumulativeRejectCount :: Int,
    perBatch :: [BatchPoolStats]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Per-batch dispatch outcome for one search try: how many drivers each batch went to and what
-- they did with it. Read-only, built from the search_request_for_driver rows plus the Redis
-- counters the allocator and the respond API keep, so it can be polled while a search is live.
getSearchTryPoolStats :: Id DST.SearchTry -> Maybe Text -> FlowHandler SearchTryPoolStatsResp
getSearchTryPoolStats searchTryId apiKey = withFlowHandlerAPI $ do
  searchTry <- QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
  searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantDoesNotExist searchReq.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  srfds <- QSRD.findAllBySTId searchTryId
  cumulativeRejectCount <- DP.getSearchTryRejectCount searchTryId
  let batchNums = DL.sort . DL.nub $ map (.batchNumber) srfds
  perBatch <- forM batchNums $ \batchNum -> do
    let rows = filter ((== batchNum) . (.batchNumber)) srfds
        countOf response = length $ filter ((== Just response) . (.response)) rows
        accepts = countOf DCommon.Accept
        rejects = countOf DCommon.Reject
        pulled = countOf DCommon.Pulled
    -- Recorded by the allocator when the batch went out. Preferred over the row count because it
    -- leaves out the synthetic "drivers exhausted" marker row, which is not an offer to anyone.
    mbSentCount <- DP.getBatchSentCount searchTryId batchNum
    let driversSent = fromMaybe (length rows) mbSentCount
    pure
      BatchPoolStats
        { batchNum,
          driversSent,
          accepts,
          rejects,
          pulled,
          unanswered = max 0 (driversSent - accepts - rejects - pulled)
        }
  pure
    SearchTryPoolStatsResp
      { searchTryId = searchTryId.getId,
        currentBatchNum = if null batchNums then -1 else DL.last batchNums,
        cumulativeRejectCount,
        perBatch
      }
