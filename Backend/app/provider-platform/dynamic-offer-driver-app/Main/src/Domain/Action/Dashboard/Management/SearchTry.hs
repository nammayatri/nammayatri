{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.SearchTry
  ( postSearchTryRecent,
  )
where

import qualified API.Types.ProviderPlatform.Management.SearchTry as Common
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Domain.Types.Merchant as DM
import qualified Environment
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.RiderDetailsExtra as QRD
import qualified Storage.Queries.SearchRequestExtra as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error (GenericError (InvalidRequest))

postSearchTryRecent ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.RecentSearchTriesReq ->
  Environment.Flow Common.RecentSearchTriesRes
postSearchTryRecent merchantShortId _opCity req = do
  when (null req.phoneNumbers) $ throwError (InvalidRequest "phoneNumbers cannot be empty")
  let limit = fromMaybe 30 req.limit
  when (limit <= 0) $ throwError (InvalidRequest "limit must be positive")

  merchant <- findMerchantByShortId merchantShortId
  let uniquePhones = DL.nub req.phoneNumbers
  phoneHashes <- forM uniquePhones $ \phone -> do
    phoneHash <- getDbHash phone
    pure (phoneHash, phone)
  let hashToPhone = Map.fromList phoneHashes

  riderDetailsList <- runInReplica $ QRD.findAllByMobileNumberHashesAndMerchant (map fst phoneHashes) merchant.id
  if null riderDetailsList
    then pure Common.RecentSearchTriesRes {searchTries = []}
    else do
      let riderIdToPhone =
            Map.fromList $
              mapMaybe
                ( \rd ->
                    (rd.id.getId,) <$> Map.lookup (rd.mobileNumber.hash) hashToPhone
                )
                riderDetailsList

      -- Fetch recent search requests for these riders (bounded by limit)
      searchReqs <- runInReplica $ QSR.findRecentByRiderIds (map (.id) riderDetailsList) limit

      -- Fetch recent search tries for those requests, then keep latest per searchRequestId
      let requestIds = map (.id) searchReqs
      searchTries <-
        if null requestIds
          then pure []
          else runInReplica $ QST.findRecentByRequestIds requestIds limit

      let latestByRequestId =
            Map.fromListWith
              ( \a b ->
                  if a.createdAt >= b.createdAt then a else b
              )
              [(st.requestId.getId, st) | st <- searchTries]

          searchReqById = Map.fromList [(sr.id.getId, sr) | sr <- searchReqs]

          items =
            mapMaybe
              ( \(requestId, st) -> do
                  sr <- Map.lookup requestId searchReqById
                  riderId <- sr.riderId
                  phoneNumber <- Map.lookup riderId.getId riderIdToPhone
                  pure $
                    Common.RecentSearchTryItem
                      { phoneNumber = phoneNumber,
                        riderId = riderId.getId,
                        searchRequestId = requestId,
                        searchTryId = st.id.getId,
                        status = show st.status,
                        vehicleServiceTier = show st.vehicleServiceTier,
                        searchRepeatCounter = st.searchRepeatCounter,
                        searchRepeatType = show st.searchRepeatType,
                        tripCategory = show st.tripCategory,
                        createdAt = st.createdAt,
                        validTill = st.validTill
                      }
              )
              (Map.toList latestByRequestId)

      -- Keep most recent search tries first, still respecting limit after grouping
      let sortedItems = DL.take limit $ DL.sortOn (Down . (.createdAt)) items
      pure Common.RecentSearchTriesRes {searchTries = sortedItems}
