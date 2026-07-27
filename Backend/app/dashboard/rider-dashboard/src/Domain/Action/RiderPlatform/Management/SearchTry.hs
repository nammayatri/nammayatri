{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.SearchTry
  ( postSearchTryRecentSearchTries,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.SearchTry as ProviderSearch
import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.SearchTry as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver as ProviderClient
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postSearchTryRecentSearchTries ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Context.City ->
  ApiTokenInfo ->
  Common.RecentSearchTriesReq ->
  Environment.Flow Common.RecentSearchTriesRes
postSearchTryRecentSearchTries merchantShortId opCity apiTokenInfo req = do
  void $ merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Just apiTokenInfo)
      Nothing
      Nothing
      (Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    -- BAP merchant shortIds map to BPP partner shortIds (e.g. NAMMA_YATRI → NAMMA_YATRI_PARTNER)
    let bppMerchantShortId = Kernel.Types.Id.ShortId (merchantShortId.getShortId <> "_PARTNER")
    -- Call BPP driver-app API with the same phoneNumbers / limit input
    bppRes <-
      ProviderClient.callDriverOfferBppRecentSearchTries bppMerchantShortId opCity (.postRecentSearchTries) $
        ProviderSearch.RecentSearchTriesReq
          { phoneNumbers = req.phoneNumbers,
            limit = req.limit
          }
    pure $
      Common.RecentSearchTriesRes
        { searchTries = map toBapItem bppRes.searchTries
        }
  where
    toBapItem :: ProviderSearch.RecentSearchTryItem -> Common.RecentSearchTryItem
    toBapItem item =
      Common.RecentSearchTryItem
        { phoneNumber = item.phoneNumber,
          riderId = item.riderId,
          searchRequestId = item.searchRequestId,
          searchTryId = item.searchTryId,
          status = item.status,
          vehicleServiceTier = item.vehicleServiceTier,
          searchRepeatCounter = item.searchRepeatCounter,
          searchRepeatType = item.searchRepeatType,
          tripCategory = item.tripCategory,
          createdAt = item.createdAt,
          validTill = item.validTill
        }
