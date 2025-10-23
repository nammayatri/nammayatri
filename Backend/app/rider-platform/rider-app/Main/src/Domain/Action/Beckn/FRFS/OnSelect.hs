{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Domain.Action.Beckn.FRFS.OnSelect where

import API.Types.UI.FRFSTicketService
import Domain.Action.Beckn.FRFS.Common (DOnSelect (..))
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Types.FRFSQuote as DQuote
import qualified Domain.Types.Merchant as Merchant
import Kernel.Beam.Functions
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Storage.Beam.BeamFlow
import SharedLogic.CallFRFSBPP
import qualified SharedLogic.FRFSUtils as FRFSUtils
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSQuote as Qquote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Tools.Metrics as Metrics

type FRFSConfirmFlow m r =
  ( MonadFlow m,
    BeamFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasBAPMetrics m r,
    BecknAPICallFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  )

validateRequest :: (EsqDBReplicaFlow m r, BeamFlow m r) => DOnSelect -> m (Merchant.Merchant, DQuote.FRFSQuote)
validateRequest DOnSelect {..} = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  quote <- runInReplica $ Qquote.findById (Id messageId) >>= fromMaybeM (QuoteDoesNotExist messageId)
  let merchantId = quote.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, quote)

onSelect :: (FRFSConfirmFlow m r) => DOnSelect -> Merchant.Merchant -> DQuote.FRFSQuote -> Maybe Bool -> Maybe Bool -> m ()
onSelect onSelectReq merchant quote isSingleMode mbEnableOffer = do
  logDebug $ "onSelect isSingleMode: " <> show isSingleMode <> " mbEnableOffer: " <> show mbEnableOffer
  Metrics.finishMetrics Metrics.SELECT_FRFS merchant.name onSelectReq.transactionId quote.merchantOperatingCityId.getId
  whenJust (onSelectReq.validTill) (\validity -> void $ Qquote.updateValidTillById quote.id validity)
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
  updatedQuoteCategories <- FRFSUtils.updateQuoteCategoriesWithQuantitySelections (mapMaybe (\category -> (find (\category' -> category'.category == category.category) quoteCategories) <&> (\category' -> (category'.id, category.quantity))) onSelectReq.categories) quoteCategories
  let fareParameters = FRFSUtils.calculateFareParametersWithQuoteFallback (FRFSUtils.mkCategoryPriceItemFromQuoteCategories updatedQuoteCategories) quote
      adultPrice = fareParameters.adultItem <&> (.unitPrice)
  whenJust adultPrice $ \price -> do
    Qquote.updatePriceAndEstimatedPriceById quote.id price (Just price)
    QJourneyLeg.updateEstimatedFaresBySearchId (Just price.amount) (Just price.amount) (Just quote.searchId.getId)
  let categorySelectionReq =
        mapMaybe
          ( \category -> do
              selectedQuantity <- category.selectedQuantity
              return $ FRFSCategorySelectionReq {quantity = selectedQuantity, quoteCategoryId = category.id}
          )
          updatedQuoteCategories
  void $ FRFSTicketService.postFrfsQuoteV2ConfirmUtil (Just quote.riderId, merchant.id) quote.id categorySelectionReq Nothing isSingleMode mbEnableOffer
