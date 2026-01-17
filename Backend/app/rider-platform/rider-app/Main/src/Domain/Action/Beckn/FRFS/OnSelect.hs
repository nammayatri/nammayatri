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
import qualified Domain.Types.FRFSQuote as DQuote
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as Merchant
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Storage.Beam.BeamFlow
import SharedLogic.FRFSConfirm
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSQuote as Qquote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Tools.Metrics as Metrics
import ExternalBPP.CallAPI.Types

validateRequest :: (EsqDBReplicaFlow m r, BeamFlow m r) => DOnSelect -> m (Merchant.Merchant, DQuote.FRFSQuote, DIBC.IntegratedBPPConfig)
validateRequest DOnSelect {..} = do
  _ <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  quote <- runInReplica $ Qquote.findById (Id messageId) >>= fromMaybeM (QuoteDoesNotExist messageId)
  let merchantId = quote.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity quote
  return (merchant, quote, integratedBppConfig)

onSelect :: (FRFSConfirmFlow m r c) => DOnSelect -> Merchant.Merchant -> DQuote.FRFSQuote -> Maybe Bool -> Maybe Bool -> Maybe CrisSdkResponse -> DIBC.IntegratedBPPConfig -> m ()
onSelect onSelectReq merchant quote isSingleMode mbEnableOffer crisSdkResponse integratedBppConfig = do
  logDebug $ "onSelect isSingleMode: " <> show isSingleMode <> " mbEnableOffer: " <> show mbEnableOffer <> " crisSdkResponse: " <> show crisSdkResponse
  Metrics.finishMetrics Metrics.SELECT_FRFS merchant.name onSelectReq.transactionId quote.merchantOperatingCityId.getId
  whenJust (onSelectReq.validTill) (\validity -> void $ Qquote.updateValidTillById quote.id validity)
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
  let categorySelectionReq =
        mapMaybe
          ( \category ->
              (find (\category' -> category'.category == category.category) quoteCategories)
                <&> (\category' -> (FRFSCategorySelectionReq {quantity = category.quantity, quoteCategoryId = category'.id}))
          )
          onSelectReq.categories
  void $ postFrfsQuoteV2ConfirmUtil (Just quote.riderId, merchant.id) quote categorySelectionReq crisSdkResponse isSingleMode mbEnableOffer integratedBppConfig
