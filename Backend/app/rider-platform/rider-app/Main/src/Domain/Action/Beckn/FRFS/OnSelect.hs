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
import qualified Domain.Types.IntegratedBPPConfig as DIBC
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
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSQuote as Qquote
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.IntegratedBPPConfig as QIBC
import Tools.Error
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

onSelect :: FRFSConfirmFlow m r => DOnSelect -> Merchant.Merchant -> DQuote.FRFSQuote -> m ()
onSelect onSelectReq merchant quote = do
  whenJust (onSelectReq.validTill) (\validity -> void $ Qquote.updateValidTillById quote.id validity)
  Qquote.updatePriceById quote.id onSelectReq.totalPrice
  platformType <- case quote.integratedBppConfigId of
    Just integratedBppConfigId' -> do
      ibppConfig <- QIBC.findById integratedBppConfigId' >>= fromMaybeM (IntegratedBPPConfigNotFound integratedBppConfigId'.getId)
      return $ DIBC.platformType ibppConfig
    Nothing -> return DIBC.APPLICATION
  let req = FRFSQuoteConfirmReq {discounts = []}
  void $ FRFSTicketService.postFrfsQuoteV2ConfirmUtil (Just quote.riderId, merchant.id) quote.id req platformType Nothing
