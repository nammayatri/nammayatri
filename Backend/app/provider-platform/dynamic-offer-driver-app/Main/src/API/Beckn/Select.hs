{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Select (API, handler) where

import qualified Beckn.ACL.OnSelect as ACL
import qualified Beckn.ACL.Select as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSR
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified SharedLogic.CallBAP as BP
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.Quote as QQuote
import TransactionLogs.PushLogs

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Select.SelectAPIV2

handler :: FlowServer API
handler = select

select ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Select.SelectReqV2 ->
  FlowHandler AckResponse
select transporterId (SignatureAuthResult _ subscriber) reqV2 = withFlowHandlerBecknAPI $ do
  transactionId <- Utils.getTransactionId reqV2.selectReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "SelectV2 API Flow" "Reached"
    dSelectReq <- ACL.buildSelectReqV2 subscriber reqV2

    Redis.whenWithLockRedis (selectLockKey dSelectReq.messageId) 60 $ do
      validationResult <- DSelect.validateRequest transporterId dSelectReq
      case validationResult of
        DSelect.EstimateBasedSelect merchant searchRequest estimates -> do
          fork "select request processing" $ do
            Redis.whenWithLockRedis (selectProcessingLockKey dSelectReq.messageId) 60 $
              DSelect.handler merchant dSelectReq searchRequest estimates
          fork "select received pushing ondc logs" do
            void $ pushLogs "select" (toJSON reqV2) merchant.id.getId "MOBILITY"
        DSelect.QuoteBasedSelect merchant searchRequest -> do
          fork "select quote-based on_select" $ do
            sendOnSelectForQuote merchant searchRequest dSelectReq reqV2
          fork "select received pushing ondc logs" do
            void $ pushLogs "select" (toJSON reqV2) merchant.id.getId "MOBILITY"
    pure Ack

selectLockKey :: Text -> Text
selectLockKey id = "Driver:Select:MessageId-" <> id

selectProcessingLockKey :: Text -> Text
selectProcessingLockKey id = "Driver:Select:Processing:MessageId-" <> id

sendOnSelectForQuote :: DM.Merchant -> DSR.SearchRequest -> DSelect.DSelectReq -> Select.SelectReqV2 -> Flow ()
sendOnSelectForQuote merchant searchRequest dSelectReq reqV2 = do
  now <- getCurrentTime
  let quoteIds = map (\(Id eid) -> Id eid) dSelectReq.estimateIds
  mbQuotes <- mapM QQuote.findById quoteIds
  case catMaybes mbQuotes of
    (quote : _) -> do
      let vehicleCategory = Utils.mapServiceTierToCategory quote.vehicleServiceTier
      bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
      let bppSubscriberId = getShortId merchant.subscriberId
      bppUri <- BP.buildBppUrl merchant.id
      let bapId = searchRequest.bapId
          bapUri = searchRequest.bapUri
      msgId <- Utils.getMessageId reqV2.selectReqContext
      ttl <- bppConfig.onSelectTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
      context <- ContextV2.buildContextV2_1 Context.ON_SELECT Context.MOBILITY msgId (Just searchRequest.transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri) (fromMaybe merchant.city searchRequest.bapCity) (fromMaybe Context.India searchRequest.bapCountry) (Just ttl)
      let onSelectMessage = ACL.mkOnSelectMessageForQuoteV2 bppConfig merchant searchRequest quote now
      internalEndPointHashMap <- asks (.internalEndPointHashMap)
      void $ BP.callOnSelectV2Direct merchant.id bppSubscriberId bapUri internalEndPointHashMap (Spec.OnSelectReq context Nothing (Just onSelectMessage))
    [] -> logError "No quotes found for quote-based select"
