{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Select (API, handler) where

import qualified Beckn.ACL.Select as ACL
import qualified Beckn.OnDemand.Transformer.MSIL.Select as MSILSelect
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error
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
select transporterId (SignatureAuthResult _ subscriber) reqV2 = withFlowHandlerBecknAPI . ActorInfo.withRequestIdActorInfo $ do
  transactionId <- Utils.getTransactionId reqV2.selectReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "SelectV2 API Flow" "Reached"
    dSelectReq' <- ACL.buildSelectReqV2 subscriber reqV2
    merchant <- QMerch.findById transporterId >>= fromMaybeM (MerchantNotFound transporterId.getId)
    city <- Utils.getContextCity reqV2.selectReqContext
    moc <- CQMOC.findByMerchantIdAndCity transporterId city >>= fromMaybeM (InvalidRequest $ "Operating City " <> show city <> " not supported or not found")
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = moc.id.getId}) Nothing >>= fromMaybeM (TransporterConfigDoesNotExist moc.id.getId)
    let isPilotMerchant = fromMaybe False transporterConfig.enableScheduledCategorySignal

    if isPilotMerchant
      then do
        -- MSIL pilot: negotiatedFare is decided from the wire item's own price
        -- object (Beckn.OnDemand.Transformer.MSIL.Select.msilParser, see doc 28).
        -- Non-pilot merchants never run this parser and never pay for the Quote
        -- lookup below -- both only happen inside this branch.
        let dSelectReq = MSILSelect.msilParser reqV2.selectReqMessage dSelectReq'
            -- The wire item.id may resolve to a Quote instead of an Estimate for
            -- the new static/scheduled /select capability (doc 25 s3). Pilot
            -- merchants aren't restricted to Quote-based select only: an
            -- item.id that resolves to an Estimate still falls through to the
            -- same estimate flow non-pilot merchants use.
            itemIdText = case dSelectReq.estimateIds of
              (eid : _) -> eid.getId
              [] -> ""
        mbQuote <- QQuote.findById (Id itemIdText)
        case mbQuote of
          Just quote ->
            Redis.whenWithLockRedis (selectLockKey dSelectReq.messageId) 60 $ do
              (validatedMerchant, searchRequest, validatedQuote) <- DSelect.validateQuoteSelect transporterId quote.id dSelectReq.transactionId dSelectReq.negotiatedFare
              fork "select-quote request processing" $
                Redis.whenWithLockRedis (selectProcessingLockKey dSelectReq.messageId) 60 $
                  DSelect.handleQuoteSelect dSelectReq.messageId validatedMerchant searchRequest validatedQuote
              fork "select received pushing ondc logs" do
                void $ pushLogs "select" (toJSON reqV2) merchant.id.getId "MOBILITY"
          Nothing -> do
            -- item.id didn't resolve to a Quote -- whether that's fine (fall
            -- through to the ordinary Estimate flow) or a bad request (NACK)
            -- depends on whether this transaction was ever a scheduled ride:
            -- scheduled rides only ever go through the Quote-based flow (doc 25
            -- s3), so one that shows up here without a Quote is a genuine
            -- mismatch, not just a plain dynamic-offer select.
            searchReq <- QSR.findByTransactionIdAndMerchantId dSelectReq.transactionId transporterId >>= fromMaybeM (SearchRequestNotFound dSelectReq.transactionId)
            if searchReq.isScheduled
              then throwError $ InvalidRequest "Scheduled ride select must resolve to a Quote, item_id did not match any Quote"
              else runEstimateFlow merchant dSelectReq -- for allowing instant ride in msil
      else runEstimateFlow merchant dSelectReq'
    pure Ack
  where
    runEstimateFlow merchant dSelectReq =
      Redis.whenWithLockRedis (selectLockKey dSelectReq.messageId) 60 $ do
        (validatedMerchant, searchRequest, estimates) <- DSelect.validateRequest transporterId dSelectReq
        fork "select request processing" $
          Redis.whenWithLockRedis (selectProcessingLockKey dSelectReq.messageId) 60 $
            DSelect.handler validatedMerchant dSelectReq searchRequest estimates
        fork "select received pushing ondc logs" do
          void $ pushLogs "select" (toJSON reqV2) merchant.id.getId "MOBILITY"

selectLockKey :: Text -> Text
selectLockKey id = "Driver:Select:MessageId-" <> id

selectProcessingLockKey :: Text -> Text
selectProcessingLockKey id = "Driver:Select:Processing:MessageId-" <> id
