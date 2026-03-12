{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Recon (API, handler) where

import qualified Beckn.ACL.OnRecon as ACLOnRecon
import qualified Beckn.ACL.Recon as ACLRecon
import qualified Beckn.OnDemand.Utils.Common as BppUtils
import qualified Beckn.Types.Core.Taxi.API.Recon as Recon
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Data.Aeson as A
import qualified Domain.Action.PPF.Recon as DRecon
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified SharedLogic.CallBAP as CallBAP
import Storage.Beam.Finance ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.BecknConfig as QBC
import TransactionLogs.PushLogs

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Recon.ReconAPIV2

handler :: FlowServer API
handler = recon

recon ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Recon.ReconReqV2 ->
  FlowHandler AckResponse
recon merchantId _authResult reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.reconReqContext
  BppUtils.withTransactionIdLogTag transactionId $ do
    logInfo "Received recon request from BAP (Collector)"
    -- Push ONDC transaction logs
    fork "recon received pushing ondc logs" do
      void $ pushLogs "recon" (A.toJSON reqV2) merchantId.getId "MOBILITY"
    -- Process recon and build on_recon response
    fork "recon request processing" do
      domainReq <- ACLRecon.buildReconDomainReq reqV2
      city <- BppUtils.getContextCity reqV2.reconReqContext
      moc <- CQMOC.findByMerchantIdAndCity merchantId city >>= fromMaybeM (InternalError $ "Operating city not found for merchant: " <> merchantId.getId)
      bppConfig <-
        listToMaybe <$> QBC.findByMerchantIdAndDomain (Just merchantId) "MOBILITY"
          >>= fromMaybeM (InternalError $ "No BecknConfig found for merchant: " <> merchantId.getId)
      results <- DRecon.processRecon bppConfig.subscriberId merchantId.getId moc.id.getId domainReq
      logInfo $ "Processed recon for " <> show (length results) <> " orders"
      -- Extract BAP URI from incoming context for on_recon response
      bapSubscriberId <- BppUtils.getContextBapId reqV2.reconReqContext
      let mbBapUri = reqV2.reconReqContext.contextBapUri
      onReconReq <- ACLOnRecon.buildOnReconReq bppConfig bapSubscriberId mbBapUri results
      logInfo $ "Dispatching on_recon response with " <> show (length results) <> " entries to BAP: " <> bapSubscriberId
      CallBAP.callOnReconV2 onReconReq merchantId
    pure Ack
