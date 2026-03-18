{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Settlement
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.Settlement as API
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Settlement
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

-- ---------------------------------------------------------------------------
-- API type
-- ---------------------------------------------------------------------------

type API =
  "settlement"
    :> ( "summary"
           :> QueryParam "dateFrom" Kernel.Prelude.UTCTime
           :> QueryParam "dateTo" Kernel.Prelude.UTCTime
           :> QueryParam "gateway" Kernel.Prelude.Text
           :> Get '[JSON] API.SettlementSummaryRes
           :<|> "list"
             :> QueryParam "amountMax" Kernel.Types.Common.HighPrecMoney
             :> QueryParam "amountMin" Kernel.Types.Common.HighPrecMoney
             :> QueryParam "dateFrom" Kernel.Prelude.UTCTime
             :> QueryParam "dateTo" Kernel.Prelude.UTCTime
             :> QueryParam "gateway" Kernel.Prelude.Text
             :> QueryParam "limit" Kernel.Prelude.Int
             :> QueryParam "offset" Kernel.Prelude.Int
             :> QueryParam "searchQuery" Kernel.Prelude.Text
             :> QueryParam "status" API.SettlementStatusFilter
             :> Get '[JSON] API.SettlementListRes
           :<|> Capture "settlementId" (Kernel.Types.Id.Id Dashboard.Common.PGPaymentSettlementReport)
             :> "details"
             :> Get '[JSON] API.SettlementDetailsRes
           :<|> "chargebacks"
             :> ( "list"
                    :> QueryParam "dateFrom" Kernel.Prelude.UTCTime
                    :> QueryParam "dateTo" Kernel.Prelude.UTCTime
                    :> QueryParam "limit" Kernel.Prelude.Int
                    :> QueryParam "offset" Kernel.Prelude.Int
                    :> QueryParam "status" API.ChargebackStatusFilter
                    :> Get '[JSON] API.ChargebackListRes
                    :<|> Capture "chargebackId" Kernel.Prelude.Text
                      :> "respond"
                      :> ReqBody '[JSON] API.ChargebackRespondReq
                      :> Post '[JSON] API.ChargebackRespondRes
                )
           :<|> "trend"
             :> QueryParam "dateFrom" Kernel.Prelude.UTCTime
             :> QueryParam "dateTo" Kernel.Prelude.UTCTime
             :> QueryParam "gateway" Kernel.Prelude.Text
             :> QueryParam "granularity" API.TrendGranularity
             :> Get '[JSON] API.SettlementTrendRes
       )

-- ---------------------------------------------------------------------------
-- Handler
-- ---------------------------------------------------------------------------

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city =
  getSettlementSummary merchantId city
    :<|> listSettlements merchantId city
    :<|> getSettlementDetails merchantId city
    :<|> ( listChargebacks merchantId city
             :<|> respondToChargeback merchantId city
         )
    :<|> getSettlementTrend merchantId city

getSettlementSummary ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Environment.FlowHandler API.SettlementSummaryRes
getSettlementSummary a3 a2 a1 b1 c1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Settlement.getSettlementSummary a3 a2 a1 b1 c1

listSettlements ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney ->
  Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe API.SettlementStatusFilter ->
  Environment.FlowHandler API.SettlementListRes
listSettlements a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Settlement.listSettlements a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getSettlementDetails ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Dashboard.Common.PGPaymentSettlementReport ->
  Environment.FlowHandler API.SettlementDetailsRes
getSettlementDetails a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Settlement.getSettlementDetails a3 a2 a1

listChargebacks ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe API.ChargebackStatusFilter ->
  Environment.FlowHandler API.ChargebackListRes
listChargebacks a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Settlement.listChargebacks a7 a6 a5 a4 a3 a2 a1

respondToChargeback ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  API.ChargebackRespondReq ->
  Environment.FlowHandler API.ChargebackRespondRes
respondToChargeback a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Settlement.respondToChargeback a4 a3 a2 a1

getSettlementTrend ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe API.TrendGranularity ->
  Environment.FlowHandler API.SettlementTrendRes
getSettlementTrend a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Settlement.getSettlementTrend a6 a5 a4 a3 a2 a1
