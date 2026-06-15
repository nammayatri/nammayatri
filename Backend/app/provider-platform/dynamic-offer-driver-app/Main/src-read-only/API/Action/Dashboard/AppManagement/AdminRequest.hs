{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.AdminRequest
  ( API.Types.Dashboard.AppManagement.AdminRequest.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.AdminRequest
import qualified Domain.Action.Dashboard.AppManagement.AdminRequest
import qualified "this" Domain.Types.AdminRequest
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.AdminRequest.API)
handler merchantId city = postAdminRequestCreate merchantId city :<|> getAdminRequestList merchantId city :<|> postAdminRequestRespond merchantId city

postAdminRequestCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.AdminRequest.CreateAdminRequestReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAdminRequestCreate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.AdminRequest.postAdminRequestCreate a5 a4 a3 a2 a1

getAdminRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest) -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdminRequestStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.ActionType -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.ReferenceTable -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentSource -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.AdminRequest.AdminRequestResp)
getAdminRequestList a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.AdminRequest.getAdminRequestList a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postAdminRequestRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.AdminRequest.RespondAdminRequestReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAdminRequestRespond a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.AdminRequest.postAdminRequestRespond a6 a5 a4 a3 a2 a1
