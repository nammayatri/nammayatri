{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}

module RiderPlatformClient.RiderApp.Operations
  ( callRiderAppOperations,
  )
where

import qualified "rider-app" API.Dashboard as BAP
import qualified "rider-app" API.Types.UI.TicketService as DTB
import qualified Beckn.Types.Core.Taxi.Search ()
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Time
import qualified "rider-app" Domain.Action.Dashboard.IssueList as DI
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import qualified "rider-app" Domain.Types.TicketBooking as DTB
import qualified "rider-app" Domain.Types.TicketBookingService as DTB
import qualified "rider-app" Domain.Types.TicketPlace as DTB
import qualified "rider-app" Domain.Types.TicketService as DTB
import qualified EulerHS.Types as Euler
import qualified IssueManagement.Common as DIssue
import IssueManagement.Common.Dashboard.Issue as Issue
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.Issue.IssueReport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common hiding (callAPI)
import Servant hiding (route)
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

data AppBackendAPIs = AppBackendAPIs
  { issues :: ListIssueAPIs,
    issuesV2 :: IssueAPIs,
    tickets :: TicketAPIs,
    hotSpot :: HotSpotAPIs
  }

data ListIssueAPIs = ListIssueAPIs
  { listIssue :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Euler.EulerClient DI.IssueListRes,
    ticketStatusCallBack :: A.Value -> Euler.EulerClient APISuccess
  }

data IssueAPIs = IssueAPIs
  { issueCategoryList :: Euler.EulerClient Issue.IssueCategoryListRes,
    issueList :: Maybe Int -> Maybe Int -> Maybe DIssue.IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> Euler.EulerClient Issue.IssueReportListResponse,
    issueInfo :: Id IssueReport -> Euler.EulerClient Issue.IssueInfoRes,
    issueInfoV2 :: Maybe (Id IssueReport) -> Maybe (ShortId IssueReport) -> Euler.EulerClient Issue.IssueInfoRes,
    issueUpdate :: Id IssueReport -> Issue.IssueUpdateByUserReq -> Euler.EulerClient APISuccess,
    issueAddComment :: Id IssueReport -> Issue.IssueAddCommentByUserReq -> Euler.EulerClient APISuccess,
    issueFetchMedia :: Text -> Euler.EulerClient Text,
    ticketStatusCallBack_ :: A.Value -> Euler.EulerClient APISuccess,
    createIssueCategory :: Issue.CreateIssueCategoryReq -> Euler.EulerClient Issue.CreateIssueCategoryRes,
    updateIssueCategory :: Id IssueCategory -> Issue.UpdateIssueCategoryReq -> Euler.EulerClient APISuccess,
    createIssueOption :: Id IssueCategory -> Id IssueMessage -> Issue.CreateIssueOptionReq -> Euler.EulerClient Issue.CreateIssueOptionRes,
    updateIssueOption :: Id IssueOption -> Issue.UpdateIssueOptionReq -> Euler.EulerClient APISuccess,
    upsertIssueMessage :: (LBS.ByteString, Issue.UpsertIssueMessageReq) -> Euler.EulerClient Issue.UpsertIssueMessageRes
  }

data TicketAPIs = TicketAPIs
  { verifyBookingDetails :: Id DTB.TicketService -> ShortId DTB.TicketBookingService -> Euler.EulerClient DTB.TicketServiceVerificationResp,
    getServices :: Id DTB.TicketPlace -> Maybe Day -> Euler.EulerClient [DTB.TicketServiceResp],
    updateSeatManagement :: DTB.TicketBookingUpdateSeatsReq -> Euler.EulerClient APISuccess,
    getTicketPlaces :: Euler.EulerClient [DTB.TicketPlace],
    cancelTicketBookingService :: DTB.TicketBookingCancelReq -> Euler.EulerClient APISuccess,
    cancelTicketService :: DTB.TicketServiceCancelReq -> Euler.EulerClient APISuccess,
    getTicketBookingDetails :: Id.ShortId DTB.TicketBooking -> Euler.EulerClient DTB.TicketBookingDetails
  }

newtype HotSpotAPIs = HotSpotAPIs
  { removeExpires :: Euler.EulerClient APISuccess
  }

mkAppBackendAPIs :: CheckedShortId DM.Merchant -> City.City -> Text -> AppBackendAPIs
mkAppBackendAPIs merchantId city token = do
  let issues = ListIssueAPIs {..}
  let issuesV2 = IssueAPIs {..}
  let tickets = TicketAPIs {..}
  let hotSpot = HotSpotAPIs {..}

  AppBackendAPIs {..}
  where
    issueClient
      :<|> issueV2Client
      :<|> ticketsClient
      :<|> hotSpotClient =
        clientWithMerchantAndCity (Proxy :: Proxy BAP.OperationsAPI) merchantId city token

    listIssue
      :<|> ticketStatusCallBack = issueClient

    issueCategoryList
      :<|> issueList
      :<|> issueInfo
      :<|> issueInfoV2
      :<|> issueUpdate
      :<|> issueAddComment
      :<|> issueFetchMedia
      :<|> ticketStatusCallBack_
      :<|> createIssueCategory
      :<|> updateIssueCategory
      :<|> createIssueOption
      :<|> updateIssueOption
      :<|> upsertIssueMessage = issueV2Client

    verifyBookingDetails
      :<|> getServices
      :<|> updateSeatManagement
      :<|> getTicketPlaces
      :<|> cancelTicketBookingService
      :<|> cancelTicketService
      :<|> getTicketBookingDetails = ticketsClient

    removeExpires = hotSpotClient

callRiderAppOperations ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI AppBackendAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  City.City ->
  (AppBackendAPIs -> b) ->
  c
callRiderAppOperations merchantId city = callServerAPI @_ @m @r APP_BACKEND_MANAGEMENT (mkAppBackendAPIs merchantId city) "callRiderAppOperations"
