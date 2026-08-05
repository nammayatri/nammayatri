module Domain.Action.RiderPlatform.AppManagement.TicketDashboard
  ( ticketDashboardUploadAsset,
    ticketDashboardDeleteAsset,
    ticketDashboardCurrentSeatStatus,
    ticketDashboardSeatManagement,
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified API.Types.Dashboard.AppManagement.TicketDashboard
import AWS.S3 (FileType (..))
import qualified Data.ByteString.Lazy as LBS
import Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding (getDashboardAccessType)
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.MerchantOnboarding
import qualified "rider-app" Domain.Types.MerchantOnboarding as MO
import qualified "rider-app" Domain.Types.TicketPlace
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

ticketDashboardUploadAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileRequest -> Environment.Flow API.Types.Dashboard.AppManagement.TicketDashboard.UploadPublicFileResponse)
ticketDashboardUploadAsset merchantShortId opCity apiTokenInfo ticketPlaceId _requestorId _requestorRole req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (req.fileType `elem` [Image] && req.reqContentType `elem` ["image/png", "image/jpeg"]) $
    throwError $ InvalidRequest "Only .png/.jpeg file supported"
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (addMultipartBoundary "XXX00XXX" . (.ticketDashboardDSL.ticketDashboardUploadAsset)) ticketPlaceId (Just requestorId) (Just requestorRole) req)
  where
    addMultipartBoundary :: LBS.ByteString -> (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> (LBS.ByteString, req) -> res) -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> req -> res
    addMultipartBoundary boundary clientFn ticketPlaceId_ _requestorId _requestorRole reqBody = clientFn ticketPlaceId_ _requestorId _requestorRole (boundary, reqBody)

ticketDashboardDeleteAsset :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> API.Types.Dashboard.AppManagement.TicketDashboard.DeletePublicFileRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
ticketDashboardDeleteAsset merchantShortId opCity apiTokenInfo ticketPlaceId _requestorId _requestorRole req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketDashboardDSL.ticketDashboardDeleteAsset) ticketPlaceId (Just requestorId) (Just requestorRole) req)

ticketDashboardCurrentSeatStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusReq -> Environment.Flow API.Types.Dashboard.AppManagement.TicketDashboard.CurrentSeatStatusResp)
ticketDashboardCurrentSeatStatus merchantShortId opCity apiTokenInfo ticketPlaceId _requestorId _requestorRole req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketDashboardDSL.ticketDashboardCurrentSeatStatus) ticketPlaceId (Just requestorId) (Just requestorRole) req

ticketDashboardSeatManagement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> API.Types.Dashboard.AppManagement.TicketDashboard.SeatManagementReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
ticketDashboardSeatManagement merchantShortId opCity apiTokenInfo ticketPlaceId _requestorId _requestorRole req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketDashboardDSL.ticketDashboardSeatManagement) ticketPlaceId (Just requestorId) (Just requestorRole) req
