{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Driver
  ( API.Types.ProviderPlatform.Management.Driver.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Driver
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Data.Time.Calendar
import qualified Domain.Action.Dashboard.Management.Driver
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Driver.API)
handler merchantId city = getDriverDocumentsInfo merchantId city :<|> postDriverPersonNumbers merchantId city :<|> postDriverUpdateTagBulk merchantId city :<|> postDriverDriverDataDecryption merchantId city :<|> postDriverPersonId merchantId city :<|> getDriverAadhaarInfo merchantId city :<|> getDriverAadhaarInfobyMobileNumber merchantId city :<|> getDriverList merchantId city :<|> getDriverActivity merchantId city :<|> postDriverDisable merchantId city :<|> postDriverAcRestrictionUpdate merchantId city :<|> postDriverBlockWithReason merchantId city :<|> postDriverBlock merchantId city :<|> getDriverBlockReasonList merchantId city :<|> postDriverUnblock merchantId city :<|> getDriverLocation merchantId city :<|> deleteDriverPermanentlyDelete merchantId city :<|> postDriverUnlinkDL merchantId city :<|> postDriverUnlinkAadhaar merchantId city :<|> postDriverUpdatePhoneNumber merchantId city :<|> postDriverUpdateByPhoneNumber merchantId city :<|> postDriverUpdateName merchantId city :<|> postDriverDeleteRC merchantId city :<|> getDriverClearStuckOnRide merchantId city :<|> postDriverSendDummyNotification merchantId city :<|> postDriverChangeOperatingCity merchantId city :<|> getDriverGetOperatingCity merchantId city :<|> postDriverPauseOrResumeServiceCharges merchantId city :<|> postDriverUpdateRCInvalidStatus merchantId city :<|> postDriverUpdateRCInvalidStatusByRCNumber merchantId city :<|> postDriverUpdateVehicleVariant merchantId city :<|> postDriverBulkReviewRCVariant merchantId city :<|> postDriverUpdateDriverTag merchantId city :<|> postDriverClearFee merchantId city :<|> getDriverPanAadharSelfieDetails merchantId city :<|> postDriverSyncDocAadharPan merchantId city :<|> postDriverUpdateVehicleManufacturing merchantId city :<|> postDriverVehicleAppendSelectedServiceTiers merchantId city :<|> postDriverVehicleUpsertSelectedServiceTiers merchantId city :<|> postDriverRefundByPayout merchantId city :<|> getDriverSecurityDepositStatus merchantId city :<|> getDriverPanAadharSelfieDetailsList merchantId city :<|> postDriverBulkSubscriptionServiceUpdate merchantId city :<|> getDriverStats merchantId city :<|> getDriverEarnings merchantId city :<|> postDriverTdsRateUpdate merchantId city :<|> postDriverUpdateMerchant merchantId city

getDriverDocumentsInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler Dashboard.Common.Driver.DriverDocumentsInfoRes)
getDriverDocumentsInfo a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverDocumentsInfo a2 a1

postDriverPersonNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.PersonIdsReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postDriverPersonNumbers a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverPersonNumbers a3 a2 a1

postDriverUpdateTagBulk :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.UpdateTagBulkReq -> Environment.FlowHandler [Dashboard.Common.UpdateTagBulkRes])
postDriverUpdateTagBulk a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateTagBulk a3 a2 a1

postDriverDriverDataDecryption :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> [API.Types.ProviderPlatform.Management.Driver.DriverEncDataReq] -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.DriverDecDataResp])
postDriverDriverDataDecryption a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverDriverDataDecryption a3 a2 a1

postDriverPersonId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.PersonMobileNoReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postDriverPersonId a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverPersonId a3 a2 a1

getDriverAadhaarInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoRes)
getDriverAadhaarInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverAadhaarInfo a3 a2 a1

getDriverAadhaarInfobyMobileNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoByPhoneReq)
getDriverAadhaarInfobyMobileNumber a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverAadhaarInfobyMobileNumber a3 a2 a1

getDriverList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverListRes)
getDriverList a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverList a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverActivity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler Dashboard.Common.Driver.DriverActivityRes)
getDriverActivity a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverActivity a2 a1

postDriverDisable :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDisable a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverDisable a3 a2 a1

postDriverAcRestrictionUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateACUsageRestrictionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAcRestrictionUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverAcRestrictionUpdate a4 a3 a2 a1

postDriverBlockWithReason :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.BlockDriverWithReasonReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBlockWithReason a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBlockWithReason a5 a4 a3 a2 a1

postDriverBlock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBlock a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBlock a3 a2 a1

getDriverBlockReasonList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.BlockReason])
getDriverBlockReasonList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverBlockReasonList a2 a1

postDriverUnblock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnblock a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUnblock a6 a5 a4 a3 a2 a1

getDriverLocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Dashboard.Common.Driver.DriverIds -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverLocationRes)
getDriverLocation a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverLocation a5 a4 a3 a2 a1

deleteDriverPermanentlyDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteDriverPermanentlyDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.deleteDriverPermanentlyDelete a3 a2 a1

postDriverUnlinkDL :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkDL a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUnlinkDL a3 a2 a1

postDriverUnlinkAadhaar :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkAadhaar a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUnlinkAadhaar a3 a2 a1

postDriverUpdatePhoneNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdatePhoneNumberReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdatePhoneNumber a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdatePhoneNumber a4 a3 a2 a1

postDriverUpdateByPhoneNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverDataReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateByPhoneNumber a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateByPhoneNumber a4 a3 a2 a1

postDriverUpdateName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverNameReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateName a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateName a4 a3 a2 a1

postDriverDeleteRC :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.DeleteRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDeleteRC a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverDeleteRC a4 a3 a2 a1

getDriverClearStuckOnRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.ClearOnRideStuckDriversRes)
getDriverClearStuckOnRide a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverClearStuckOnRide a3 a2 a1

postDriverSendDummyNotification :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSendDummyNotification a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverSendDummyNotification a3 a2 a1

postDriverChangeOperatingCity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ChangeOperatingCityReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverChangeOperatingCity a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverChangeOperatingCity a4 a3 a2 a1

getDriverGetOperatingCity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.GetOperatingCityResp)
getDriverGetOperatingCity a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverGetOperatingCity a5 a4 a3 a2 a1

postDriverPauseOrResumeServiceCharges :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.PauseOrResumeServiceChargesReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverPauseOrResumeServiceCharges a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverPauseOrResumeServiceCharges a4 a3 a2 a1

postDriverUpdateRCInvalidStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateRCInvalidStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateRCInvalidStatus a4 a3 a2 a1

postDriverUpdateRCInvalidStatusByRCNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusByRCNumberReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateRCInvalidStatusByRCNumber a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateRCInvalidStatusByRCNumber a3 a2 a1

postDriverUpdateVehicleVariant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateVehicleVariantReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateVehicleVariant a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateVehicleVariant a4 a3 a2 a1

postDriverBulkReviewRCVariant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantReq] -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantRes])
postDriverBulkReviewRCVariant a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBulkReviewRCVariant a3 a2 a1

postDriverUpdateDriverTag :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverTagReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateDriverTag a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateDriverTag a4 a3 a2 a1

postDriverClearFee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverClearFee a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverClearFee a4 a3 a2 a1

getDriverPanAadharSelfieDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsResp)
getDriverPanAadharSelfieDetails a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverPanAadharSelfieDetails a4 a3 a2 a1

postDriverSyncDocAadharPan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Driver.AadharPanSyncReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSyncDocAadharPan a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverSyncDocAadharPan a3 a2 a1

postDriverUpdateVehicleManufacturing :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateVehicleManufacturingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateVehicleManufacturing a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateVehicleManufacturing a4 a3 a2 a1

postDriverVehicleAppendSelectedServiceTiers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.AppendSelectedServiceTiersReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverVehicleAppendSelectedServiceTiers a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverVehicleAppendSelectedServiceTiers a4 a3 a2 a1

postDriverVehicleUpsertSelectedServiceTiers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Driver.UpsertDriverServiceTiersCsvReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverVehicleUpsertSelectedServiceTiers a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverVehicleUpsertSelectedServiceTiers a3 a2 a1

postDriverRefundByPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.RefundByPayoutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRefundByPayout a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverRefundByPayout a4 a3 a2 a1

getDriverSecurityDepositStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Dashboard.Common.Driver.ServiceNames -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.SecurityDepositDfStatusRes])
getDriverSecurityDepositStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverSecurityDepositStatus a4 a3 a2 a1

getDriverPanAadharSelfieDetailsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsListResp])
getDriverPanAadharSelfieDetailsList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverPanAadharSelfieDetailsList a4 a3 a2 a1

postDriverBulkSubscriptionServiceUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Driver.BulkServiceUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBulkSubscriptionServiceUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBulkSubscriptionServiceUpdate a3 a2 a1

getDriverStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverStatsRes)
getDriverStats a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverStats a6 a5 a4 a3 a2 a1

getDriverEarnings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Time.Calendar.Day -> Data.Time.Calendar.Day -> Dashboard.Common.Driver.EarningType -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.EarningPeriodStatsRes)
getDriverEarnings a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverEarnings a7 a6 a5 a4 a3 a2 a1

postDriverTdsRateUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Driver.UpdateTdsRateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverTdsRateUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverTdsRateUpdate a3 a2 a1

postDriverUpdateMerchant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverMerchantReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateMerchant a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateMerchant a4 a3 a2 a1
