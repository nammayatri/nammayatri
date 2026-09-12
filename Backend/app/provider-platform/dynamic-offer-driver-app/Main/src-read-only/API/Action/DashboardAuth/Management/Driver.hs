{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Driver
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Data.Time
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
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

type API = ("driver" :> (GetDriverDocumentsInfo :<|> PostDriverPersonNumbers :<|> PostDriverUpdateTagBulk :<|> PostDriverDriverDataDecryption :<|> PostDriverPersonId :<|> GetDriverAadhaarInfo :<|> GetDriverAadhaarInfobyMobileNumber :<|> GetDriverLoginOtp :<|> GetDriverList :<|> GetDriverActivity :<|> PostDriverDisable :<|> PostDriverAcRestrictionUpdate :<|> PostDriverBlockWithReason :<|> PostDriverBlock :<|> GetDriverBlockReasonList :<|> PostDriverUnblock :<|> GetDriverLocation :<|> DeleteDriverPermanentlyDelete :<|> PostDriverUnlinkDL :<|> PostDriverUnlinkAadhaar :<|> PostDriverUpdatePhoneNumber :<|> PostDriverUpdateByPhoneNumber :<|> PostDriverUpdateName :<|> PostDriverDeleteRC :<|> GetDriverClearStuckOnRide :<|> PostDriverSendDummyNotification :<|> PostDriverChangeOperatingCity :<|> GetDriverGetOperatingCity :<|> PostDriverPauseOrResumeServiceCharges :<|> PostDriverUpdateRCInvalidStatus :<|> PostDriverUpdateRCInvalidStatusByRCNumber :<|> PostDriverUpdateVehicleVariant :<|> PostDriverBulkReviewRCVariant :<|> PostDriverUpdateDriverTag :<|> PostDriverUpdateSpecialLocWarrior :<|> PostDriverClearFee :<|> GetDriverPanAadharSelfieDetails :<|> PostDriverSyncDocAadharPan :<|> PostDriverUpdateVehicleManufacturing :<|> PostDriverVehicleAppendSelectedServiceTiers :<|> PostDriverVehicleUpsertSelectedServiceTiers :<|> PostDriverRefundByPayout :<|> GetDriverSecurityDepositStatus :<|> GetDriverPanAadharSelfieDetailsList :<|> PostDriverBulkSubscriptionServiceUpdate :<|> GetDriverStats :<|> GetDriverEarnings :<|> PostDriverTdsRateUpdate :<|> PostDriverUpdateMerchant :<|> GetDriverAirportPreference :<|> PostDriverAirportPreference :<|> GetDriverSearchRequestStats :<|> GetDriverIdentityInfo :<|> PostDriverIdentityInfoUpdate :<|> PostDriverAssociationChange))

type GetDriverDocumentsInfo =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_DOCUMENTS_INFO"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverDocumentsInfo
  )

type PostDriverPersonNumbers =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PERSON_NUMBERS"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverPersonNumbers
  )

type PostDriverUpdateTagBulk =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_TAG_BULK"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateTagBulk
  )

type PostDriverDriverDataDecryption =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DRIVER_DATA_DECRYPTION"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverDriverDataDecryption
  )

type PostDriverPersonId = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PERSON_ID" :> API.Types.ProviderPlatform.Management.Driver.PostDriverPersonId)

type GetDriverAadhaarInfo = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_AADHAAR_INFO" :> API.Types.ProviderPlatform.Management.Driver.GetDriverAadhaarInfo)

type GetDriverAadhaarInfobyMobileNumber =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_AADHAAR_INFOBY_MOBILE_NUMBER"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverAadhaarInfobyMobileNumber
  )

type GetDriverLoginOtp =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LOGIN_OTP"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverLoginOtp
  )

type GetDriverList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LIST" :> API.Types.ProviderPlatform.Management.Driver.GetDriverList)

type GetDriverActivity = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_ACTIVITY" :> API.Types.ProviderPlatform.Management.Driver.GetDriverActivity)

type PostDriverDisable = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DISABLE" :> API.Types.ProviderPlatform.Management.Driver.PostDriverDisable)

type PostDriverAcRestrictionUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_AC_RESTRICTION_UPDATE"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverAcRestrictionUpdate
  )

type PostDriverBlockWithReason =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK_WITH_REASON"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverBlockWithReason
  )

type PostDriverBlock = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK" :> API.Types.ProviderPlatform.Management.Driver.PostDriverBlock)

type GetDriverBlockReasonList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_BLOCK_REASON_LIST"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverBlockReasonList
  )

type PostDriverUnblock =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNBLOCK"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUnblock
  )

type GetDriverLocation = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LOCATION" :> API.Types.ProviderPlatform.Management.Driver.GetDriverLocation)

type DeleteDriverPermanentlyDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/DELETE_DRIVER_PERMANENTLY_DELETE"
      :> API.Types.ProviderPlatform.Management.Driver.DeleteDriverPermanentlyDelete
  )

type PostDriverUnlinkDL = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNLINK_DL" :> API.Types.ProviderPlatform.Management.Driver.PostDriverUnlinkDL)

type PostDriverUnlinkAadhaar =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNLINK_AADHAAR"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUnlinkAadhaar
  )

type PostDriverUpdatePhoneNumber =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_PHONE_NUMBER"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdatePhoneNumber
  )

type PostDriverUpdateByPhoneNumber =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_BY_PHONE_NUMBER"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateByPhoneNumber
  )

type PostDriverUpdateName = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_NAME" :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateName)

type PostDriverDeleteRC = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DELETE_RC" :> API.Types.ProviderPlatform.Management.Driver.PostDriverDeleteRC)

type GetDriverClearStuckOnRide =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_CLEAR_STUCK_ON_RIDE"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverClearStuckOnRide
  )

type PostDriverSendDummyNotification =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_SEND_DUMMY_NOTIFICATION"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverSendDummyNotification
  )

type PostDriverChangeOperatingCity =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_CHANGE_OPERATING_CITY"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverChangeOperatingCity
  )

type GetDriverGetOperatingCity =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_GET_OPERATING_CITY"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverGetOperatingCity
  )

type PostDriverPauseOrResumeServiceCharges =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PAUSE_OR_RESUME_SERVICE_CHARGES"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverPauseOrResumeServiceCharges
  )

type PostDriverUpdateRCInvalidStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_RC_INVALID_STATUS"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateRCInvalidStatus
  )

type PostDriverUpdateRCInvalidStatusByRCNumber =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_RC_INVALID_STATUS_BY_RC_NUMBER"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateRCInvalidStatusByRCNumber
  )

type PostDriverUpdateVehicleVariant =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_VEHICLE_VARIANT"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateVehicleVariant
  )

type PostDriverBulkReviewRCVariant =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BULK_REVIEW_RC_VARIANT"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverBulkReviewRCVariant
  )

type PostDriverUpdateDriverTag =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_DRIVER_TAG"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateDriverTag
  )

type PostDriverUpdateSpecialLocWarrior =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_SPECIAL_LOC_WARRIOR"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateSpecialLocWarrior
  )

type PostDriverClearFee =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_CLEAR_FEE"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverClearFee
  )

type GetDriverPanAadharSelfieDetails =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverPanAadharSelfieDetails
  )

type PostDriverSyncDocAadharPan =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_SYNC_DOC_AADHAR_PAN"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverSyncDocAadharPan
  )

type PostDriverUpdateVehicleManufacturing =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_VEHICLE_MANUFACTURING"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateVehicleManufacturing
  )

type PostDriverVehicleAppendSelectedServiceTiers =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_VEHICLE_APPEND_SELECTED_SERVICE_TIERS"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverVehicleAppendSelectedServiceTiers
  )

type PostDriverVehicleUpsertSelectedServiceTiers =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_VEHICLE_UPSERT_SELECTED_SERVICE_TIERS"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverVehicleUpsertSelectedServiceTiers
  )

type PostDriverRefundByPayout =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_REFUND_BY_PAYOUT"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverRefundByPayout
  )

type GetDriverSecurityDepositStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_SECURITY_DEPOSIT_STATUS"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverSecurityDepositStatus
  )

type GetDriverPanAadharSelfieDetailsList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS_LIST"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverPanAadharSelfieDetailsList
  )

type PostDriverBulkSubscriptionServiceUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BULK_SUBSCRIPTION_SERVICE_UPDATE"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverBulkSubscriptionServiceUpdate
  )

type GetDriverStats = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_STATS" :> API.Types.ProviderPlatform.Management.Driver.GetDriverStats)

type GetDriverEarnings =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_EARNINGS"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverEarnings
  )

type PostDriverTdsRateUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_TDS_RATE_UPDATE"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverTdsRateUpdate
  )

type PostDriverUpdateMerchant =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_MERCHANT"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateMerchant
  )

type GetDriverAirportPreference =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_AIRPORT_PREFERENCE"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverAirportPreference
  )

type PostDriverAirportPreference =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_AIRPORT_PREFERENCE"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverAirportPreference
  )

type GetDriverSearchRequestStats =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_SEARCH_REQUEST_STATS"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverSearchRequestStats
  )

type GetDriverIdentityInfo =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_IDENTITY_INFO"
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverIdentityInfo
  )

type PostDriverIdentityInfoUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_IDENTITY_INFO_UPDATE"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverIdentityInfoUpdate
  )

type PostDriverAssociationChange =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_ASSOCIATION_CHANGE"
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverAssociationChange
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverDocumentsInfo merchantId city :<|> postDriverPersonNumbers merchantId city :<|> postDriverUpdateTagBulk merchantId city :<|> postDriverDriverDataDecryption merchantId city :<|> postDriverPersonId merchantId city :<|> getDriverAadhaarInfo merchantId city :<|> getDriverAadhaarInfobyMobileNumber merchantId city :<|> getDriverLoginOtp merchantId city :<|> getDriverList merchantId city :<|> getDriverActivity merchantId city :<|> postDriverDisable merchantId city :<|> postDriverAcRestrictionUpdate merchantId city :<|> postDriverBlockWithReason merchantId city :<|> postDriverBlock merchantId city :<|> getDriverBlockReasonList merchantId city :<|> postDriverUnblock merchantId city :<|> getDriverLocation merchantId city :<|> deleteDriverPermanentlyDelete merchantId city :<|> postDriverUnlinkDL merchantId city :<|> postDriverUnlinkAadhaar merchantId city :<|> postDriverUpdatePhoneNumber merchantId city :<|> postDriverUpdateByPhoneNumber merchantId city :<|> postDriverUpdateName merchantId city :<|> postDriverDeleteRC merchantId city :<|> getDriverClearStuckOnRide merchantId city :<|> postDriverSendDummyNotification merchantId city :<|> postDriverChangeOperatingCity merchantId city :<|> getDriverGetOperatingCity merchantId city :<|> postDriverPauseOrResumeServiceCharges merchantId city :<|> postDriverUpdateRCInvalidStatus merchantId city :<|> postDriverUpdateRCInvalidStatusByRCNumber merchantId city :<|> postDriverUpdateVehicleVariant merchantId city :<|> postDriverBulkReviewRCVariant merchantId city :<|> postDriverUpdateDriverTag merchantId city :<|> postDriverUpdateSpecialLocWarrior merchantId city :<|> postDriverClearFee merchantId city :<|> getDriverPanAadharSelfieDetails merchantId city :<|> postDriverSyncDocAadharPan merchantId city :<|> postDriverUpdateVehicleManufacturing merchantId city :<|> postDriverVehicleAppendSelectedServiceTiers merchantId city :<|> postDriverVehicleUpsertSelectedServiceTiers merchantId city :<|> postDriverRefundByPayout merchantId city :<|> getDriverSecurityDepositStatus merchantId city :<|> getDriverPanAadharSelfieDetailsList merchantId city :<|> postDriverBulkSubscriptionServiceUpdate merchantId city :<|> getDriverStats merchantId city :<|> getDriverEarnings merchantId city :<|> postDriverTdsRateUpdate merchantId city :<|> postDriverUpdateMerchant merchantId city :<|> getDriverAirportPreference merchantId city :<|> postDriverAirportPreference merchantId city :<|> getDriverSearchRequestStats merchantId city :<|> getDriverIdentityInfo merchantId city :<|> postDriverIdentityInfoUpdate merchantId city :<|> postDriverAssociationChange merchantId city

getDriverDocumentsInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler Dashboard.Common.Driver.DriverDocumentsInfoRes)
getDriverDocumentsInfo a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverDocumentsInfo a3 a2

postDriverPersonNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.PersonIdsReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postDriverPersonNumbers a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverPersonNumbers a4 a3 a1

postDriverUpdateTagBulk :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.UpdateTagBulkReq -> Environment.FlowHandler [Dashboard.Common.UpdateTagBulkRes])
postDriverUpdateTagBulk a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateTagBulk a4 a3 a1

postDriverDriverDataDecryption :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> [API.Types.ProviderPlatform.Management.Driver.DriverEncDataReq] -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.DriverDecDataResp])
postDriverDriverDataDecryption a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverDriverDataDecryption a4 a3 a1

postDriverPersonId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.PersonMobileNoReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postDriverPersonId a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverPersonId a4 a3 a1

getDriverAadhaarInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoRes)
getDriverAadhaarInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverAadhaarInfo a4 a3 a1

getDriverAadhaarInfobyMobileNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoByPhoneReq)
getDriverAadhaarInfobyMobileNumber a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverAadhaarInfobyMobileNumber a4 a3 a1

getDriverList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.ApprovalStatusFilter) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.OnboardingAs) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverListRes)
getDriverList a18 a17 _a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverList a18 a17 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverActivity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler Dashboard.Common.Driver.DriverActivityRes)
getDriverActivity a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverActivity a3 a2

postDriverDisable :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDisable a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverDisable a4 a3 a1

postDriverAcRestrictionUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateACUsageRestrictionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAcRestrictionUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverAcRestrictionUpdate a5 a4 a2 a1

postDriverBlockWithReason :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.BlockDriverWithReasonReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBlockWithReason a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBlockWithReason a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorName a3) a1

postDriverBlock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBlock a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBlock a4 a3 a1

getDriverBlockReasonList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.BlockReason])
getDriverBlockReasonList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverBlockReasonList a3 a2

postDriverUnblock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnblock a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUnblock a6 a5 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorName a4) a2 a1

getDriverLocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Dashboard.Common.Driver.DriverIds -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverLocationRes)
getDriverLocation a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverLocation a6 a5 a3 a2 a1

deleteDriverPermanentlyDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteDriverPermanentlyDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ do
  res <- Domain.Action.Dashboard.Management.Driver.deleteDriverPermanentlyDelete a4 a3 a1
  Tools.Auth.DashboardRegistration.deleteDashboardPerson a1.getId
  Kernel.Prelude.pure res

postDriverUnlinkDL :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkDL a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUnlinkDL a4 a3 a1

postDriverUnlinkAadhaar :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkAadhaar a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUnlinkAadhaar a4 a3 a1

postDriverUpdatePhoneNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdatePhoneNumberReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdatePhoneNumber a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdatePhoneNumber a5 a4 a2 a1

postDriverUpdateByPhoneNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverDataReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateByPhoneNumber a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateByPhoneNumber a5 a4 a2 a1

postDriverUpdateName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverNameReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateName a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateName a5 a4 a2 a1

postDriverDeleteRC :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.DeleteRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDeleteRC a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverDeleteRC a5 a4 a2 a1

getDriverClearStuckOnRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.ClearOnRideStuckDriversRes)
getDriverClearStuckOnRide a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverClearStuckOnRide a4 a3 a1

postDriverSendDummyNotification :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DummyRideRequestRes)
postDriverSendDummyNotification a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverSendDummyNotification a4 a3 a1

postDriverChangeOperatingCity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ChangeOperatingCityReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverChangeOperatingCity a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverChangeOperatingCity a5 a4 a2 a1

getDriverGetOperatingCity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.GetOperatingCityResp)
getDriverGetOperatingCity a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverGetOperatingCity a6 a5 a3 a2 a1

postDriverPauseOrResumeServiceCharges :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.PauseOrResumeServiceChargesReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverPauseOrResumeServiceCharges a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverPauseOrResumeServiceCharges a5 a4 a2 a1

postDriverUpdateRCInvalidStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateRCInvalidStatus a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateRCInvalidStatus a5 a4 a2 a1

postDriverUpdateRCInvalidStatusByRCNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusByRCNumberReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateRCInvalidStatusByRCNumber a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateRCInvalidStatusByRCNumber a4 a3 a1

postDriverUpdateVehicleVariant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateVehicleVariantReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateVehicleVariant a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateVehicleVariant a5 a4 a2 a1

postDriverBulkReviewRCVariant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantReq] -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantRes])
postDriverBulkReviewRCVariant a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBulkReviewRCVariant a4 a3 a1

postDriverUpdateDriverTag :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverTagReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateDriverTag a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateDriverTag a5 a4 a2 a1

postDriverUpdateSpecialLocWarrior :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverSpecialLocWarriorReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateSpecialLocWarrior a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateSpecialLocWarrior a5 a4 a2 a1

postDriverClearFee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverClearFee a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverClearFee a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1

getDriverPanAadharSelfieDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsResp)
getDriverPanAadharSelfieDetails a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverPanAadharSelfieDetails a5 a4 a2 a1

postDriverSyncDocAadharPan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Driver.AadharPanSyncReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSyncDocAadharPan a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverSyncDocAadharPan a4 a3 a1

postDriverUpdateVehicleManufacturing :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateVehicleManufacturingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateVehicleManufacturing a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateVehicleManufacturing a5 a4 a2 a1

postDriverVehicleAppendSelectedServiceTiers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.AppendSelectedServiceTiersReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverVehicleAppendSelectedServiceTiers a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverVehicleAppendSelectedServiceTiers a5 a4 a2 a1

postDriverVehicleUpsertSelectedServiceTiers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Driver.UpsertDriverServiceTiersCsvReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverVehicleUpsertSelectedServiceTiers a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverVehicleUpsertSelectedServiceTiers a4 a3 a1

postDriverRefundByPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.RefundByPayoutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRefundByPayout a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverRefundByPayout a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1

getDriverSecurityDepositStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.ServiceNames) -> Environment.FlowHandler ([API.Types.ProviderPlatform.Management.Driver.SecurityDepositDfStatusRes]))
getDriverSecurityDepositStatus a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverSecurityDepositStatus a5 a4 a2 a1

getDriverPanAadharSelfieDetailsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler ([API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsListResp]))
getDriverPanAadharSelfieDetailsList a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverPanAadharSelfieDetailsList a5 a4 a2 a1

postDriverBulkSubscriptionServiceUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Driver.BulkServiceUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBulkSubscriptionServiceUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBulkSubscriptionServiceUpdate a4 a3 a1

getDriverStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverStatsRes)
getDriverStats a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverStats a6 a5 a3 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)

getDriverEarnings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Time.Day -> Data.Time.Day -> Dashboard.Common.Driver.EarningType -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.EarningPeriodStatsRes)
getDriverEarnings a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverEarnings a7 a6 a4 a3 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)

postDriverTdsRateUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Driver.UpdateTdsRateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverTdsRateUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverTdsRateUpdate a4 a3 a1

postDriverUpdateMerchant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverMerchantReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateMerchant a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateMerchant a5 a4 a2 a1

getDriverAirportPreference :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.AirportPreferenceRes)
getDriverAirportPreference a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverAirportPreference a6 a5 a3 a2 a1

postDriverAirportPreference :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.AirportPreferenceReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAirportPreference a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverAirportPreference a5 a4 a2 a1

getDriverSearchRequestStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverSearchRequestStatsRes)
getDriverSearchRequestStats a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverSearchRequestStats a6 a5 a3 a2 a1

getDriverIdentityInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverIdentityInfoRes)
getDriverIdentityInfo a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverIdentityInfo a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)

postDriverIdentityInfoUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverIdentityInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverIdentityInfoUpdate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverIdentityInfoUpdate a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1

postDriverAssociationChange :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.ChangeAssociationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAssociationChange a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverAssociationChange a5 a4 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2 a1

getDriverLoginOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverLoginOtpRes)
getDriverLoginOtp a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverLoginOtp a6 a5 a3 a2 a1
