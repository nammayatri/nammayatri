{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Driver
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Data.Time.Calendar
import qualified Domain.Action.ProviderPlatform.Management.Driver
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driver" :> (GetDriverDocumentsInfo :<|> PostDriverPersonNumbers :<|> PostDriverUpdateTagBulk :<|> PostDriverDriverDataDecryption :<|> PostDriverPersonId :<|> GetDriverAadhaarInfo :<|> GetDriverAadhaarInfobyMobileNumber :<|> GetDriverList :<|> GetDriverActivity :<|> PostDriverDisable :<|> PostDriverAcRestrictionUpdate :<|> PostDriverBlockWithReason :<|> PostDriverBlock :<|> GetDriverBlockReasonList :<|> PostDriverUnblock :<|> GetDriverLocation :<|> DeleteDriverPermanentlyDelete :<|> PostDriverUnlinkDL :<|> PostDriverUnlinkAadhaar :<|> PostDriverUpdatePhoneNumber :<|> PostDriverUpdateByPhoneNumber :<|> PostDriverUpdateName :<|> PostDriverDeleteRC :<|> GetDriverClearStuckOnRide :<|> PostDriverSendDummyNotification :<|> PostDriverChangeOperatingCity :<|> GetDriverGetOperatingCity :<|> PostDriverPauseOrResumeServiceCharges :<|> PostDriverUpdateRCInvalidStatus :<|> PostDriverUpdateRCInvalidStatusByRCNumber :<|> PostDriverUpdateVehicleVariant :<|> PostDriverBulkReviewRCVariant :<|> PostDriverUpdateDriverTag :<|> PostDriverClearFee :<|> GetDriverPanAadharSelfieDetails :<|> PostDriverSyncDocAadharPan :<|> PostDriverUpdateVehicleManufacturing :<|> PostDriverVehicleAppendSelectedServiceTiers :<|> PostDriverVehicleUpsertSelectedServiceTiers :<|> PostDriverRefundByPayout :<|> GetDriverSecurityDepositStatus :<|> GetDriverPanAadharSelfieDetailsList :<|> PostDriverBulkSubscriptionServiceUpdate :<|> GetDriverStats :<|> GetDriverEarnings :<|> PostDriverUpdateMerchant))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverDocumentsInfo merchantId city :<|> postDriverPersonNumbers merchantId city :<|> postDriverUpdateTagBulk merchantId city :<|> postDriverDriverDataDecryption merchantId city :<|> postDriverPersonId merchantId city :<|> getDriverAadhaarInfo merchantId city :<|> getDriverAadhaarInfobyMobileNumber merchantId city :<|> getDriverList merchantId city :<|> getDriverActivity merchantId city :<|> postDriverDisable merchantId city :<|> postDriverAcRestrictionUpdate merchantId city :<|> postDriverBlockWithReason merchantId city :<|> postDriverBlock merchantId city :<|> getDriverBlockReasonList merchantId city :<|> postDriverUnblock merchantId city :<|> getDriverLocation merchantId city :<|> deleteDriverPermanentlyDelete merchantId city :<|> postDriverUnlinkDL merchantId city :<|> postDriverUnlinkAadhaar merchantId city :<|> postDriverUpdatePhoneNumber merchantId city :<|> postDriverUpdateByPhoneNumber merchantId city :<|> postDriverUpdateName merchantId city :<|> postDriverDeleteRC merchantId city :<|> getDriverClearStuckOnRide merchantId city :<|> postDriverSendDummyNotification merchantId city :<|> postDriverChangeOperatingCity merchantId city :<|> getDriverGetOperatingCity merchantId city :<|> postDriverPauseOrResumeServiceCharges merchantId city :<|> postDriverUpdateRCInvalidStatus merchantId city :<|> postDriverUpdateRCInvalidStatusByRCNumber merchantId city :<|> postDriverUpdateVehicleVariant merchantId city :<|> postDriverBulkReviewRCVariant merchantId city :<|> postDriverUpdateDriverTag merchantId city :<|> postDriverClearFee merchantId city :<|> getDriverPanAadharSelfieDetails merchantId city :<|> postDriverSyncDocAadharPan merchantId city :<|> postDriverUpdateVehicleManufacturing merchantId city :<|> postDriverVehicleAppendSelectedServiceTiers merchantId city :<|> postDriverVehicleUpsertSelectedServiceTiers merchantId city :<|> postDriverRefundByPayout merchantId city :<|> getDriverSecurityDepositStatus merchantId city :<|> getDriverPanAadharSelfieDetailsList merchantId city :<|> postDriverBulkSubscriptionServiceUpdate merchantId city :<|> getDriverStats merchantId city :<|> getDriverEarnings merchantId city :<|> postDriverUpdateMerchant merchantId city

type GetDriverDocumentsInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_DOCUMENTS_INFO)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverDocumentsInfo
  )

type PostDriverPersonNumbers =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_PERSON_NUMBERS)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverPersonNumbers
  )

type PostDriverUpdateTagBulk =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UPDATE_TAG_BULK)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateTagBulk
  )

type PostDriverDriverDataDecryption =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_DRIVER_DATA_DECRYPTION)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverDriverDataDecryption
  )

type PostDriverPersonId =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_PERSON_ID)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverPersonId
  )

type GetDriverAadhaarInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_AADHAAR_INFO)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverAadhaarInfo
  )

type GetDriverAadhaarInfobyMobileNumber =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_AADHAAR_INFOBY_MOBILE_NUMBER)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverAadhaarInfobyMobileNumber
  )

type GetDriverList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_LIST)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverList
  )

type GetDriverActivity =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_ACTIVITY)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverActivity
  )

type PostDriverDisable =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_DISABLE)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverDisable
  )

type PostDriverAcRestrictionUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_AC_RESTRICTION_UPDATE)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverAcRestrictionUpdate
  )

type PostDriverBlockWithReason =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_BLOCK_WITH_REASON)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverBlockWithReason
  )

type PostDriverBlock =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_BLOCK)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverBlock
  )

type GetDriverBlockReasonList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_BLOCK_REASON_LIST)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverBlockReasonList
  )

type PostDriverUnblock =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UNBLOCK)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUnblock
  )

type GetDriverLocation =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_LOCATION)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverLocation
  )

type DeleteDriverPermanentlyDelete =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.DELETE_DRIVER_PERMANENTLY_DELETE)
      :> API.Types.ProviderPlatform.Management.Driver.DeleteDriverPermanentlyDelete
  )

type PostDriverUnlinkDL =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UNLINK_DL)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUnlinkDL
  )

type PostDriverUnlinkAadhaar =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UNLINK_AADHAAR)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUnlinkAadhaar
  )

type PostDriverUpdatePhoneNumber =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UPDATE_PHONE_NUMBER)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdatePhoneNumber
  )

type PostDriverUpdateByPhoneNumber =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UPDATE_BY_PHONE_NUMBER)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateByPhoneNumber
  )

type PostDriverUpdateName =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UPDATE_NAME)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateName
  )

type PostDriverDeleteRC =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_DELETE_RC)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverDeleteRC
  )

type GetDriverClearStuckOnRide =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_CLEAR_STUCK_ON_RIDE)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverClearStuckOnRide
  )

type PostDriverSendDummyNotification =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_SEND_DUMMY_NOTIFICATION)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverSendDummyNotification
  )

type PostDriverChangeOperatingCity =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_CHANGE_OPERATING_CITY)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverChangeOperatingCity
  )

type GetDriverGetOperatingCity =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_GET_OPERATING_CITY)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverGetOperatingCity
  )

type PostDriverPauseOrResumeServiceCharges =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_PAUSE_OR_RESUME_SERVICE_CHARGES)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverPauseOrResumeServiceCharges
  )

type PostDriverUpdateRCInvalidStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UPDATE_RC_INVALID_STATUS)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateRCInvalidStatus
  )

type PostDriverUpdateRCInvalidStatusByRCNumber =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UPDATE_RC_INVALID_STATUS_BY_RC_NUMBER)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateRCInvalidStatusByRCNumber
  )

type PostDriverUpdateVehicleVariant =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UPDATE_VEHICLE_VARIANT)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateVehicleVariant
  )

type PostDriverBulkReviewRCVariant =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_BULK_REVIEW_RC_VARIANT)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverBulkReviewRCVariant
  )

type PostDriverUpdateDriverTag =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UPDATE_DRIVER_TAG)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateDriverTag
  )

type PostDriverClearFee =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_CLEAR_FEE)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverClearFee
  )

type GetDriverPanAadharSelfieDetails =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverPanAadharSelfieDetails
  )

type PostDriverSyncDocAadharPan =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_SYNC_DOC_AADHAR_PAN)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverSyncDocAadharPan
  )

type PostDriverUpdateVehicleManufacturing =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UPDATE_VEHICLE_MANUFACTURING)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateVehicleManufacturing
  )

type PostDriverVehicleAppendSelectedServiceTiers =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_VEHICLE_APPEND_SELECTED_SERVICE_TIERS)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverVehicleAppendSelectedServiceTiers
  )

type PostDriverVehicleUpsertSelectedServiceTiers =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_VEHICLE_UPSERT_SELECTED_SERVICE_TIERS)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverVehicleUpsertSelectedServiceTiers
  )

type PostDriverRefundByPayout =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_REFUND_BY_PAYOUT)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverRefundByPayout
  )

type GetDriverSecurityDepositStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_SECURITY_DEPOSIT_STATUS)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverSecurityDepositStatus
  )

type GetDriverPanAadharSelfieDetailsList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS_LIST)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverPanAadharSelfieDetailsList
  )

type PostDriverBulkSubscriptionServiceUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_BULK_SUBSCRIPTION_SERVICE_UPDATE)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverBulkSubscriptionServiceUpdate
  )

type GetDriverStats =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_STATS)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverStats
  )

type GetDriverEarnings =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.GET_DRIVER_EARNINGS)
      :> API.Types.ProviderPlatform.Management.Driver.GetDriverEarnings
  )

type PostDriverUpdateMerchant =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER / 'API.Types.ProviderPlatform.Management.Driver.POST_DRIVER_UPDATE_MERCHANT)
      :> API.Types.ProviderPlatform.Management.Driver.PostDriverUpdateMerchant
  )

getDriverDocumentsInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler Dashboard.Common.Driver.DriverDocumentsInfoRes)
getDriverDocumentsInfo merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverDocumentsInfo merchantShortId opCity apiTokenInfo

postDriverPersonNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.PersonIdsReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postDriverPersonNumbers merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverPersonNumbers merchantShortId opCity apiTokenInfo req

postDriverUpdateTagBulk :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.UpdateTagBulkReq -> Environment.FlowHandler [Dashboard.Common.UpdateTagBulkRes])
postDriverUpdateTagBulk merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUpdateTagBulk merchantShortId opCity apiTokenInfo req

postDriverDriverDataDecryption :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> [API.Types.ProviderPlatform.Management.Driver.DriverEncDataReq] -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.DriverDecDataResp])
postDriverDriverDataDecryption merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverDriverDataDecryption merchantShortId opCity apiTokenInfo req

postDriverPersonId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.PersonMobileNoReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postDriverPersonId merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverPersonId merchantShortId opCity apiTokenInfo req

getDriverAadhaarInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoRes)
getDriverAadhaarInfo merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverAadhaarInfo merchantShortId opCity apiTokenInfo driverId

getDriverAadhaarInfobyMobileNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoByPhoneReq)
getDriverAadhaarInfobyMobileNumber merchantShortId opCity apiTokenInfo mobileNo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverAadhaarInfobyMobileNumber merchantShortId opCity apiTokenInfo mobileNo

getDriverList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverListRes)
getDriverList merchantShortId opCity apiTokenInfo limit offset verified enabled blocked subscribed phone vehicleNumberSearchString mbNameSearchString = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverList merchantShortId opCity apiTokenInfo limit offset verified enabled blocked subscribed phone vehicleNumberSearchString mbNameSearchString

getDriverActivity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler Dashboard.Common.Driver.DriverActivityRes)
getDriverActivity merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverActivity merchantShortId opCity apiTokenInfo

postDriverDisable :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDisable merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverDisable merchantShortId opCity apiTokenInfo driverId

postDriverAcRestrictionUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateACUsageRestrictionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAcRestrictionUpdate merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverAcRestrictionUpdate merchantShortId opCity apiTokenInfo driverId req

postDriverBlockWithReason :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.BlockDriverWithReasonReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBlockWithReason merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverBlockWithReason merchantShortId opCity apiTokenInfo driverId req

postDriverBlock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBlock merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverBlock merchantShortId opCity apiTokenInfo driverId

getDriverBlockReasonList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.BlockReason])
getDriverBlockReasonList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverBlockReasonList merchantShortId opCity apiTokenInfo

postDriverUnblock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnblock merchantShortId opCity apiTokenInfo driverId preventWeeklyCancellationRateBlockingTill preventDailyCancellationRateBlockingTill = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUnblock merchantShortId opCity apiTokenInfo driverId preventWeeklyCancellationRateBlockingTill preventDailyCancellationRateBlockingTill

getDriverLocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Dashboard.Common.Driver.DriverIds -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverLocationRes)
getDriverLocation merchantShortId opCity apiTokenInfo limit offset req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverLocation merchantShortId opCity apiTokenInfo limit offset req

deleteDriverPermanentlyDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteDriverPermanentlyDelete merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.deleteDriverPermanentlyDelete merchantShortId opCity apiTokenInfo driverId

postDriverUnlinkDL :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkDL merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUnlinkDL merchantShortId opCity apiTokenInfo driverId

postDriverUnlinkAadhaar :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkAadhaar merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUnlinkAadhaar merchantShortId opCity apiTokenInfo driverId

postDriverUpdatePhoneNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdatePhoneNumberReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdatePhoneNumber merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUpdatePhoneNumber merchantShortId opCity apiTokenInfo driverId req

postDriverUpdateByPhoneNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverDataReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateByPhoneNumber merchantShortId opCity apiTokenInfo mobileNo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUpdateByPhoneNumber merchantShortId opCity apiTokenInfo mobileNo req

postDriverUpdateName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverNameReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateName merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUpdateName merchantShortId opCity apiTokenInfo driverId req

postDriverDeleteRC :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.DeleteRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDeleteRC merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverDeleteRC merchantShortId opCity apiTokenInfo driverId req

getDriverClearStuckOnRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.ClearOnRideStuckDriversRes)
getDriverClearStuckOnRide merchantShortId opCity apiTokenInfo dbSyncTime = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverClearStuckOnRide merchantShortId opCity apiTokenInfo dbSyncTime

postDriverSendDummyNotification :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSendDummyNotification merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverSendDummyNotification merchantShortId opCity apiTokenInfo driverId

postDriverChangeOperatingCity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ChangeOperatingCityReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverChangeOperatingCity merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverChangeOperatingCity merchantShortId opCity apiTokenInfo driverId req

getDriverGetOperatingCity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.GetOperatingCityResp)
getDriverGetOperatingCity merchantShortId opCity apiTokenInfo mobileCountryCode mobileNumber rideId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverGetOperatingCity merchantShortId opCity apiTokenInfo mobileCountryCode mobileNumber rideId

postDriverPauseOrResumeServiceCharges :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.PauseOrResumeServiceChargesReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverPauseOrResumeServiceCharges merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverPauseOrResumeServiceCharges merchantShortId opCity apiTokenInfo driverId req

postDriverUpdateRCInvalidStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateRCInvalidStatus merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUpdateRCInvalidStatus merchantShortId opCity apiTokenInfo driverId req

postDriverUpdateRCInvalidStatusByRCNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusByRCNumberReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateRCInvalidStatusByRCNumber merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUpdateRCInvalidStatusByRCNumber merchantShortId opCity apiTokenInfo req

postDriverUpdateVehicleVariant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateVehicleVariantReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateVehicleVariant merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUpdateVehicleVariant merchantShortId opCity apiTokenInfo driverId req

postDriverBulkReviewRCVariant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantReq] -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantRes])
postDriverBulkReviewRCVariant merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverBulkReviewRCVariant merchantShortId opCity apiTokenInfo req

postDriverUpdateDriverTag :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverTagReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateDriverTag merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUpdateDriverTag merchantShortId opCity apiTokenInfo driverId req

postDriverClearFee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverClearFee merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverClearFee merchantShortId opCity apiTokenInfo driverId req

getDriverPanAadharSelfieDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsResp)
getDriverPanAadharSelfieDetails merchantShortId opCity apiTokenInfo countryCode phoneNo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverPanAadharSelfieDetails merchantShortId opCity apiTokenInfo countryCode phoneNo

postDriverSyncDocAadharPan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Driver.AadharPanSyncReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSyncDocAadharPan merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverSyncDocAadharPan merchantShortId opCity apiTokenInfo req

postDriverUpdateVehicleManufacturing :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateVehicleManufacturingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateVehicleManufacturing merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUpdateVehicleManufacturing merchantShortId opCity apiTokenInfo driverId req

postDriverVehicleAppendSelectedServiceTiers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.AppendSelectedServiceTiersReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverVehicleAppendSelectedServiceTiers merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverVehicleAppendSelectedServiceTiers merchantShortId opCity apiTokenInfo driverId req

postDriverVehicleUpsertSelectedServiceTiers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Driver.UpsertDriverServiceTiersCsvReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverVehicleUpsertSelectedServiceTiers merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverVehicleUpsertSelectedServiceTiers merchantShortId opCity apiTokenInfo req

postDriverRefundByPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.RefundByPayoutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRefundByPayout merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverRefundByPayout merchantShortId opCity apiTokenInfo driverId req

getDriverSecurityDepositStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Dashboard.Common.Driver.ServiceNames -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.SecurityDepositDfStatusRes])
getDriverSecurityDepositStatus merchantShortId opCity apiTokenInfo driverId serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverSecurityDepositStatus merchantShortId opCity apiTokenInfo driverId serviceName

getDriverPanAadharSelfieDetailsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsListResp])
getDriverPanAadharSelfieDetailsList merchantShortId opCity apiTokenInfo docType driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverPanAadharSelfieDetailsList merchantShortId opCity apiTokenInfo docType driverId

postDriverBulkSubscriptionServiceUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Driver.BulkServiceUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBulkSubscriptionServiceUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverBulkSubscriptionServiceUpdate merchantShortId opCity apiTokenInfo req

getDriverStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverStatsRes)
getDriverStats merchantShortId opCity apiTokenInfo entityId fromDate toDate = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverStats merchantShortId opCity apiTokenInfo entityId fromDate toDate

getDriverEarnings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Time.Calendar.Day -> Data.Time.Calendar.Day -> Dashboard.Common.Driver.EarningType -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.EarningPeriodStatsRes)
getDriverEarnings merchantShortId opCity apiTokenInfo from to earningType driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.getDriverEarnings merchantShortId opCity apiTokenInfo from to earningType driverId

postDriverUpdateMerchant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverMerchantReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateMerchant merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Driver.postDriverUpdateMerchant merchantShortId opCity apiTokenInfo driverId req
