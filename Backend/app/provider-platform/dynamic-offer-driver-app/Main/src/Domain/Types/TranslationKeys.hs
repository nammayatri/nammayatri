{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The driver platform's code-owned text keys, one type per table that holds the text.
-- See "Domain.Types.TranslationKey" for the stored form and the legacy-name rule.
module Domain.Types.TranslationKeys
  ( TranslationKey (..),
    EarningsLabel (..),
    AcUsageMessage (..),
    RcRuleFailure (..),
    DriverPushKey (..),
    pushNotificationType,
    SmsKey (..),
    VerificationMessage (..),
  )
where

import qualified Dashboard.Common.DriverCoins as DCoins
import qualified DashboardAlert.Domain.Types.Audience as DAA
import qualified Domain.Types.Alert.AlertRequestType as DAlertType
import qualified Domain.Types.Alert.OnboardingAlertAction as DOAA
import Domain.Types.DocumentOnboardingStage (DocumentOnboardingStage (..), LegalStructure (..))
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.ServiceTierType as DST
import Domain.Types.TranslationKey
import qualified IssueManagement.Common as IC
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Text.Show

-- | Text stored in @translations@.
data TranslationKey
  = Document_Title DVC.DocumentType
  | Document_Description DVC.DocumentType
  | Document_Name DVC.DocumentType
  | FleetOwnerDocument_Title DVC.DocumentType
  | FleetOwnerDocument_Description DVC.DocumentType
  | OnboardingStage_Title DocumentOnboardingStage
  | OnboardingStage_Description DocumentOnboardingStage
  | Verification_Message VerificationMessage
  | DocumentDecision_Title DMM.MessageKey
  | DocumentDecision_Body DMM.MessageKey
  | Earnings_Label EarningsLabel
  | AcUsage_Message AcUsageMessage
  | RcRule_Failure RcRuleFailure
  | Coins_MetroMessage DCoins.MetroRideType
  | Coins_Message DCoins.CoinMessage
  | ServiceTier_Name DST.ServiceTierType
  | ServiceTier_ShortDescription DST.ServiceTierType
  | ServiceTier_LongDescription DST.ServiceTierType
  | Wallet_Item Text
  | DocumentField_Label DVC.DocumentType Text
  | DocumentField_Hint DVC.DocumentType Text
  | DocumentField_Placeholder DVC.DocumentType Text
  | DocumentField_Description DVC.DocumentType Text
  | -- A sample image's title and text, named by its English title ('textKeyName')
    DocumentField_ImageTitle DVC.DocumentType Text Text
  | DocumentField_ImageText DVC.DocumentType Text Text
  deriving stock (Eq)

instance Show TranslationKey where
  show = \case
    Document_Title d -> dotted ["Document", "Title", keyName d]
    Document_Description d -> dotted ["Document", "Description", keyName d]
    Document_Name d -> dotted ["Document", "Name", keyName d]
    FleetOwnerDocument_Title d -> dotted ["FleetOwnerDocument", "Title", keyName d]
    FleetOwnerDocument_Description d -> dotted ["FleetOwnerDocument", "Description", keyName d]
    OnboardingStage_Title s -> dotted ["OnboardingStage", "Title", keyName s]
    OnboardingStage_Description s -> dotted ["OnboardingStage", "Description", keyName s]
    Verification_Message m -> dotted ["Verification", "Message", verificationMessageText m]
    DocumentDecision_Title k -> dotted ["DocumentDecision", "Title", keyName k]
    DocumentDecision_Body k -> dotted ["DocumentDecision", "Body", keyName k]
    Earnings_Label l -> dotted ["Earnings", "Label", keyName l]
    AcUsage_Message m -> dotted ["AcUsage", "Message", keyName m]
    RcRule_Failure f -> dotted ["RcRule", "Failure", keyName f]
    Coins_MetroMessage m -> dotted ["Coins", "MetroMessage", keyName m]
    Coins_Message m -> dotted ["Coins", "Message", keyName m]
    ServiceTier_Name t -> dotted ["ServiceTier", "Name", keyName t]
    ServiceTier_ShortDescription t -> dotted ["ServiceTier", "ShortDescription", keyName t]
    ServiceTier_LongDescription t -> dotted ["ServiceTier", "LongDescription", keyName t]
    Wallet_Item ref -> dotted ["Wallet", "Item", ref]
    DocumentField_Label d f -> dotted ["DocumentField", "Label", keyName d, f]
    DocumentField_Hint d f -> dotted ["DocumentField", "Hint", keyName d, f]
    DocumentField_Placeholder d f -> dotted ["DocumentField", "Placeholder", keyName d, f]
    DocumentField_Description d f -> dotted ["DocumentField", "Description", keyName d, f]
    DocumentField_ImageTitle d f n -> dotted ["DocumentField", "ImageTitle", keyName d, f, n]
    DocumentField_ImageText d f n -> dotted ["DocumentField", "ImageText", keyName d, f, n]

-- | A key named by free text is left out of 'allKeys', which lists only what the code can enumerate;
-- the dashboard lists such keys from where their names live (wallet items from 'allWalletReferences').
instance IsTranslationKey TranslationKey where
  allKeys =
    concat
      [ map Document_Title documentTypes,
        map Document_Description documentTypes,
        map Document_Name documentTypes,
        map FleetOwnerDocument_Title documentTypes,
        map FleetOwnerDocument_Description documentTypes,
        map OnboardingStage_Title allDocumentOnboardingStages,
        map OnboardingStage_Description allDocumentOnboardingStages,
        map Verification_Message [minBound .. maxBound],
        map DocumentDecision_Title documentDecisions,
        map DocumentDecision_Body documentDecisions,
        map Earnings_Label [minBound .. maxBound],
        map AcUsage_Message [minBound .. maxBound],
        map RcRule_Failure [minBound .. maxBound],
        map Coins_MetroMessage [minBound .. maxBound],
        map Coins_Message [minBound .. maxBound],
        map ServiceTier_Name serviceTiers,
        map ServiceTier_ShortDescription serviceTiers,
        map ServiceTier_LongDescription serviceTiers
      ]
    where
      documentTypes = [minBound .. maxBound]
      serviceTiers = [minBound .. maxBound]
      documentDecisions = [DMM.DOCUMENT_INVALID, DMM.DOCUMENT_VALID]

  -- The fleet-owner keys read the same rows as the driver keys, so both carry that name.
  legacyKeyName = \case
    Document_Title d -> Just (keyName d <> "_Title")
    Document_Description d -> Just (keyName d <> "_Description")
    Document_Name d -> Just ("DOC_TYPE_" <> keyName d)
    FleetOwnerDocument_Title d -> Just (keyName d <> "_Title")
    FleetOwnerDocument_Description d -> Just (keyName d <> "_Description")
    OnboardingStage_Title s -> Just ("DocumentOnboardingStage_" <> keyName s <> "_Title")
    OnboardingStage_Description s -> Just ("DocumentOnboardingStage_" <> keyName s <> "_Description")
    Verification_Message m -> Just (keyName m)
    DocumentDecision_Title k -> Just (keyName k <> "_TITLE")
    DocumentDecision_Body k -> Just (keyName k <> "_BODY")
    Earnings_Label l -> Just (earningsLabelText l)
    AcUsage_Message Restricted -> Just "AC_RESTRICTION_MESSAGE"
    AcUsage_Message Working -> Just "AC_WORKING_MESSAGE"
    RcRule_Failure f -> Just (keyName f)
    Coins_MetroMessage m -> Just (keyName m)
    Coins_Message m -> Just (keyName m)
    ServiceTier_Name _ -> Nothing
    ServiceTier_ShortDescription _ -> Nothing
    ServiceTier_LongDescription _ -> Nothing
    Wallet_Item _ -> Nothing
    DocumentField_Label _ _ -> Nothing
    DocumentField_Hint _ _ -> Nothing
    DocumentField_Placeholder _ _ -> Nothing
    DocumentField_Description _ _ -> Nothing
    DocumentField_ImageTitle {} -> Nothing
    DocumentField_ImageText {} -> Nothing

allDocumentOnboardingStages :: [DocumentOnboardingStage]
allDocumentOnboardingStages =
  [ PersonalOnboarding,
    VehicleDetailsStage,
    CompanyOnboarding,
    TaxAndLegal IndividualLegalStructure,
    TaxAndLegal LegalEntityStructure,
    BankDetails
  ]

-- | The document-status line. Two constructors are misspelt; the stored key text is spelt
-- correctly and the legacy name keeps the misspelling the lookups used.
data VerificationMessage
  = InvalidDocumentNumber
  | VerficationFailed
  | NoDcoumentFound
  | LimitExceed
  | DLInvalid
  | RCInvalid
  | DocumentInvalid
  | DocumentValid
  | VerificationInProgress
  | VerificationPendingOnUserInput
  | UnderManualReview
  | Unauthorized
  | PullRequired
  | ConsentDenied
  | Other
  | Reasons
  deriving stock (Show, Eq, Ord, Bounded, Enum)

verificationMessageText :: VerificationMessage -> Text
verificationMessageText = \case
  VerficationFailed -> "VerificationFailed"
  NoDcoumentFound -> "NoDocumentFound"
  m -> keyName m

-- | The row names of the driver's earnings breakdown on ride details.
data EarningsLabel
  = FarePaidByCustomer
  | Discount
  | Tips
  | Commission
  | Tax
  | NetDriverEarnings
  | Fare
  | AirportConvenienceFee
  | ServiceCharge
  | PaymentCharge
  | PaymentChargeVat
  | PaymentChargePaidByCustomer
  | TollCharges
  | CommissionInclTax
  | PaymentChargeInclTax
  | BuyerDiscount
  deriving stock (Show, Eq, Ord, Bounded, Enum)

-- | The legacy row key of each earnings label.
earningsLabelText :: EarningsLabel -> Text
earningsLabelText = \case
  FarePaidByCustomer -> "FARE_PAID_BY_CUSTOMER"
  Discount -> "DISCOUNT"
  Tips -> "TIPS"
  Commission -> "COMMISSION"
  Tax -> "TAX"
  NetDriverEarnings -> "NET_DRIVER_EARNINGS"
  Fare -> "FARE"
  AirportConvenienceFee -> "AIRPORT_CONVENIENCE_FEE"
  ServiceCharge -> "SERVICE_CHARGE"
  PaymentCharge -> "PAYMENT_CHARGE"
  PaymentChargeVat -> "PAYMENT_CHARGE_VAT"
  PaymentChargePaidByCustomer -> "PAYMENT_CHARGE_PAID_BY_CUSTOMER"
  TollCharges -> "TOLL_CHARGES"
  CommissionInclTax -> "COMMISSION_INCL_TAX"
  PaymentChargeInclTax -> "PAYMENT_CHARGE_INCL_TAX"
  BuyerDiscount -> "BUYER_DISCOUNT"

-- | The AC-usage note on the driver's vehicle details.
data AcUsageMessage = Restricted | Working
  deriving stock (Show, Eq, Ord, Bounded, Enum)

-- | The reason shown when a vehicle registration fails a rule.
data RcRuleFailure
  = InvalidFuelType
  | InvalidVehicleClass
  | InvalidOEM
  | InvalidManufacturingYear
  | FitnessCertificateExpired
  | InsuranceExpired
  | PermitExpired
  | PUCExpired
  deriving stock (Show, Eq, Ord, Bounded, Enum)

-- | Text stored in @merchant_push_notification@. The second segment names the recipient where
-- the same event reaches more than one person.
data DriverPushKey
  = DriverPush_DocumentDecision DMM.MessageKey
  | DriverPush_Onboarding_Approved
  | DriverPush_Vehicle_Approved
  | DriverPush_Driver_OnboardingAsChanged
  | DriverPush_FleetOwner_DriverOnboardingAsChanged
  | DriverPush_Driver_FleetOwnerChanged
  | DriverPush_FleetOwner_DriverAdded
  | DriverPush_FleetOwner_DriverRemoved
  | DriverPush_FleetOwner_DriverUnlinked
  | DriverPush_FleetOwner_DriverLeft
  | DriverPush_Driver_RemovedFromFleet
  | DriverPush_Driver_LeftFleet
  | DriverPush_FleetOwner_VehicleUnlinked
  | DriverPush_Driver_VehicleUnlinked
  | DriverPush_Driver_DeclinedFleetInvite
  | DriverPush_FleetOwner_DriverDeclinedInvite
  | DriverPush_Driver_FleetRequestApproved
  | DriverPush_Driver_FleetRequestRejected
  | DriverPush_Operator_DriverUnlinked
  | DriverPush_Operator_FleetUnlinked
  | DriverPush_Driver_Unblocked
  | DriverPush_OnboardingAlert DOAA.OnboardingAlertAction DAA.DashboardAccessType
  | DriverPush_Payout_Completed
  | DriverPush_Payout_Failed
  | DriverPush_Payout_FailedWithDetail
  | DriverPush_Payout_VpaAlert
  | DriverPush_Payout_ReferralReward
  | DriverPush_IssueStatus IC.IssueStatus
  | DriverPush_IssueChat_Title
  | DriverPush_IssueChat_ImageFallback
  | DriverPush_IssueChat_MessageFallback
  | -- Ride
    DriverPush_Ride_Cancelled
  | DriverPush_Ride_NewAvailable
  | DriverPush_Ride_Started
  | DriverPush_Ride_StartedAc
  | DriverPush_Ride_AllocationRequest
  | DriverPush_Ride_ClearedFare
  | DriverPush_Ride_SearchCancelled
  | DriverPush_Ride_StopAdded
  | DriverPush_Ride_StopEdited
  | DriverPush_Ride_StopDetected
  | DriverPush_Ride_RouteDeviation
  | DriverPush_Ride_Stoppage
  | DriverPush_Ride_TollCrossed
  | DriverPush_Ride_MissedCall
  | DriverPush_Ride_EkdLiveCallFeedback
  | DriverPush_SharedCab_Allocation
  | DriverPush_SharedCab_CheckIn
  | DriverPush_AvailableForRidesEnded_RequestLimitReached
  | DriverPush_AvailableForRidesEnded_ConsecutiveRejections
  | DriverPush_AvailableForRidesEnded_DriverAtFaultCancellation
  | -- Driver
    DriverPush_Driver_RegistrationApproved
  | DriverPush_Driver_FeedbackBadge
  | DriverPush_Driver_MarkedFavourite
  | DriverPush_Driver_Referral
  | DriverPush_Driver_FleetConsent
  | DriverPush_Driver_OperatorConsent
  | DriverPush_IssueBreach_ExtraFareMitigation
  | -- Vehicle
    DriverPush_Vehicle_UnhygienicWarning
  | DriverPush_Vehicle_UnsafeWarning
  | DriverPush_PickupZone_Request
  | DriverPush_PickupZone_NoShow
  | -- Reminders
    DriverPush_Reminder_VehicleInspection
  | DriverPush_Reminder_DriverInspection
  | DriverPush_Reminder_TrainingVideo
  | DriverPush_Reminder_DocumentExpiry
  | -- Subscription payments
    DriverPush_Payment_Failed
  | DriverPush_Payment_Pending
  | DriverPush_Payment_Success
  | DriverPush_Payment_ManualModeOnCancel
  | DriverPush_Payment_ManualModeOnPause
  | DriverPush_Payment_ManualModeOnSuspend
  | -- Safety (sent to the driver's emergency contacts)
    DriverPush_Safety_SosResolvedSafe
  | DriverPush_Safety_LiveTrackingStopped
  | -- Dashboard config alerts
    DriverPush_ConfigAlert DAlertType.AlertRequestType DAA.DashboardAccessType
  deriving stock (Eq, Generic)

instance Show DriverPushKey where
  show = \case
    DriverPush_DocumentDecision decision -> dotted ["DriverPush", "DocumentDecision", keyName decision]
    DriverPush_OnboardingAlert action audience -> dotted ["DriverPush", "OnboardingAlert", keyName action, keyName audience]
    DriverPush_IssueStatus status -> dotted ["DriverPush", "IssueStatus", keyName status]
    DriverPush_ConfigAlert requestType audience -> dotted ["DriverPush", "ConfigAlert", keyName requestType, keyName audience]
    k -> dottedConstructorName k

instance IsTranslationKey DriverPushKey where
  allKeys =
    map DriverPush_DocumentDecision [DMM.DOCUMENT_INVALID, DMM.DOCUMENT_VALID]
      <> [ DriverPush_Onboarding_Approved,
           DriverPush_Vehicle_Approved,
           DriverPush_Driver_OnboardingAsChanged,
           DriverPush_FleetOwner_DriverOnboardingAsChanged,
           DriverPush_Driver_FleetOwnerChanged,
           DriverPush_FleetOwner_DriverAdded,
           DriverPush_FleetOwner_DriverRemoved,
           DriverPush_FleetOwner_DriverUnlinked,
           DriverPush_FleetOwner_DriverLeft,
           DriverPush_Driver_RemovedFromFleet,
           DriverPush_Driver_LeftFleet,
           DriverPush_FleetOwner_VehicleUnlinked,
           DriverPush_Driver_VehicleUnlinked,
           DriverPush_Driver_DeclinedFleetInvite,
           DriverPush_FleetOwner_DriverDeclinedInvite,
           DriverPush_Driver_FleetRequestApproved,
           DriverPush_Driver_FleetRequestRejected,
           DriverPush_Operator_DriverUnlinked,
           DriverPush_Operator_FleetUnlinked,
           DriverPush_Driver_Unblocked
         ]
      <> map (uncurry DriverPush_OnboardingAlert) reachableOnboardingAlerts
      <> [ DriverPush_Payout_Completed,
           DriverPush_Payout_Failed,
           DriverPush_Payout_FailedWithDetail,
           DriverPush_Payout_VpaAlert,
           DriverPush_Payout_ReferralReward
         ]
      <> map DriverPush_IssueStatus [minBound .. maxBound]
      <> [ DriverPush_IssueChat_Title,
           DriverPush_IssueChat_ImageFallback,
           DriverPush_IssueChat_MessageFallback,
           DriverPush_Ride_Cancelled,
           DriverPush_Ride_NewAvailable,
           DriverPush_Ride_Started,
           DriverPush_Ride_StartedAc,
           DriverPush_Ride_AllocationRequest,
           DriverPush_Ride_ClearedFare,
           DriverPush_Ride_SearchCancelled,
           DriverPush_Ride_StopAdded,
           DriverPush_Ride_StopEdited,
           DriverPush_Ride_StopDetected,
           DriverPush_Ride_RouteDeviation,
           DriverPush_Ride_Stoppage,
           DriverPush_Ride_TollCrossed,
           DriverPush_Ride_MissedCall,
           DriverPush_Ride_EkdLiveCallFeedback,
           DriverPush_SharedCab_Allocation,
           DriverPush_SharedCab_CheckIn,
           DriverPush_AvailableForRidesEnded_RequestLimitReached,
           DriverPush_AvailableForRidesEnded_ConsecutiveRejections,
           DriverPush_AvailableForRidesEnded_DriverAtFaultCancellation,
           DriverPush_Driver_RegistrationApproved,
           DriverPush_Driver_FeedbackBadge,
           DriverPush_Driver_MarkedFavourite,
           DriverPush_Driver_Referral,
           DriverPush_Driver_FleetConsent,
           DriverPush_Driver_OperatorConsent,
           DriverPush_IssueBreach_ExtraFareMitigation,
           DriverPush_Vehicle_UnhygienicWarning,
           DriverPush_Vehicle_UnsafeWarning,
           DriverPush_PickupZone_Request,
           DriverPush_PickupZone_NoShow,
           DriverPush_Reminder_VehicleInspection,
           DriverPush_Reminder_DriverInspection,
           DriverPush_Reminder_TrainingVideo,
           DriverPush_Reminder_DocumentExpiry,
           DriverPush_Payment_Failed,
           DriverPush_Payment_Pending,
           DriverPush_Payment_Success,
           DriverPush_Payment_ManualModeOnCancel,
           DriverPush_Payment_ManualModeOnPause,
           DriverPush_Payment_ManualModeOnSuspend,
           DriverPush_Safety_SosResolvedSafe,
           DriverPush_Safety_LiveTrackingStopped
         ]
      <> [ DriverPush_ConfigAlert requestType audience
           | requestType <- [minBound .. maxBound],
             audience <- [DAA.DASHBOARD_ADMIN, DAA.DASHBOARD_USER]
         ]

  legacyKeyName = \case
    DriverPush_DocumentDecision decision -> Just (keyName decision)
    DriverPush_Onboarding_Approved -> Just "ONBOARDING_APPROVED"
    DriverPush_Vehicle_Approved -> Just "VEHICLE_APPROVED"
    DriverPush_Driver_OnboardingAsChanged -> Just "DRIVER_ONBOARDING_AS_CHANGED"
    DriverPush_FleetOwner_DriverOnboardingAsChanged -> Just "FLEET_OWNER_DRIVER_ONBOARDING_AS_CHANGED"
    DriverPush_Driver_FleetOwnerChanged -> Just "DRIVER_FLEET_OWNER_CHANGED"
    DriverPush_FleetOwner_DriverAdded -> Just "FLEET_OWNER_DRIVER_ADDED"
    DriverPush_FleetOwner_DriverRemoved -> Just "FLEET_OWNER_DRIVER_REMOVED"
    DriverPush_FleetOwner_DriverUnlinked -> Just "DRIVER_UNLINK_FROM_FLEET"
    DriverPush_FleetOwner_DriverLeft -> Just "FLEET_OWNER_DRIVER_LEFT"
    DriverPush_Driver_RemovedFromFleet -> Just "DRIVER_REMOVED_FROM_FLEET"
    DriverPush_Driver_LeftFleet -> Just "DRIVER_LEFT_FLEET"
    DriverPush_FleetOwner_VehicleUnlinked -> Just "FLEET_OWNER_VEHICLE_UNLINKED"
    DriverPush_Driver_VehicleUnlinked -> Just "FLEET_VEHICLE_UNLINKED"
    DriverPush_Driver_DeclinedFleetInvite -> Just "DRIVER_DECLINED_FLEET_INVITE"
    DriverPush_FleetOwner_DriverDeclinedInvite -> Just "FLEET_OWNER_DRIVER_DECLINED_INVITE"
    DriverPush_Driver_FleetRequestApproved -> Just "FLEET_REQUEST_APPROVED"
    DriverPush_Driver_FleetRequestRejected -> Just "FLEET_REQUEST_REJECTED"
    DriverPush_Operator_DriverUnlinked -> Just "DRIVER_UNLINK_FROM_OPERATOR"
    DriverPush_Operator_FleetUnlinked -> Just "FLEET_UNLINK_FROM_OPERATOR"
    DriverPush_Driver_Unblocked -> Just "UNBLOCK_DRIVER_KEY"
    DriverPush_OnboardingAlert action audience -> Just ("ONBOARDING_ALERT_" <> onboardingAlertActionText action <> "_" <> keyName audience)
    DriverPush_Payout_Completed -> Nothing
    DriverPush_Payout_Failed -> Nothing
    DriverPush_Payout_FailedWithDetail -> Nothing
    DriverPush_Payout_VpaAlert -> Just "PAYOUT_VPA_ALERT"
    DriverPush_Payout_ReferralReward -> Just "PAYOUT_REFERRAL_REWARD"
    DriverPush_IssueStatus _ -> Nothing
    DriverPush_IssueChat_Title -> Nothing
    DriverPush_IssueChat_ImageFallback -> Nothing
    DriverPush_IssueChat_MessageFallback -> Nothing
    DriverPush_Ride_Cancelled -> Just "NOTIFY_DRIVER_ON_CANCEL"
    DriverPush_Ride_NewAvailable -> Just "NEW_RIDE_AVAILABLE"
    DriverPush_Ride_Started -> Just "RIDE_STARTED"
    DriverPush_Ride_StartedAc -> Just "AC_RIDE_STARTED"
    DriverPush_Ride_AllocationRequest -> Just "ALLOCATION_REQUEST"
    DriverPush_Ride_ClearedFare -> Just "CLEARED_FARE"
    DriverPush_Ride_SearchCancelled -> Just "CANCELLED_SEARCH_REQUEST"
    DriverPush_Ride_StopAdded -> Just "ADD_STOP"
    DriverPush_Ride_StopEdited -> Just "EDIT_STOP"
    DriverPush_Ride_StopDetected -> Just "STOP_DETECTION_KEY"
    DriverPush_Ride_RouteDeviation -> Just "DRIVER_ROUTE_DEVIATION"
    DriverPush_Ride_Stoppage -> Just "DRIVER_RIDE_STOPPAGE"
    DriverPush_Ride_TollCrossed -> Just "TOLL_CROSSED"
    DriverPush_Ride_MissedCall -> Just "FCM_CHAT_MESSAGE"
    DriverPush_Ride_EkdLiveCallFeedback -> Just "EKD_LIVE_CALL_FEEDBACK"
    DriverPush_SharedCab_Allocation -> Just "SHARED_CAB_ALLOCATION"
    DriverPush_SharedCab_CheckIn -> Just "SHARED_CAB_CHECK_IN"
    DriverPush_AvailableForRidesEnded_RequestLimitReached -> Just "AVAILABLE_FOR_RIDES_ENDED_REQUEST_LIMIT_REACHED"
    DriverPush_AvailableForRidesEnded_ConsecutiveRejections -> Just "AVAILABLE_FOR_RIDES_ENDED_CONSECUTIVE_REJECTIONS"
    DriverPush_AvailableForRidesEnded_DriverAtFaultCancellation -> Just "AVAILABLE_FOR_RIDES_ENDED_DRIVER_AT_FAULT_CANCELLATION"
    DriverPush_Driver_RegistrationApproved -> Just "REGISTRATION_APPROVED"
    DriverPush_Driver_FeedbackBadge -> Just "FEEDBACK_BADGE_PN"
    DriverPush_Driver_MarkedFavourite -> Just "FAVOURITE_DRIVER_ALERT"
    DriverPush_Driver_Referral -> Just "REFERRAL_FLOW"
    DriverPush_Driver_FleetConsent -> Just "FLEET_CONSENT"
    DriverPush_Driver_OperatorConsent -> Just "OPERATOR_CONSENT"
    DriverPush_IssueBreach_ExtraFareMitigation -> Just "ISSUE_BREACH_EXTRA_FARE_MITIGATION"
    DriverPush_Vehicle_UnhygienicWarning -> Just "UNHYGIENIC_VEHICLE_WARNING"
    DriverPush_Vehicle_UnsafeWarning -> Just "VEHICLE_UNSAFE_WARNING"
    DriverPush_PickupZone_Request -> Just "PICKUP_ZONE_REQUEST"
    DriverPush_PickupZone_NoShow -> Just "PICKUP_ZONE_NO_SHOW"
    DriverPush_Reminder_VehicleInspection -> Just "VEHICLE_INSPECTION"
    DriverPush_Reminder_DriverInspection -> Just "DRIVER_INSPECTION"
    DriverPush_Reminder_TrainingVideo -> Just "TRAINING_VIDEO"
    DriverPush_Reminder_DocumentExpiry -> Just "DOCUMENT_EXPIRY_REMINDER"
    DriverPush_Payment_Failed -> Just "PAYMENT_FAILED"
    DriverPush_Payment_Pending -> Just "PAYMENT_PENDING"
    DriverPush_Payment_Success -> Just "PAYMENT_SUCCESS"
    DriverPush_Payment_ManualModeOnCancel -> Just "PAYMENT_MODE_MANUAL_ON_CANCEL"
    DriverPush_Payment_ManualModeOnPause -> Just "PAYMENT_MODE_MANUAL_ON_PAUSE"
    DriverPush_Payment_ManualModeOnSuspend -> Just "PAYMENT_MODE_MANUAL_ON_SUSPEND"
    DriverPush_Safety_SosResolvedSafe -> Just "SOS_RESOLVED_SAFE"
    DriverPush_Safety_LiveTrackingStopped -> Just "LIVE_TRACKING_STOPPED"
    DriverPush_ConfigAlert requestType audience -> Just ("CONFIG_ALERT_" <> configAlertRequestText requestType <> "_" <> keyName audience)

-- | The notification type a key's first row gets when the dashboard creates it: the one the
-- sender passes in code, so the row only carries the texts. Nothing for keys whose sender takes
-- the type from the row, for the bell alerts (not FCM) and for keys nothing sends; their first
-- row keeps coming from SQL or ConfigPilot.
pushNotificationType :: DriverPushKey -> Maybe FCM.FCMNotificationType
pushNotificationType = \case
  DriverPush_DocumentDecision DMM.DOCUMENT_INVALID -> Just FCM.DOCUMENT_INVALID
  DriverPush_DocumentDecision DMM.DOCUMENT_VALID -> Just FCM.DRIVER_NOTIFY
  DriverPush_Onboarding_Approved -> Just FCM.DRIVER_NOTIFY
  DriverPush_Vehicle_Approved -> Just FCM.DRIVER_NOTIFY
  DriverPush_Driver_OnboardingAsChanged -> Just FCM.DRIVER_NOTIFY
  DriverPush_FleetOwner_DriverOnboardingAsChanged -> Just FCM.DRIVER_NOTIFY
  DriverPush_Driver_FleetOwnerChanged -> Just FCM.DRIVER_NOTIFY
  DriverPush_FleetOwner_DriverAdded -> Just FCM.DRIVER_NOTIFY
  DriverPush_FleetOwner_DriverRemoved -> Just FCM.DRIVER_NOTIFY
  DriverPush_FleetOwner_DriverUnlinked -> Just FCM.DRIVER_UNLINK_FROM_FLEET
  DriverPush_FleetOwner_DriverLeft -> Just FCM.DRIVER_UNLINK_FROM_FLEET
  DriverPush_FleetOwner_DriverDeclinedInvite -> Just FCM.DRIVER_UNLINK_FROM_FLEET
  DriverPush_Driver_RemovedFromFleet -> Just FCM.DRIVER_NOTIFY
  DriverPush_Driver_LeftFleet -> Just FCM.DRIVER_NOTIFY
  DriverPush_Driver_DeclinedFleetInvite -> Just FCM.DRIVER_NOTIFY
  DriverPush_FleetOwner_VehicleUnlinked -> Just FCM.DRIVER_NOTIFY
  DriverPush_Driver_VehicleUnlinked -> Just FCM.DRIVER_NOTIFY
  DriverPush_Operator_DriverUnlinked -> Just FCM.DRIVER_UNLINK_FROM_OPERATOR
  DriverPush_Operator_FleetUnlinked -> Just FCM.FLEET_UNLINK_FROM_OPERATOR
  DriverPush_Payout_Completed -> Just FCM.PAYOUT_COMPLETED
  DriverPush_Payout_Failed -> Just FCM.PAYOUT_FAILED
  DriverPush_Payout_FailedWithDetail -> Just FCM.PAYOUT_FAILED
  DriverPush_IssueStatus _ -> Just FCM.CHAT_MESSAGE
  DriverPush_IssueChat_Title -> Just FCM.CHAT_MESSAGE
  DriverPush_IssueChat_ImageFallback -> Just FCM.CHAT_MESSAGE
  DriverPush_IssueChat_MessageFallback -> Just FCM.CHAT_MESSAGE
  DriverPush_Ride_Cancelled -> Just FCM.CANCELLED_PRODUCT
  DriverPush_Ride_NewAvailable -> Just FCM.NEW_RIDE_AVAILABLE
  DriverPush_Ride_Started -> Just FCM.TRIP_STARTED
  DriverPush_Ride_StartedAc -> Just FCM.TRIP_STARTED
  DriverPush_Ride_ClearedFare -> Just FCM.CLEARED_FARE
  DriverPush_Ride_SearchCancelled -> Just FCM.CANCELLED_SEARCH_REQUEST
  DriverPush_Ride_StopDetected -> Just FCM.DRIVER_STOP_DETECTED
  DriverPush_AvailableForRidesEnded_RequestLimitReached -> Just FCM.DRIVER_NOTIFY
  DriverPush_AvailableForRidesEnded_ConsecutiveRejections -> Just FCM.DRIVER_NOTIFY
  DriverPush_AvailableForRidesEnded_DriverAtFaultCancellation -> Just FCM.DRIVER_NOTIFY
  DriverPush_Driver_FeedbackBadge -> Just FCM.FEEDBACK_BADGE_PN
  DriverPush_Vehicle_UnhygienicWarning -> Just FCM.UNHYGIENIC_VEHICLE_WARNING
  DriverPush_Vehicle_UnsafeWarning -> Just FCM.VEHICLE_UNSAFE_WARNING
  DriverPush_Safety_SosResolvedSafe -> Just FCM.SOS_RESOLVED
  DriverPush_Safety_LiveTrackingStopped -> Just FCM.SOS_RESOLVED
  _ -> Nothing

-- | The bell-alert pairs the onboarding code can produce. The flag-change actions (approve,
-- reject, enable, disable, block, unblock, approval pending) have a sender, @notifyFlagChanges@,
-- which has no caller; their keys are listed so the rows can be prepared.
reachableOnboardingAlerts :: [(DOAA.OnboardingAlertAction, DAA.DashboardAccessType)]
reachableOnboardingAlerts =
  [ (action, DAA.FLEET_OWNER)
    | action <-
        [ DOAA.UnlinkVehicleAction,
          DOAA.ActivateVehicleAction,
          DOAA.DeactivateVehicleAction,
          DOAA.UnlinkFromFleetAction,
          DOAA.ActivateToFleetAction,
          DOAA.DeactivateFromFleetAction
        ]
  ]
    <> [ (action, audience)
         | action <-
             [ DOAA.DeleteAction,
               DOAA.ChangeFleetOwnerAction,
               DOAA.DocumentApprovalPendingAction,
               DOAA.RejectAction,
               DOAA.ApproveAction,
               DOAA.EnableAction,
               DOAA.DisableAction,
               DOAA.BlockAction,
               DOAA.UnblockAction
             ],
           audience <- [DAA.DASHBOARD_ADMIN, DAA.FLEET_OWNER]
       ]

-- | The action's part of the legacy @ONBOARDING_ALERT_<action>_<audience>@ row key.
onboardingAlertActionText :: DOAA.OnboardingAlertAction -> Text
onboardingAlertActionText = \case
  DOAA.LinkVehicleAction -> "LINK_VEHICLE"
  DOAA.UnlinkVehicleAction -> "UNLINK_VEHICLE"
  DOAA.ActivateVehicleAction -> "ACTIVATE_VEHICLE"
  DOAA.DeactivateVehicleAction -> "DEACTIVATE_VEHICLE"
  DOAA.LinkToOperatorAction -> "LINK_TO_OPERATOR"
  DOAA.UnlinkFromOperatorAction -> "UNLINK_FROM_OPERATOR"
  DOAA.UnlinkFromFleetAction -> "UNLINK_FROM_FLEET"
  DOAA.AddAction -> "ADD"
  DOAA.DeleteAction -> "DELETE"
  DOAA.EnableAction -> "ENABLE"
  DOAA.DisableAction -> "DISABLE"
  DOAA.BlockAction -> "BLOCK"
  DOAA.UnblockAction -> "UNBLOCK"
  DOAA.ApproveAction -> "APPROVE"
  DOAA.RejectAction -> "REJECT"
  DOAA.SetOnboardingAsAction -> "SET_ONBOARDING_AS"
  DOAA.LinkToFleetAction -> "LINK_TO_FLEET"
  DOAA.ActivateToFleetAction -> "ACTIVATE_TO_FLEET"
  DOAA.DeactivateFromFleetAction -> "DEACTIVATE_FROM_FLEET"
  DOAA.ViewAction -> "VIEW"
  DOAA.ChangeFleetOwnerAction -> "CHANGE_FLEET_OWNER"
  DOAA.ExpireAction -> "EXPIRE"
  DOAA.UnlinkDocumentAction -> "UNLINK_DOCUMENT"
  DOAA.OnboardingFlagMutationAction -> "ONBOARDING_FLAG_MUTATION"
  DOAA.DocumentApprovalPendingAction -> "DOCUMENT_APPROVAL_PENDING"

-- | The request type's part of the legacy @CONFIG_ALERT_<type>_<audience>@ row key.
configAlertRequestText :: DAlertType.AlertRequestType -> Text
configAlertRequestText = \case
  DAlertType.FareConfigUpdate -> "FARE_CONFIG_UPDATE"
  DAlertType.OperatingCityCreate -> "OPERATING_CITY_CREATE"
  other -> keyName other

-- | Text stored in @merchant_message@. The row's key column stays the 'DMM.MessageKey' enum;
-- this is the key's name in the registry and on the dashboard.
newtype SmsKey = Sms_Body DMM.MessageKey
  deriving stock (Eq)

instance Show SmsKey where
  show (Sms_Body k) = dotted ["Sms", "Body", keyName k]

instance IsTranslationKey SmsKey where
  allKeys = map Sms_Body [minBound .. maxBound]
  legacyKeyName (Sms_Body k) = Just (keyName k)
