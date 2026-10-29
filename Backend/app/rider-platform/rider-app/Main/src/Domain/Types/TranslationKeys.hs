{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The rider platform's code-owned text keys, one type per table that holds the text.
-- See "Domain.Types.TranslationKey" for the stored form and the legacy-name rule.
module Domain.Types.TranslationKeys
  ( RiderTranslationKey (..),
    RiderPushKey (..),
    SmsKey (..),
    pushNotificationType,
  )
where

import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.ServiceTierType as DST
import Domain.Types.TranslationKey
import qualified IssueManagement.Common as IC
import qualified Kernel.External.Notification.Interface.Types as Notification
import Kernel.Prelude
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Text.Show

-- | Text stored in the rider @translations@ table.
data RiderTranslationKey
  = ServiceTier_Name DST.ServiceTierType
  | ServiceTier_ShortDescription DST.ServiceTierType
  deriving stock (Eq, Generic)

instance Show RiderTranslationKey where
  show = \case
    ServiceTier_Name t -> dotted ["ServiceTier", "Name", keyName t]
    ServiceTier_ShortDescription t -> dotted ["ServiceTier", "ShortDescription", keyName t]

instance IsTranslationKey RiderTranslationKey where
  allKeys =
    concat
      [ map ServiceTier_Name serviceTiers,
        map ServiceTier_ShortDescription serviceTiers
      ]
    where
      serviceTiers = [minBound .. maxBound]

-- | Text stored in the rider @merchant_push_notification@ table.
data RiderPushKey
  = RiderPush_Booking_CancelledWithRide
  | RiderPush_Booking_CancelledNoRide
  | RiderPush_Booking_Reallocated
  | RiderPush_Booking_EstimateReallocated
  | RiderPush_Trip_Started
  | RiderPush_Trip_Finished
  | RiderPush_Refund_RequestApproved
  | RiderPush_Refund_RequestRejected
  | RiderPush_Refund_Failed
  | RiderPush_Refund_Succeeded
  | RiderPush_IssueStatus IC.IssueStatus
  | RiderPush_IssueChat_Title
  | RiderPush_IssueChat_ImageFallback
  | RiderPush_IssueChat_MessageFallback
  | -- Search and quotes
    RiderPush_Search_Expired
  | RiderPush_Quote_Received
  | RiderPush_Quote_DriverIncoming
  | -- Driver and trip progress
    RiderPush_Driver_Assigned
  | RiderPush_Driver_BetterAssigned
  | RiderPush_Driver_OnTheWay
  | RiderPush_Driver_Reaching
  | RiderPush_Driver_Reached
  | RiderPush_Driver_ReachedOtpLess
  | RiderPush_Driver_ReachedDestination
  | RiderPush_Driver_ReachingDestination
  | RiderPush_Driver_StartedReturnTrip
  | RiderPush_Trip_Updated
  | RiderPush_Ride_OtpConfirmed
  | RiderPush_Ride_ServiceTierChanged
  | RiderPush_Ride_StopReached
  | RiderPush_Ride_TollCrossed
  | RiderPush_Ride_TollChargeApprovalRequired
  | RiderPush_Ride_MissedCall
  | RiderPush_Ride_EndOffer
  | RiderPush_Ride_FirstRide
  | RiderPush_Ride_EkdLiveCallFeedback
  | RiderPush_Parcel_ImageUploaded
  | -- Account
    RiderPush_Account_RegistrationApproved
  | RiderPush_Account_Deleted
  | RiderPush_CancellationRate_NudgeDaily
  | RiderPush_CancellationRate_NudgeWeekly
  | RiderPush_CancellationRate_NudgeMonthly
  | -- Safety (sent to the rider's emergency contacts)
    RiderPush_Safety_AlertDeviation
  | RiderPush_Safety_AlertRideStoppage
  | RiderPush_Safety_FollowRide
  | RiderPush_Safety_SosAlert
  | RiderPush_Safety_SosResolved
  | RiderPush_Safety_SosResolvedSafe
  | RiderPush_Safety_LiveTrackingStarted
  | RiderPush_Safety_LiveTrackingStopped
  | -- Rewards and referrals
    RiderPush_Reward_Unlock
  | RiderPush_OfferCashback_Initiated
  | RiderPush_OfferCashback_Completed
  | RiderPush_OfferCashback_Failed
  | RiderPush_Referral_BonusEarned
  | RiderPush_Referral_RewardAddVpa
  | RiderPush_Referral_ReferredByRewardAddVpa
  | RiderPush_Driver_Birthday
  | -- Public transport
    RiderPush_Bus_TripStarted
  | RiderPush_Bus_TripEnded
  | RiderPush_Bus_ApproachingNearing
  | RiderPush_Bus_ApproachingRelaxed
  | RiderPush_Bus_PrevStopCrossed
  | RiderPush_Frfs_TripDetailsUpdated
  | RiderPush_Shuttle_TrackingOnStart
  | RiderPush_Ticket_Cancelled
  | RiderPush_Pass_ExpiryReminder
  | -- Shared cab
    RiderPush_SharedCab_Assigned
  | RiderPush_SharedCab_Arriving
  | RiderPush_SharedCab_Reassigned
  | RiderPush_SharedCab_BoardAny
  | RiderPush_SharedCab_RouteChange
  | RiderPush_SharedCab_BookingCancelled
  | RiderPush_SharedCab_FindingTimeout
  | RiderPush_SharedCab_OnTheWay
  | RiderPush_SharedCab_ArrivingSoon
  | -- Payment order status, one key per service type and outcome
    RiderPush_Payment DOrder.PaymentServiceType Notification.Category
  deriving stock (Eq, Generic)

instance Show RiderPushKey where
  show = \case
    RiderPush_IssueStatus status -> dotted ["RiderPush", "IssueStatus", keyName status]
    RiderPush_Payment serviceType category -> dotted ["RiderPush", "Payment", keyName serviceType, keyName category]
    k -> dottedConstructorName k

instance IsTranslationKey RiderPushKey where
  allKeys =
    [ RiderPush_Booking_CancelledWithRide,
      RiderPush_Booking_CancelledNoRide,
      RiderPush_Booking_Reallocated,
      RiderPush_Booking_EstimateReallocated,
      RiderPush_Trip_Started,
      RiderPush_Trip_Finished,
      RiderPush_Refund_RequestApproved,
      RiderPush_Refund_RequestRejected,
      RiderPush_Refund_Failed,
      RiderPush_Refund_Succeeded
    ]
      <> map RiderPush_IssueStatus [minBound .. maxBound]
      <> [ RiderPush_IssueChat_Title,
           RiderPush_IssueChat_ImageFallback,
           RiderPush_IssueChat_MessageFallback,
           RiderPush_Search_Expired,
           RiderPush_Quote_Received,
           RiderPush_Quote_DriverIncoming,
           RiderPush_Driver_Assigned,
           RiderPush_Driver_BetterAssigned,
           RiderPush_Driver_OnTheWay,
           RiderPush_Driver_Reaching,
           RiderPush_Driver_Reached,
           RiderPush_Driver_ReachedOtpLess,
           RiderPush_Driver_ReachedDestination,
           RiderPush_Driver_ReachingDestination,
           RiderPush_Driver_StartedReturnTrip,
           RiderPush_Trip_Updated,
           RiderPush_Ride_OtpConfirmed,
           RiderPush_Ride_ServiceTierChanged,
           RiderPush_Ride_StopReached,
           RiderPush_Ride_TollCrossed,
           RiderPush_Ride_TollChargeApprovalRequired,
           RiderPush_Ride_MissedCall,
           RiderPush_Ride_EndOffer,
           RiderPush_Ride_FirstRide,
           RiderPush_Ride_EkdLiveCallFeedback,
           RiderPush_Parcel_ImageUploaded,
           RiderPush_Account_RegistrationApproved,
           RiderPush_Account_Deleted,
           RiderPush_CancellationRate_NudgeDaily,
           RiderPush_CancellationRate_NudgeWeekly,
           RiderPush_CancellationRate_NudgeMonthly,
           RiderPush_Safety_AlertDeviation,
           RiderPush_Safety_AlertRideStoppage,
           RiderPush_Safety_FollowRide,
           RiderPush_Safety_SosAlert,
           RiderPush_Safety_SosResolved,
           RiderPush_Safety_SosResolvedSafe,
           RiderPush_Safety_LiveTrackingStarted,
           RiderPush_Safety_LiveTrackingStopped,
           RiderPush_Reward_Unlock,
           RiderPush_OfferCashback_Initiated,
           RiderPush_OfferCashback_Completed,
           RiderPush_OfferCashback_Failed,
           RiderPush_Referral_BonusEarned,
           RiderPush_Referral_RewardAddVpa,
           RiderPush_Referral_ReferredByRewardAddVpa,
           RiderPush_Driver_Birthday,
           RiderPush_Bus_TripStarted,
           RiderPush_Bus_TripEnded,
           RiderPush_Bus_ApproachingNearing,
           RiderPush_Bus_ApproachingRelaxed,
           RiderPush_Bus_PrevStopCrossed,
           RiderPush_Frfs_TripDetailsUpdated,
           RiderPush_Shuttle_TrackingOnStart,
           RiderPush_Ticket_Cancelled,
           RiderPush_Pass_ExpiryReminder,
           RiderPush_SharedCab_Assigned,
           RiderPush_SharedCab_Arriving,
           RiderPush_SharedCab_Reassigned,
           RiderPush_SharedCab_BoardAny,
           RiderPush_SharedCab_RouteChange,
           RiderPush_SharedCab_BookingCancelled,
           RiderPush_SharedCab_FindingTimeout,
           RiderPush_SharedCab_OnTheWay,
           RiderPush_SharedCab_ArrivingSoon
         ]
      <> [ RiderPush_Payment serviceType category
           | serviceType <- [minBound .. maxBound],
             category <- paymentCategories
         ]
    where
      paymentCategories =
        [ Notification.REFUND_PENDING,
          Notification.REFUND_FAILED,
          Notification.REFUND_SUCCESS,
          Notification.FULFILLMENT_PENDING,
          Notification.FULFILLMENT_SUCCESS
        ]

  legacyKeyName = \case
    RiderPush_Booking_CancelledWithRide -> Just "BOOKING_CANCEL_WITH_RIDE"
    RiderPush_Booking_CancelledNoRide -> Just "BOOKING_CANCEL_WITH_NO_RIDE"
    RiderPush_Booking_Reallocated -> Just "BOOKING_REALLOCATED"
    RiderPush_Booking_EstimateReallocated -> Just "EST_OR_QUOTE_REALLOCATED"
    RiderPush_Trip_Started -> Just "TRIP_STARTED"
    RiderPush_Trip_Finished -> Just "TRIP_FINISHED"
    RiderPush_Refund_RequestApproved -> Just "REFUND_REQUEST_APPROVED"
    RiderPush_Refund_RequestRejected -> Just "REFUND_REQUEST_REJECTED"
    RiderPush_Refund_Failed -> Just "REFUND_FAILED"
    RiderPush_Refund_Succeeded -> Just "REFUND_SUCCESS"
    RiderPush_IssueStatus _ -> Nothing
    RiderPush_IssueChat_Title -> Nothing
    RiderPush_IssueChat_ImageFallback -> Nothing
    RiderPush_IssueChat_MessageFallback -> Nothing
    RiderPush_Search_Expired -> Just "EXPIRED_CASE"
    RiderPush_Quote_Received -> Just "QUOTE_RECEIVED"
    RiderPush_Quote_DriverIncoming -> Just "DRIVER_QUOTE_INCOMING"
    RiderPush_Driver_Assigned -> Just "DRIVER_ASSIGNMENT"
    RiderPush_Driver_BetterAssigned -> Just "BETTER_DRIVER_ASSIGNED"
    RiderPush_Driver_OnTheWay -> Just "DRIVER_ON_THE_WAY"
    RiderPush_Driver_Reaching -> Just "DRIVER_REACHING"
    RiderPush_Driver_Reached -> Just "DRIVER_HAS_REACHED"
    RiderPush_Driver_ReachedOtpLess -> Just "DRIVER_HAS_REACHED_OTP_LESS"
    RiderPush_Driver_ReachedDestination -> Just "DRIVER_HAS_REACHED_DESTINATION"
    RiderPush_Driver_ReachingDestination -> Just "DRIVER_REACHING_DESTINATION"
    RiderPush_Driver_StartedReturnTrip -> Just "DRIVER_STARTED_RETURN_TRIP"
    RiderPush_Trip_Updated -> Just "TRIP_UPDATED"
    RiderPush_Ride_OtpConfirmed -> Just "OTP_RIDE_CONFIRMED"
    RiderPush_Ride_ServiceTierChanged -> Just "SERVICE_TIER_CHANGED"
    RiderPush_Ride_StopReached -> Just "STOP_REACHED"
    RiderPush_Ride_TollCrossed -> Just "TOLL_CROSSED"
    RiderPush_Ride_TollChargeApprovalRequired -> Just "TOLL_CHARGE_APPROVAL_REQUIRED"
    RiderPush_Ride_MissedCall -> Just "FCM_CHAT_MESSAGE"
    RiderPush_Ride_EndOffer -> Just "RIDE_END_OFFER"
    RiderPush_Ride_FirstRide -> Just "FIRST_RIDE_EVENT"
    RiderPush_Ride_EkdLiveCallFeedback -> Just "EKD_LIVE_CALL_FEEDBACK"
    RiderPush_Parcel_ImageUploaded -> Just "PARCEL_IMAGE_UPLOADED"
    RiderPush_Account_RegistrationApproved -> Just "REGISTRATION_APPROVED"
    RiderPush_Account_Deleted -> Just "ACCOUNT_DELETED"
    RiderPush_CancellationRate_NudgeDaily -> Just "CUSTOMER_CANCELLATION_RATE_NUDGE_DAILY"
    RiderPush_CancellationRate_NudgeWeekly -> Just "CUSTOMER_CANCELLATION_RATE_NUDGE_WEEKLY"
    RiderPush_CancellationRate_NudgeMonthly -> Just "CUSTOMER_CANCELLATION_RATE_NUDGE_MONTHLY"
    RiderPush_Safety_AlertDeviation -> Just "SAFETY_ALERT_DEVIATION"
    RiderPush_Safety_AlertRideStoppage -> Just "SAFETY_ALERT_RIDE_STOPPAGE"
    RiderPush_Safety_FollowRide -> Just "FOLLOW_RIDE"
    RiderPush_Safety_SosAlert -> Just "SOS_ALERT"
    RiderPush_Safety_SosResolved -> Just "SOS_RESOLVED"
    RiderPush_Safety_SosResolvedSafe -> Just "SOS_RESOLVED_SAFE"
    RiderPush_Safety_LiveTrackingStarted -> Just "LIVE_TRACKING_STARTED"
    RiderPush_Safety_LiveTrackingStopped -> Just "LIVE_TRACKING_STOPPED"
    RiderPush_Reward_Unlock -> Just "REWARD_UNLOCK"
    RiderPush_OfferCashback_Initiated -> Just "OFFER_CASHBACK_INITIATED"
    RiderPush_OfferCashback_Completed -> Just "OFFER_CASHBACK_COMPLETED"
    RiderPush_OfferCashback_Failed -> Just "OFFER_CASHBACK_FAILED"
    RiderPush_Referral_BonusEarned -> Just "REFERRAL_BONUS_EARNED"
    RiderPush_Referral_RewardAddVpa -> Just "REFERRAL_REWARD_ADD_VPA"
    RiderPush_Referral_ReferredByRewardAddVpa -> Just "REFERRED_BY_REWARD_ADD_VPA"
    RiderPush_Driver_Birthday -> Just "DRIVER_BIRTHDAY"
    RiderPush_Bus_TripStarted -> Just "BUS_TRIP_STARTED"
    RiderPush_Bus_TripEnded -> Just "BUS_TRIP_ENDED"
    RiderPush_Bus_ApproachingNearing -> Just "BUS_APPROACHING_NEARING"
    RiderPush_Bus_ApproachingRelaxed -> Just "BUS_APPROACHING_RELAXED"
    RiderPush_Bus_PrevStopCrossed -> Just "BUS_PREV_STOP_CROSSED"
    RiderPush_Frfs_TripDetailsUpdated -> Just "FRFS_TRIP_DETAILS_UPDATED"
    RiderPush_Shuttle_TrackingOnStart -> Just "SHUTTLE_TRACKING_ON_START"
    RiderPush_Ticket_Cancelled -> Just "TICKET_CANCELLED"
    RiderPush_Pass_ExpiryReminder -> Just "PASS_EXPIRY_REMINDER"
    RiderPush_SharedCab_Assigned -> Just "SHARED_CAB_ASSIGNED"
    RiderPush_SharedCab_Arriving -> Just "SHARED_CAB_ARRIVING"
    RiderPush_SharedCab_Reassigned -> Just "SHARED_CAB_REASSIGNED"
    RiderPush_SharedCab_BoardAny -> Just "SHARED_CAB_BOARD_ANY"
    RiderPush_SharedCab_RouteChange -> Just "SHARED_CAB_ROUTE_CHANGE"
    RiderPush_SharedCab_BookingCancelled -> Just "SHARED_CAB_BOOKING_CANCELLED"
    RiderPush_SharedCab_FindingTimeout -> Just "SHARED_CAB_FINDING_TIMEOUT"
    RiderPush_SharedCab_OnTheWay -> Just "SHARED_CAB_ON_THE_WAY"
    RiderPush_SharedCab_ArrivingSoon -> Just "SHARED_CAB_ARRIVING_SOON"
    RiderPush_Payment serviceType category -> Just (keyName serviceType <> "_" <> keyName category)

-- | The notification type a key's first row gets when the dashboard creates it: the one the
-- sender passes in code, so the row only carries the texts. Nothing for keys whose sender takes
-- the type from the row; their first row keeps coming from SQL or ConfigPilot.
pushNotificationType :: RiderPushKey -> Maybe Notification.Category
pushNotificationType = \case
  RiderPush_IssueStatus _ -> Just Notification.CHAT_MESSAGE
  RiderPush_IssueChat_Title -> Just Notification.CHAT_MESSAGE
  RiderPush_IssueChat_ImageFallback -> Just Notification.CHAT_MESSAGE
  RiderPush_IssueChat_MessageFallback -> Just Notification.CHAT_MESSAGE
  RiderPush_Trip_Updated -> Just Notification.TRIP_UPDATED
  RiderPush_Safety_SosResolved -> Just Notification.SOS_RESOLVED
  RiderPush_Safety_SosResolvedSafe -> Just Notification.SOS_RESOLVED
  RiderPush_Safety_LiveTrackingStarted -> Just Notification.SHARE_RIDE
  RiderPush_Safety_LiveTrackingStopped -> Just Notification.SOS_RESOLVED
  RiderPush_Payment _ category -> Just category
  _ -> Nothing

-- | Text stored in @merchant_message@. The row's key column stays the 'DMM.MessageKey' enum;
-- this is the key's name in the registry and on the dashboard.
newtype SmsKey = Sms_Body DMM.MessageKey
  deriving stock (Eq)

instance Show SmsKey where
  show (Sms_Body k) = dotted ["Sms", "Body", keyName k]

instance IsTranslationKey SmsKey where
  allKeys = map Sms_Body [minBound .. maxBound]
  legacyKeyName (Sms_Body k) = Just (keyName k)
