module Resources.LocalizableV2.Strings where

import Prelude hiding(apply)
import Resources.LocalizableV2.Types 
import Language.Types
import Type.Proxy (Proxy(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Record.Unsafe (unsafeGet)

import Resources.LocalizableV2.HI
import Resources.LocalizableV2.KN
import Resources.LocalizableV2.ML
import Resources.LocalizableV2.TA
import Resources.LocalizableV2.TE
import Resources.LocalizableV2.BN
import Resources.LocalizableV2.EN
import Locale.Utils

stringsMap :: Languages
stringsMap =
  Languages
    { english: getEn
    , hindi: getHi
    , malayalam: getMl
    , bengali: getBn
    , tamil: getTa
    , kannada: getKn
    , telugu: getTe
    }

infixl 1 readFromNT as @~

readFromNT ::
  forall l a r r_ t.
  Cons l a r_ r =>
  IsSymbol l =>
  Lacks l r_ =>
  Newtype t (Record r) =>
  t -> (Proxy l) -> a
readFromNT a label = unsafeGet (reflectSymbol label) $ unwrap a

getStringV2 ::
  forall l r_ r.
  Cons l String r_ r =>
  IsSymbol l =>
  Lacks l r_ =>
  Newtype Keymap (Record r) =>
  Proxy l -> String
getStringV2 key =
  let
    language = getLanguageLocale languageKey
  in
    case language of
      "BN_IN" -> stringsMap @~ bengali @~ key
      "HI_IN" -> stringsMap @~ hindi @~ key
      "KN_IN" -> stringsMap @~ kannada @~ key
      "ML_IN" -> stringsMap @~ malayalam @~ key
      "TA_IN" -> stringsMap @~ tamil @~ key
      "TE_IN" -> stringsMap @~ telugu @~ key
      _ -> stringsMap @~ english @~ key

getString :: String -> STR -> String
getString language str =
  let
    proxy = getProxy str

    langMap = case language of
      "BN_IN" -> stringsMap @~ bengali
      "HI_IN" -> stringsMap @~ hindi
      "KN_IN" -> stringsMap @~ kannada
      "ML_IN" -> stringsMap @~ malayalam
      "TA_IN" -> stringsMap @~ tamil
      "TE_IN" -> stringsMap @~ telugu
      _ -> stringsMap @~ english
  in
    proxy $ langMap

getEN :: STR -> String
getEN = getString "EN_IN"

getHI :: STR -> String
getHI = getString "HI_IN"

getKN :: STR -> String
getKN = getString "KN_IN"

getML :: STR -> String
getML = getString "ML_IN"

getTA :: STR -> String
getTA = getString "TA_IN"

getTE :: STR -> String
getTE = getString "TE_IN"

getBN :: STR -> String
getBN = getString "BN_IN"

getProxy :: STR -> (Keymap -> String)
getProxy str =
  case str of
    ABOUT -> \a -> a @~ about
    SAFETY -> \a -> a @~ safety
    SAFETY_CHECK_IN -> \a -> a @~ safety_check_in
    ABOUT_APP_DESCRIPTION -> \a -> a @~ about_app_description
    ABOUT_REFERRAL_PROGRAM -> \a -> a @~ about_referral_program
    ABOUT_REFERRAL_PROGRAM_DISCRIPTION arg1 -> \a -> (a @~ about_referral_program_discription) arg1
    ACCOUNT_DELETION_CONFIRMATION -> \a -> a @~ account_deletion_confirmation
    ADD_ANOTHER_CONTACT -> \a -> a @~ add_another_contact
    ADD_EMERGENCY_CONTACTS -> \a -> a @~ add_emergency_contacts
    ADD_FAVOURITE -> \a -> a @~ add_favourite
    ADD_NEW_ADDRESS -> \a -> a @~ add_new_address
    ADD_NEW_FAVOURITE -> \a -> a @~ add_new_favourite
    ADD_NOW -> \a -> a @~ add_now
    ADD_SAVED_LOCATION_FROM_SETTINGS -> \a -> a @~ add_saved_location_from_settings
    ADD_TAG -> \a -> a @~ add_tag
    ADDRESS -> \a -> a @~ address
    ADDRESSES -> \a -> a @~ addresses
    ALL_FAVOURITES -> \a -> a @~ all_favourites
    ALL_TOPICS -> \a -> a @~ all_topics
    ALREADY_EXISTS -> \a -> a @~ already_exists
    ALSO_SHARE_YOUR_RIDE_STATUS_AND_LOCATION -> \a -> a @~ also_share_your_ride_status_and_location
    AMOUNT_PAID -> \a -> a @~ amount_paid
    ANONYMOUS_CALL -> \a -> a @~ anonymous_call
    ARE_YOU_STARING -> \a -> a @~ are_you_staring
    ARE_YOU_SURE_YOU_WANT_TO_CANCEL -> \a -> a @~ are_you_sure_you_want_to_cancel
    ARE_YOU_SURE_YOU_WANT_TO_LOGOUT -> \a -> a @~ are_you_sure_you_want_to_logout
    ARE_YOU_SURE_YOU_WANT_TO_REMOVE_CONTACT -> \a -> a @~ are_you_sure_you_want_to_remove_contact
    ARE_YOU_SURE_YOU_WANT_TO_REMOVE_FAVOURITE_ -> \a -> a @~ are_you_sure_you_want_to_remove_favourite_
    ASK_FOR_PRICE -> \a -> a @~ ask_for_price
    ASK_FOR_PRICE_INFO -> \a -> a @~ ask_for_price_info
    ASKED_FOR_MORE_MONEY -> \a -> a @~ asked_for_more_money
    AT_DROP -> \a -> a @~ at_drop
    AT_PICKUP -> \a -> a @~ at_pickup
    AUTO_ACCEPTING_SELECTED_RIDE -> \a -> a @~ auto_accepting_selected_ride
    AUTO_ASSIGN_A_RIDE -> \a -> a @~ auto_assign_a_ride
    AUTO_ASSIGN_DRIVER -> \a -> a @~ auto_assign_driver
    AWAY -> \a -> a @~ away
    AWAY_C -> \a -> a @~ away_c
    BASE_FARES -> \a -> a @~ base_fares
    BOARD_THE_FIRST -> \a -> a @~ board_the_first
    BOOK_NOW -> \a -> a @~ book_now
    BOOK_RIDE_ -> \a -> a @~ book_ride_
    BOOKING_PREFERENCE -> \a -> a @~ booking_preference
    BOOST_YOUR_RIDE_CHANCES_AND_HELP_DRIVERS_WITH_TIPS -> \a -> a @~ boost_your_ride_chances_and_help_drivers_with_tips
    BY_CASH -> \a -> a @~ by_cash
    BY_TAPPING_CONTINUE -> \a -> a @~ by_tapping_continue
    CALL -> \a -> a @~ call
    CALL_DRIVER -> \a -> a @~ call_driver
    CALL_DRIVER_USING -> \a -> a @~ call_driver_using
    CALL_EMERGENCY_CONTACTS -> \a -> a @~ call_emergency_contacts
    CALL_EMERGENCY_CENTRE -> \a -> a @~ call_emergency_centre
    CANCEL_ -> \a -> a @~ cancel_
    CANCEL_AUTO_ASSIGNING -> \a -> a @~ cancel_auto_assigning
    CANCEL_ONGOING_SEARCH -> \a -> a @~ cancel_ongoing_search
    CANCEL_RIDE -> \a -> a @~ cancel_ride
    REQUEST_EDIT -> \a -> a @~ request_edit
    CANCEL_SEARCH -> \a -> a @~ cancel_search
    CANCEL_STR -> \a -> a @~ cancel_str
    CANCELLED -> \a -> a @~ cancelled
    CHANGE -> \a -> a @~ change
    CHANGE_DROP_LOCATION -> \a -> a @~ change_drop_location
    CHANGE_LOCATION -> \a -> a @~ change_location
    CHECK_OUT_LIVE_STATS -> \a -> a @~ check_out_live_stats
    CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN -> \a -> a @~ check_your_internet_connection_and_try_again
    CHOOSE_A_RIDE_AS_PER_YOUR_COMFORT -> \a -> a @~ choose_a_ride_as_per_your_comfort
    CHOOSE_BETWEEN_MULTIPLE_DRIVERS -> \a -> a @~ choose_between_multiple_drivers
    CHOOSE_BETWEEN_MULTIPLE_RIDES -> \a -> a @~ choose_between_multiple_rides
    CHOOSE_ON_MAP -> \a -> a @~ choose_on_map
    CHOOSE_YOUR_RIDE -> \a -> a @~ choose_your_ride
    COMFY -> \a -> a @~ comfy
    CONFIRM_AND_BOOK -> \a -> a @~ confirm_and_book
    CONFIRM_AND_SAVE -> \a -> a @~ confirm_and_save
    CONFIRM_CHANGES -> \a -> a @~ confirm_changes
    CONFIRM_DROP_LOCATION -> \a -> a @~ confirm_drop_location
    CHECK_REVISED_FARE_AND_ROUTE -> \a -> a @~ check_revised_fare_and_route
    CONFIRM_EMERGENCY_CONTACTS -> \a -> a @~ confirm_emergency_contacts
    CONFIRM_FOR -> \a -> a @~ confirm_for
    CONFIRM_LOCATION -> \a -> a @~ confirm_location
    CONFIRM_PICKUP_LOCATION -> \a -> a @~ confirm_pickup_location
    CONFIRM_RIDE_ -> \a -> a @~ confirm_ride_
    CONFIRMING_THE_RIDE_FOR_YOU -> \a -> a @~ confirming_the_ride_for_you
    CONTACT_SUPPORT -> \a -> a @~ contact_support
    CONTACTS_SELECTED -> \a -> a @~ contacts_selected
    CONTINUE -> \a -> a @~ continue
    COPIED -> \a -> a @~ copied
    COULD_NOT_CONNECT_TO_DRIVER -> \a -> a @~ could_not_connect_to_driver
    CURRENT_LOCATION -> \a -> a @~ current_location
    FARE_UPDATED -> \a -> a @~ fare_updated
    PLEASE_CONFIRM_WITH_YOUR_AFTER_REQUESTING -> \a -> a @~ please_confirm_with_your_after_requesting
    PREVIOUSLY -> \a -> a @~ previously
    ROUTE_AND_FARE_UPDATED -> \a -> a @~ route_and_fare_updated
    PREVIOUS_FARE -> \a -> a @~ previous_fare
    YOUR_DRIVER_MIGHT_WANT_TO_GO_TOWARDS_THE_CURRENT_DROP_KINDLY_ASK_THEM_TO_CONFIRM_AFTER_REQUESTING -> \a -> a @~ your_driver_might_want_to_go_towards_the_current_drop_kindly_ask_them_to_confirm_after_requesting
    CURRENTLY_WE_ARE_LIVE_IN_ arg1 -> \a -> (a @~ currently_we_are_live_in_) arg1
    CUSTOMER_SELECTED_FARE -> \a -> a @~ customer_selected_fare
    CUSTOMER_TIP_DESCRIPTION -> \a -> a @~ customer_tip_description
    DIAL_112 -> \a -> a @~ dial_112
    DETAILS -> \a -> a @~ details
    DATA_COLLECTION_AUTHORITY -> \a -> a @~ data_collection_authority
    DAY_TIME_CHARGES arg1 arg2 -> \a -> (a @~ day_time_charges) arg1 arg2
    DAYTIME_CHARGES_APPLICABLE_AT_NIGHT arg1 arg2 -> \a -> (a @~ daytime_charges_applicable_at_night) arg1 arg2
    DAYTIME_CHARGES_APPLIED_AT_NIGHT arg1 arg2 arg3 -> \a -> (a @~ daytime_charges_applied_at_night) arg1 arg2 arg3
    DEL_ACCOUNT -> \a -> a @~ del_account
    DELETE -> \a -> a @~ delete
    DENY_ACCESS -> \a -> a @~ deny_access
    DESCRIBE_YOUR_ISSUE -> \a -> a @~ describe_your_issue
    DESTINATION_OUTSIDE_LIMITS -> \a -> a @~ destination_outside_limits
    DIRECT_CALL -> \a -> a @~ direct_call
    DO_YOU_NEED_EMERGENCY_HELP -> \a -> a @~ do_you_need_emergency_help
    DOWNLOAD_PDF -> \a -> a @~ download_pdf
    DRAG_THE_MAP -> \a -> a @~ drag_the_map
    DRIVER_PICKUP_CHARGES arg1 -> \a -> (a @~ driver_pickup_charges) arg1
    DRIVER_REQUESTED_TO_CANCEL -> \a -> a @~ driver_requested_to_cancel
    DRIVER_WAS_NOT_REACHABLE -> \a -> a @~ driver_was_not_reachable
    DRIVER_WAS_RUDE -> \a -> a @~ driver_was_rude
    DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO -> \a -> a @~ drivers_can_charge_an_additional_fare_upto
    DRIVERS_CAN_CHARGE_BETWEEN_THE_ABOVE_RANGE -> \a -> a @~ drivers_can_charge_between_the_above_range
    DRIVERS_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC -> \a -> a @~ drivers_may_quote_extra_to_cover_for_traffic
    DRIVER_ADDITION_LIMITS_ARE_IN_INCREMENTS -> \a -> a @~ driver_addition_limits_are_in_increments
    DROP -> \a -> a @~ drop
    DROP_LOCATION_FAR_AWAY -> \a -> a @~ drop_location_far_away
    EARLY_END_RIDE_CHARGES -> \a -> a @~ early_end_ride_charges
    EARLY_END_RIDE_CHARGES_DESCRIPTION -> \a -> a @~ early_end_ride_charges_description
    ECONOMICAL -> \a -> a @~ economical
    EDIT -> \a -> a @~ edit
    EDIT_FAVOURITE -> \a -> a @~ edit_favourite
    EMAIL -> \a -> a @~ email
    EMAIL_ALREADY_EXISTS -> \a -> a @~ email_already_exists
    EMAIL_ID -> \a -> a @~ email_id
    TRUSTED_CONTACS_ADDED_SUCCESSFULLY -> \a -> a @~ trusted_contacs_added_successfully
    EMERGENCY_CONTACTS -> \a -> a @~ emergency_contacts
    EDIT_EMERGENCY_CONTACTS -> \a -> a @~ edit_emergency_contacts
    EMERGENCY_HELP -> \a -> a @~ emergency_help
    EMPTY_RIDES -> \a -> a @~ empty_rides
    ENABLE_THIS_FEATURE_TO_CHOOSE_YOUR_RIDE -> \a -> a @~ enable_this_feature_to_choose_your_ride
    ENJOY_RIDING_WITH_US -> \a -> a @~ enjoy_riding_with_us
    ENTER_4_DIGIT_OTP -> \a -> a @~ enter_4_digit_otp
    ENTER_A_LOCATION -> \a -> a @~ enter_a_location
    ENTER_MOBILE_NUMBER -> \a -> a @~ enter_mobile_number
    ENTER_OTP -> \a -> a @~ enter_otp
    ENTER_YOUR_MOBILE_NUMBER -> \a -> a @~ enter_your_mobile_number
    ENTER_YOUR_NAME -> \a -> a @~ enter_your_name
    ERROR_404 -> \a -> a @~ error_404
    ERROR_OCCURED_TRY_AGAIN -> \a -> a @~ error_occured_try_again
    ESTIMATES_CHANGED -> \a -> a @~ estimates_changed
    ESTIMATES_REVISED_TO -> \a -> a @~ estimates_revised_to
    ETA_WAS_TOO_LONG -> \a -> a @~ eta_was_too_long
    ETA_WAS_TOO_SHORT -> \a -> a @~ eta_was_too_short
    EXISTS_AS -> \a -> a @~ exists_as
    EXPIRES_IN -> \a -> a @~ expires_in
    FAQ -> \a -> a @~ faq
    FARE_HAS_BEEN_UPDATED -> \a -> a @~ fare_has_been_updated
    FARE_WAS_HIGH -> \a -> a @~ fare_was_high
    FAVOURITE -> \a -> a @~ favourite
    FAVOURITE_ADDED_SUCCESSFULLY -> \a -> a @~ favourite_added_successfully
    FAVOURITE_LOCATION -> \a -> a @~ favourite_location
    FAVOURITE_REMOVED_SUCCESSFULLY -> \a -> a @~ favourite_removed_successfully
    FAVOURITE_UPDATED_SUCCESSFULLY -> \a -> a @~ favourite_updated_successfully
    FAVOURITE_YOUR_CURRENT_LOCATION -> \a -> a @~ favourite_your_current_location
    FAVOURITES -> \a -> a @~ favourites
    FEMALE -> \a -> a @~ female
    FINDING_RIDES_NEAR_YOU -> \a -> a @~ finding_rides_near_you
    FOR_OTHER_ISSUES_WRITE_TO_US -> \a -> a @~ for_other_issues_write_to_us
    FULL_NAME -> \a -> a @~ full_name
    GENDER_STR -> \a -> a @~ gender_str
    GET_ESTIMATE_FARE -> \a -> a @~ get_estimate_fare
    GETTING_DELAYED_PLEASE_WAIT -> \a -> a @~ getting_delayed_please_wait
    GETTING_ESTIMATES_FOR_YOU -> \a -> a @~ getting_estimates_for_you
    GETTING_REVISED_ESTIMATE -> \a -> a @~ getting_revised_estimate
    GETTING_STARTED_AND_FAQS -> \a -> a @~ getting_started_and_faqs
    GIVE_THIS_LOCATION_A_NAME -> \a -> a @~ give_this_location_a_name
    GO_BACK_ -> \a -> a @~ go_back_
    GO_HOME_ -> \a -> a @~ go_home_
    GO_TO_HOME__ -> \a -> a @~ go_to_home__
    GOOGLE_MAP_ -> \a -> a @~ google_map_
    GOT_ANOTHER_RIDE_ELSE_WHERE -> \a -> a @~ got_another_ride_else_where
    GOT_IT -> \a -> a @~ got_it
    GOT_IT_TELL_US_MORE -> \a -> a @~ got_it_tell_us_more
    GOVERNMENT_CHAGRES -> \a -> a @~ government_chagres
    GRANT_ACCESS -> \a -> a @~ grant_access
    CGST -> \a -> a @~ cgst
    HAVE_REFERRAL_CODE -> \a -> a @~ have_referral_code
    HATCHBACK -> \a -> a @~ hatchback
    HELP_AND_SUPPORT -> \a -> a @~ help_and_support
    HELP_US_WITH_YOUR_FEEDBACK_OPTIONAL -> \a -> a @~ help_us_with_your_feedback_optional
    HELP_US_WITH_YOUR_REASON -> \a -> a @~ help_us_with_your_reason
    HEY -> \a -> a @~ hey
    HOME -> \a -> a @~ home
    HOPE_YOUR_RIDE_WAS_HASSLE_FREE -> \a -> a @~ hope_your_ride_was_hassle_free
    HOW_DO_YOU_IDENTIFY_YOURSELF -> \a -> a @~ how_do_you_identify_yourself
    HOW_SHOULD_WE_ADDRESS_YOU -> \a -> a @~ how_should_we_address_you
    HOW_THE_PRICING_WORKS -> \a -> a @~ how_the_pricing_works
    HOW_THIS_WORKS -> \a -> a @~ how_this_works
    HOW_WAS_YOUR_RIDE_EXPERIENCE -> \a -> a @~ how_was_your_ride_experience
    HOW_WAS_YOUR_RIDE_WITH -> \a -> a @~ how_was_your_ride_with
    ACTUAL_FARE_WAS_HIGHER_THAN_WHAT_WAS_SHOWN -> \a -> a @~ actual_fare_was_higher_than_what_was_shown
    I_AM_ON_MY_WAY -> \a -> a @~ i_am_on_my_way
    I_HAVE_ARRIVED -> \a -> a @~ i_have_arrived
    IF_YOU_STILL_WANNA_BOOK_RIDE_CLICK_CONTINUE_AND_START_BOOKING_THE_RIDE -> \a -> a @~ if_you_still_wanna_book_ride_click_continue_and_start_booking_the_ride
    IN -> \a -> a @~ in'
    IN_APP_TRACKING -> \a -> a @~ in_app_tracking
    INVALID_CODE_PLEASE_RE_ENTER -> \a -> a @~ invalid_code_please_re_enter
    INVALID_MOBILE_NUMBER -> \a -> a @~ invalid_mobile_number
    INVOICE -> \a -> a @~ invoice
    IS_ON_THE_WAY -> \a -> a @~ is_on_the_way
    IS_WAITING_AT_PICKUP -> \a -> a @~ is_waiting_at_pickup
    IT_SEEMS_TO_BE_A_VERY_BUSY_DAY -> \a -> a @~ it_seems_to_be_a_very_busy_day
    LANGUAGE -> \a -> a @~ language
    LET_TRY_THAT_AGAIN -> \a -> a @~ let_try_that_again
    LIVE_STATS_DASHBOARD -> \a -> a @~ live_stats_dashboard
    LOAD_MORE -> \a -> a @~ load_more
    LOADING -> \a -> a @~ loading
    LOCATION -> \a -> a @~ location
    LOCATION_ALREADY -> \a -> a @~ location_already
    LOCATION_ALREADY_EXISTS -> \a -> a @~ location_already_exists
    LOCATION_ALREADY_EXISTS_AS -> \a -> a @~ location_already_exists_as
    LOCATION_UNSERVICEABLE -> \a -> a @~ location_unserviceable
    LOGIN_USING_THE_OTP_SENT_TO -> \a -> a @~ login_using_the_otp_sent_to
    LOGO -> \a -> a @~ logo
    LOGOUT_ -> \a -> a @~ logout_
    LOOKING_FOR_YOU_AT_PICKUP -> \a -> a @~ looking_for_you_at_pickup
    LOST_SOMETHING -> \a -> a @~ lost_something
    MALE -> \a -> a @~ male
    MANDATORY -> \a -> a @~ mandatory
    MAX_CHAR_LIMIT_REACHED -> \a -> a @~ max_char_limit_reached
    MAYBE_LATER -> \a -> a @~ maybe_later
    MESSAGE -> \a -> a @~ message
    METERS_AWAY_FROM_YOUR_DESTINATION -> \a -> a @~ meters_away_from_your_destination
    MIN_FARE_UPTO arg1 -> \a -> (a @~ min_fare_upto) arg1
    MORE_THAN -> \a -> a @~ more_than
    MINS_AWAY -> \a -> a @~ mins_away
    MOBILE -> \a -> a @~ mobile
    MOBILE_NUMBER_STR -> \a -> a @~ mobile_number_str
    MY_RIDES -> \a -> a @~ my_rides
    NAME -> \a -> a @~ name
    NAME_ALREADY_IN_USE -> \a -> a @~ name_already_in_use
    NAVIGATE -> \a -> a @~ navigate
    NEARBY -> \a -> a @~ nearby
    NIGHT_TIME_CHARGES arg1 arg2 -> \a -> (a @~ night_time_charges) arg1 arg2
    NO -> \a -> a @~ no
    NO_CONTACTS_LEFT_ON_DEVICE_TO_ADD -> \a -> a @~ no_contacts_left_on_device_to_add
    NO_DONT -> \a -> a @~ no_dont
    NO_EMERGENCY_CONTACTS_SET -> \a -> a @~ no_emergency_contacts_set
    NO_FAVOURITES_SAVED_YET -> \a -> a @~ no_favourites_saved_yet
    NO_MORE_RIDES -> \a -> a @~ no_more_rides
    NO_TIP -> \a -> a @~ no_tip
    NOMINAL_FARE -> \a -> a @~ nominal_fare
    CUSTOMER_CANCELLATION_DUES -> \a -> a @~ customer_cancellation_dues
    NOT_NOW -> \a -> a @~ not_now
    NOTE -> \a -> a @~ note
    NOTIFY_ME -> \a -> a @~ notify_me
    OF -> \a -> a @~ of'
    OK_I_WILL_WAIT -> \a -> a @~ ok_i_will_wait
    ONLINE_ -> \a -> a @~ online_
    OTHER -> \a -> a @~ other
    OTHERS -> \a -> a @~ others
    OTP -> \a -> a @~ otp
    OUR_SUGGESTED_PRICE_FOR_THIS_TRIP_IS -> \a -> a @~ our_suggested_price_for_this_trip_is
    PAID -> \a -> a @~ paid
    PAY_DIRECTLY_TO_YOUR_DRIVER_USING_CASH_UPI -> \a -> a @~ pay_directly_to_your_driver_using_cash_upi
    PAY_DRIVER_USING_CASH_OR_UPI -> \a -> a @~ pay_driver_using_cash_or_upi
    PAY_DRIVER_USING_CASH_OR_UPI_ -> \a -> a @~ pay_driver_using_cash_or_upi_
    PAY_THE_DRIVER -> \a -> a @~ pay_the_driver
    PAY_THE_DRIVER_INFO -> \a -> a @~ pay_the_driver_info
    PAY_THE_DRIVER_NOTE -> \a -> a @~ pay_the_driver_note
    PAY_VIA_CASH_OR_UPI -> \a -> a @~ pay_via_cash_or_upi
    PAYMENT_METHOD -> \a -> a @~ payment_method
    PAYMENT_METHOD_STRING -> \a -> a @~ payment_method_string
    PAYMENT_METHOD_STRING_ -> \a -> a @~ payment_method_string_
    PEOPLE -> \a -> a @~ people
    PERCENTAGE_OF_NOMINAL_FARE -> \a -> a @~ percentage_of_nominal_fare
    PERSONAL_DETAILS -> \a -> a @~ personal_details
    PICK_UP_LOCATION -> \a -> a @~ pick_up_location
    PICK_UP_LOCATION_INCORRECT -> \a -> a @~ pick_up_location_incorrect
    PICKUP_AND_DROP -> \a -> a @~ pickup_and_drop
    PICKUP_CHARGE -> \a -> a @~ pickup_charge
    PLACE_CALL -> \a -> a @~ place_call
    PLEASE_CHOOSE_YOUR_PREFERRED_LANGUAGE_TO_CONTINUE -> \a -> a @~ please_choose_your_preferred_language_to_continue
    PLEASE_COME_FAST_I_AM_WAITING -> \a -> a @~ please_come_fast_i_am_waiting
    PLEASE_COME_SOON -> \a -> a @~ please_come_soon
    PLEASE_PAY_THE_FINAL_AMOUNT_TO_THE_DRIVER_VIA_CASH -> \a -> a @~ please_pay_the_final_amount_to_the_driver_via_cash
    PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL -> \a -> a @~ please_tell_us_why_you_want_to_cancel
    PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE -> \a -> a @~ please_update_app_to_continue_service
    PLEASE_WAIT_I_WILL_BE_THERE -> \a -> a @~ please_wait_i_will_be_there
    PLEASE_WAIT_WHILE_IN_PROGRESS -> \a -> a @~ please_wait_while_in_progress
    PREFER_NOT_TO_SAY -> \a -> a @~ prefer_not_to_say
    PRIVACY_POLICY -> \a -> a @~ privacy_policy
    PROBLEM_AT_OUR_END -> \a -> a @~ problem_at_our_end
    PROFILE_COMPLETION -> \a -> a @~ profile_completion
    PROMOTION -> \a -> a @~ promotion
    QUOTE_EXPIRED -> \a -> a @~ quote_expired
    RATE_ABOVE_MIN_FARE -> \a -> a @~ rate_above_min_fare
    RATE_CARD -> \a -> a @~ rate_card
    RATE_YOUR_DRIVER -> \a -> a @~ rate_your_driver
    RATE_YOUR_RIDE -> \a -> a @~ rate_your_ride
    RATE_YOUR_RIDE_WITH -> \a -> a @~ rate_your_ride_with
    REFEREAL_CODE_DISCRIPTION -> \a -> a @~ refereal_code_discription
    REFERRAL_CODE_APPLIED -> \a -> a @~ referral_code_applied
    REFERRAL_CODE_SUCCESSFULL -> \a -> a @~ referral_code_successfull
    REGISTER_USING_DIFFERENT_NUMBER -> \a -> a @~ register_using_different_number
    REMOVE -> \a -> a @~ remove
    REMOVE_FAVOURITE -> \a -> a @~ remove_favourite
    REPEAT_RIDE -> \a -> a @~ repeat_ride
    REPORT_AN_ISSUE -> \a -> a @~ report_an_issue
    REPORT_AN_ISSUE_WITH_THIS_TRIP -> \a -> a @~ report_an_issue_with_this_trip
    REQUEST_AUTO_RIDE arg1 -> \a -> (a @~ request_auto_ride) arg1
    REQUEST_CALLBACK -> \a -> a @~ request_callback
    REQUEST_RIDE -> \a -> a @~ request_ride
    REQUEST_SUBMITTED -> \a -> a @~ request_submitted
    REQUEST_TO_DELETE_ACCOUNT -> \a -> a @~ request_to_delete_account
    RESEND -> \a -> a @~ resend
    RIDE_COMPLETED -> \a -> a @~ ride_completed
    RIDE_DETAILS -> \a -> a @~ ride_details
    RIDE_FARE -> \a -> a @~ ride_fare
    RIDE_ID -> \a -> a @~ ride_id
    RIDE_NOT_SERVICEABLE -> \a -> a @~ ride_not_serviceable
    APP_NOT_SERVICEABLE -> \a -> a @~ app_not_serviceable
    SAVE -> \a -> a @~ save
    SAVE_AS -> \a -> a @~ save_as
    SAVE_PLACE -> \a -> a @~ save_place
    SAVED_ADDRESS_HELPS_YOU_KEEP_YOUR_FAVOURITE_PLACES_HANDY -> \a -> a @~ saved_address_helps_you_keep_your_favourite_places_handy
    SAVED_ADDRESSES -> \a -> a @~ saved_addresses
    SEARCH_AGAIN_WITH -> \a -> a @~ search_again_with
    SEARCH_AGAIN_WITH_A_TIP -> \a -> a @~ search_again_with_a_tip
    SEARCH_AGAIN_WITHOUT_A_TIP -> \a -> a @~ search_again_without_a_tip
    SEARCH_CONTACTS -> \a -> a @~ search_contacts
    SELECT_A_RIDE -> \a -> a @~ select_a_ride
    SELECT_AN_OFFER -> \a -> a @~ select_an_offer
    SELECT_AN_OFFER_FROM_OUR_DRIVERS -> \a -> a @~ select_an_offer_from_our_drivers
    SELECT_AN_OFFER_FROM_OUR_DRIVERS_INFO -> \a -> a @~ select_an_offer_from_our_drivers_info
    SELECT_CONTACTS -> \a -> a @~ select_contacts
    SELECT_FAVOURITE -> \a -> a @~ select_favourite
    SELECT_ON_MAP -> \a -> a @~ select_on_map
    SELECT_YOUR_DROP -> \a -> a @~ select_your_drop
    SELECT_YOUR_GENDER -> \a -> a @~ select_your_gender
    SEND_EMAIL -> \a -> a @~ send_email
    SERVICE_CHARGES -> \a -> a @~ service_charges
    SET_LOCATION_ON_MAP -> \a -> a @~ set_location_on_map
    SET_NOW -> \a -> a @~ set_now
    SET_UP_YOUR_ACCOUNT -> \a -> a @~ set_up_your_account
    SHARE_APP -> \a -> a @~ share_app
    SHARE_RIDE_WITH_EMERGENCY_CONTACTS -> \a -> a @~ share_ride_with_emergency_contacts
    SHOW_ALL_OPTIONS -> \a -> a @~ show_all_options
    SIX_DIGIT_REFERRAL_CODE -> \a -> a @~ six_digit_referral_code
    SKIP -> \a -> a @~ skip
    SOFTWARE_LICENSE -> \a -> a @~ software_license
    SORRY_WE_COULDNT_FIND_ANY_RIDES -> \a -> a @~ sorry_we_couldnt_find_any_rides
    SORT_BY -> \a -> a @~ sort_by
    SPACIOUS -> \a -> a @~ spacious
    START_ -> \a -> a @~ start_
    START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS -> \a -> a @~ start_your_chat_using_these_quick_chat_suggestions
    START_YOUR_CHAT_WITH_THE_DRIVER -> \a -> a @~ start_your_chat_with_the_driver
    STEPS_TO_COMPLETE -> \a -> a @~ steps_to_complete
    SUBJECT -> \a -> a @~ subject
    SUBMIT -> \a -> a @~ submit
    SUBMIT_FEEDBACK -> \a -> a @~ submit_feedback
    SUCCESSFUL_ONBOARD arg1 -> \a -> (a @~ successful_onboard) arg1
    SUPPORT -> \a -> a @~ support
    SUV -> \a -> a @~ suv
    SEDAN -> \a -> a @~ sedan
    T_AND_C_A -> \a -> a @~ t_and_c_a
    TERMS_AND_CONDITIONS -> \a -> a @~ terms_and_conditions
    THANK_YOU_FOR_WRITING -> \a -> a @~ thank_you_for_writing
    THANK_YOU_FOR_WRITING_TO_US -> \a -> a @~ thank_you_for_writing_to_us
    THANK_YOUR_DRIVER -> \a -> a @~ thank_your_driver
    THE_TRIP_IS_VERY_SHORT_AND_JUST_TAKE -> \a -> a @~ the_trip_is_very_short_and_just_take
    TIP -> \a -> a @~ tip
    TO_THE -> \a -> a @~ to_the
    TOTAL_AMOUNT -> \a -> a @~ total_amount
    TOTAL_FARE_MAY_CHANGE_DUE_TO_CHANGE_IN_ROUTE -> \a -> a @~ total_fare_may_change_due_to_change_in_route
    TOTAL_PAID -> \a -> a @~ total_paid
    TRACK_LIVE_LOCATION_USING -> \a -> a @~ track_live_location_using
    TRIP_CHARGES -> \a -> a @~ trip_charges
    TRIP_DETAILS_ -> \a -> a @~ trip_details_
    TRIP_ID -> \a -> a @~ trip_id
    TRY_AGAIN -> \a -> a @~ try_again
    TRY_AGAIN_WITH -> \a -> a @~ try_again_with
    TRY_AGAIN_WITH_A_TIP -> \a -> a @~ try_again_with_a_tip
    TRY_AGAIN_WITHOUT_TIP -> \a -> a @~ try_again_without_tip
    TRY_CONNECTING_WITH_THE_DRIVER -> \a -> a @~ try_connecting_with_the_driver
    TRY_LOOKING_FOR_RIDES_AGAIN -> \a -> a @~ try_looking_for_rides_again
    UNREACHABLE_PLEASE_CALL_BACK -> \a -> a @~ unreachable_please_call_back
    UPDATE -> \a -> a @~ update
    UPDATE_PERSONAL_DETAILS -> \a -> a @~ update_personal_details
    SETUP_NOW -> \a -> a @~ setup_now
    UPDATE_REQUIRED -> \a -> a @~ update_required
    USE_CURRENT_LOCATION -> \a -> a @~ use_current_location
    USER -> \a -> a @~ user
    VERIFYING_OTP -> \a -> a @~ verifying_otp
    VIEW_ALL_RIDES -> \a -> a @~ view_all_rides
    VIEW_BREAKDOWN -> \a -> a @~ view_breakdown
    VIEW_DETAILS -> \a -> a @~ view_details
    VIEW_INVOICE -> \a -> a @~ view_invoice
    VISIT_MY_RIDES_SECTION_FOR_RIDE_SPECIFIC_COMPLAINTS -> \a -> a @~ visit_my_rides_section_for_ride_specific_complaints
    WAIT_TIME -> \a -> a @~ wait_time
    WAIT_TIME_TOO_LONG -> \a -> a @~ wait_time_too_long
    WAITING_CHARGE -> \a -> a @~ waiting_charge
    WAITING_CHARGE_DESCRIPTION -> \a -> a @~ waiting_charge_description
    WAITING_CHARGE_RATECARD_DESCRIPTION arg1 arg2 -> \a -> (a @~ waiting_charge_ratecard_description) arg1 arg2
    WAITING_CHARGE_INFO arg1 arg2 -> \a -> (a @~ waiting_charge_info) arg1 arg2
    WE_HAVE_RECEIVED_YOUR_ISSUE -> \a -> a @~ we_have_received_your_issue
    WE_HAVE_RECEIVED_YOUR_ISSUE_WELL_REACH_OUT_TO_YOU_IN_SOMETIME -> \a -> a @~ we_have_received_your_issue_well_reach_out_to_you_in_sometime
    WE_NEED_ACCESS_TO_YOUR_LOCATION -> \a -> a @~ we_need_access_to_your_location
    WE_WILL_DELETE_YOUR_ACCOUNT -> \a -> a @~ we_will_delete_your_account
    WELCOME_TEXT -> \a -> a @~ welcome_text
    WHERE_TO -> \a -> a @~ where_to
    WORK -> \a -> a @~ work
    WRITE_A_COMMENT -> \a -> a @~ write_a_comment
    WRITE_TO_US -> \a -> a @~ write_to_us
    WRONG_OTP -> \a -> a @~ wrong_otp
    YES -> \a -> a @~ yes
    YES_CANCEL_SEARCH -> \a -> a @~ yes_cancel_search
    YES_DELETE_IT -> \a -> a @~ yes_delete_it
    YES_REMOVE -> \a -> a @~ yes_remove
    YES_TRY_AGAIN -> \a -> a @~ yes_try_again
    YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT arg1 -> \a -> (a @~ you_are_about_to_call_namma_yatri_support) arg1
    YOU_ARE_ABOUT_TO_CALL_NEAREST_EMERGENCY_CENTRE -> \a -> a @~ you_are_about_to_call_nearest_emergency_centre
    YOU_ARE_OFFLINE -> \a -> a @~ you_are_offline
    YOU_CAN_CANCEL_RIDE -> \a -> a @~ you_can_cancel_ride
    YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE -> \a -> a @~ you_can_describe_the_issue_you_faced_here
    YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER arg1 -> \a -> (a @~ you_can_get_referral_code_from_driver) arg1
    YOU_CAN_TAKE_A_WALK_OR_CONTINUE_WITH_RIDE_BOOKING -> \a -> a @~ you_can_take_a_walk_or_continue_with_ride_booking
    YOU_HAVE_RIDE_OFFERS_ARE_YOU_SURE_YOU_WANT_TO_CANCEL -> \a -> a @~ you_have_ride_offers_are_you_sure_you_want_to_cancel
    YOU_HAVENT_TAKEN_A_TRIP_YET -> \a -> a @~ you_havent_taken_a_trip_yet
    YOU_HAVENT_TAKEN_A_TRIP_YET_IN_PAST_HOURS arg1 -> \a -> (a @~ you_havent_taken_a_trip_yet_in_past_hours) arg1
    YOU_RATED -> \a -> a @~ you_rated
    YOU_WILL_BE_ASKED_TO_SELECT_CONTACTS -> \a -> a @~ you_will_be_asked_to_select_contacts
    YOUR_EMAIL_ID -> \a -> a @~ your_email_id
    LOCATION_PERMISSION_SUBTITLE -> \a -> a @~ location_permission_subtitle
    YOUR_NUMBER_WILL_BE_VISIBLE_TO_THE_DRIVER_USE_IF_NOT_CALLING_FROM_REGISTERED_NUMBER -> \a -> a @~ your_number_will_be_visible_to_the_driver_use_if_not_calling_from_registered_number
    YOUR_NUMBER_WILL_NOT_BE_SHOWN_TO_THE_DRIVER_THE_CALL_WILL_BE_RECORDED_FOR_COMPLIANCE -> \a -> a @~ your_number_will_not_be_shown_to_the_driver_the_call_will_be_recorded_for_compliance
    YOUR_RECENT_RIDE -> \a -> a @~ your_recent_ride
    YOUR_RIDE_HAS_STARTED -> \a -> a @~ your_ride_has_started
    YOUR_RIDE_IS_NOW_COMPLETE -> \a -> a @~ your_ride_is_now_complete
    YOUR_RIDES -> \a -> a @~ your_rides
    YOUR_TRIP_IS_TOO_SHORT_YOU_ARE_JUST -> \a -> a @~ your_trip_is_too_short_you_are_just
    DOWNLOAD_INVOICE -> \a -> a @~ download_invoice
    WAS_YOUR_CALL_SUCCESSFUL -> \a -> a @~ was_your_call_successful
    DRIVER_ADDITIONS -> \a -> a @~ driver_additions
    FARE_UPDATE_POLICY -> \a -> a @~ fare_update_policy
    DRIVER_ADDITIONS_OPTIONAL -> \a -> a @~ driver_additions_optional
    THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC -> \a -> a @~ the_driver_may_quote_extra_to_cover_for_traffic
    DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE arg1 -> \a -> (a @~ driver_additions_are_calculated_at_rate) arg1
    DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE -> \a -> a @~ driver_may_not_charge_this_additional_fare
    YOU_MAY_SEE_AN_UPDATED_FINAL_FARE_DUE_TO_ANY_OF_THE_BELOW_REASONS -> \a -> a @~ you_may_see_an_updated_final_fare_due_to_any_of_the_below_reasons
    REASON_CHANGE_IN_ROUTE_A -> \a -> a @~ reason_change_in_route_a
    REASON_CHANGE_IN_ROUTE_B -> \a -> a @~ reason_change_in_route_b
    GO_TO_ZONE arg1 -> \a -> (a @~ go_to_zone) arg1
    REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON -> \a -> a @~ request_received_we_will_call_you_back_soon
    CONTACT_REMOVED_SUCCESSFULLY -> \a -> a @~ contact_removed_successfully
    CORPORATE_ADDRESS -> \a -> a @~ corporate_address
    CORPORATE_ADDRESS_DESCRIPTION arg1 -> \a -> (a @~ corporate_address_description) arg1
    CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL arg1 -> \a -> (a @~ corporate_address_description_additional) arg1
    REGISTERED_ADDRESS -> \a -> a @~ registered_address
    REGISTERED_ADDRESS_DESCRIPTION arg1 -> \a -> (a @~ registered_address_description) arg1
    REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL arg1 -> \a -> (a @~ registered_address_description_additional) arg1
    RECOMMENDED -> \a -> a @~ recommended
    COMPLETE_YOUR_PROFILE_FOR_A_PERSONALISED_RIDE_EXPERIENCE -> \a -> a @~ complete_your_profile_for_a_personalised_ride_experience
    COMPLETE_YOUR_NAMMA_SAFETY_SETUP_FOR_SAFE_RIDE_EXPERIENCE -> \a -> a @~ complete_your_namma_safety_setup_for_safe_ride_experience
    UPDATE_NOW -> \a -> a @~ update_now
    WE_WOULD_APPRECIATE_YOUR_FEEDBACK -> \a -> a @~ we_would_appreciate_your_feedback
    REASON_FOR_DELETING_ACCOUNT -> \a -> a @~ reason_for_deleting_account
    SUBMIT_REQUEST -> \a -> a @~ submit_request
    PLEASE_ENTER_A_VALID_EMAIL -> \a -> a @~ please_enter_a_valid_email
    WE_WOULD_APPRECIATE_YOUR_REASONING -> \a -> a @~ we_would_appreciate_your_reasoning
    OK_GOT_IT -> \a -> a @~ ok_got_it
    WAIT_FOR_DRIVER -> \a -> a @~ wait_for_driver
    NO_LONGER_REQUIRE_A_RIDE_DUE_TO_CHANGE_IN_PLANS -> \a -> a @~ no_longer_require_a_ride_due_to_change_in_plans
    CANCELLING_AS_I_GOT_A_RIDE_ON_ANOTHER_APP -> \a -> a @~ cancelling_as_i_got_a_ride_on_another_app
    DRIVER_LOCATION_WASNT_CHANGING_ON_THE_MAP -> \a -> a @~ driver_location_wasnt_changing_on_the_map
    DRIVER_WAS_TAKING_TOO_LONG_TO_REACH_THE_PICKUP_LOCATION -> \a -> a @~ driver_was_taking_too_long_to_reach_the_pickup_location
    THE_PICKUP_LOCATION_ENTERED_WAS_WRONG -> \a -> a @~ the_pickup_location_entered_was_wrong
    YOUR_DRIVER_IS_JUST -> \a -> a @~ your_driver_is_just
    M_AWAY -> \a -> a @~ m_away
    DRIVER_HAS_ALREADY_TRAVELLED -> \a -> a @~ driver_has_already_travelled
    PLEASE_CONTACT_THE_DRIVER_BEFORE_CANCELLING -> \a -> a @~ please_contact_the_driver_before_cancelling
    CONFIRM_WITH_YOUR_DRIVER -> \a -> a @~ confirm_with_your_driver
    CHANGE_OF_PLANS -> \a -> a @~ change_of_plans
    DRIVER_IS_NOT_MOVING -> \a -> a @~ driver_is_not_moving
    WRONG_PICKUP_LOCATION -> \a -> a @~ wrong_pickup_location
    DIFFERENT_VEHICLE_NUMBER -> \a -> a @~ different_vehicle_number
    VEHICLE_NUMBER_IS_DIFFERENT_FROM_WHAT_IS_SHOWN_IN_THE_APP -> \a -> a @~ vehicle_number_is_different_from_what_is_shown_in_the_app
    DIFFERENT_AUTO -> \a -> a @~ different_auto
    DIFFERENT_CAB -> \a -> a @~ different_cab
    DRIVER_MIGHT_BE_TAKING_ALTERNATE_ROUTE -> \a -> a @~ driver_might_be_taking_alternate_route
    DRIVER_IS_NOT_MOVING_Q -> \a -> a @~ driver_is_not_moving_q
    WOULD_YOU_LIKE_TO_CHECK_WITH_THE_DRIVER_BEFORE_CANCELLING -> \a -> a @~ would_you_like_to_check_with_the_driver_before_cancelling
    DRIVER_IS_NEAR_YOUR_LOCATION -> \a -> a @~ driver_is_near_your_location
    SOME_OTHER_REASON -> \a -> a @~ some_other_reason
    LOCATION_PERMISSION_SUBTITLE_NEW_USER -> \a -> a @~ location_permission_subtitle_new_user
    METRO_RIDE -> \a -> a @~ metro_ride
    GO_BACK_TEXT -> \a -> a @~ go_back_text
    DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_JUST -> \a -> a @~ driver_preferred_your_special_request_and_is_just
    DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST -> \a -> a @~ driver_preferred_your_special_request
    AND_HAS_TRAVELLED -> \a -> a @~ and_has_travelled
    PLEASE_FIND_REVISED_FARE_ESTIMATE -> \a -> a @~ please_find_revised_fare_estimate
    FARE_ESTIMATE -> \a -> a @~ fare_estimate
    TIP_SELECTED -> \a -> a @~ tip_selected
    ADD_A_TIP_TO_FIND_A_RIDE_QUICKER -> \a -> a @~ add_a_tip_to_find_a_ride_quicker
    IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL -> \a -> a @~ it_seems_to_be_taking_longer_than_usual
    CONTINUE_SEARCH_WITH -> \a -> a @~ continue_search_with
    CONTINUING_SEARCH_WITH -> \a -> a @~ continuing_search_with
    SEARCHING_WITH -> \a -> a @~ searching_with
    THE_DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION -> \a -> a @~ the_driver_preferred_your_special_request_and_is_already_on_the_way_to_your_location
    DRIVER_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION -> \a -> a @~ driver_is_already_on_the_way_to_your_location
    ALLOW_LOCATION_ACCESS -> \a -> a @~ allow_location_access
    MESSAGE_FROM_DRIVER -> \a -> a @~ message_from_driver
    REPLY -> \a -> a @~ reply
    NAME_SHOULD_BE_MORE_THAN_2_CHARACTERS -> \a -> a @~ name_should_be_more_than_2_characters
    THIS_FIELD_IS_REQUIRED -> \a -> a @~ this_field_is_required
    EMAIL_EXISTS_ALREADY -> \a -> a @~ email_exists_already
    OKAY_GOT_IT -> \a -> a @~ okay_got_it
    CALL_NAMMA_YATRI_SUPPORT arg1 -> \a -> (a @~ call_namma_yatri_support) arg1
    CALL_112 -> \a -> a @~ call_112
    SEATS -> \a -> a @~ seats
    OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN -> \a -> a @~ otp_page_has_been_expired_please_request_otp_again
    OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER -> \a -> a @~ otp_entering_limit_exhausted_please_try_again_later
    TOO_MANY_LOGIN_ATTEMPTS_PLEASE_TRY_AGAIN_LATER -> \a -> a @~ too_many_login_attempts_please_try_again_later
    SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN -> \a -> a @~ something_went_wrong_please_try_again
    SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES -> \a -> a @~ sorry_limit_exceeded_you_cant_add_any_more_favourites
    IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_RIDE_ -> \a -> a @~ it_seems_like_you_have_an_ongoing_ride_
    CANCELLATION_UNSUCCESSFULL_PLEASE_TRY_AGAIN -> \a -> a @~ cancellation_unsuccessfull_please_try_again
    NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN -> \a -> a @~ no_driver_available_at_the_moment_please_try_again
    OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN arg1 -> \a -> (a @~ otp_for_the_jatri_sathi_zone_has_been_expired_please_try_looking_again) arg1
    NO_CONTACTS_FOUND_ON_THE_DEVICE_TO_BE_ADDED -> \a -> a @~ no_contacts_found_on_the_device_to_be_added
    PLEASE_ENABLE_CONTACTS_PERMISSION_TO_PROCEED -> \a -> a @~ please_enable_contacts_permission_to_proceed
    LIMIT_REACHED_3_OF_3_EMERGENCY_CONTACTS_ALREADY_ADDED -> \a -> a @~ limit_reached_3_of_3_emergency_contacts_already_added
    INVALID_CONTACT_FORMAT -> \a -> a @~ invalid_contact_format
    OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER -> \a -> a @~ otp_resent_limit_exhausted_please_try_again_later
    RATE_YOUR_EXPERIENCE -> \a -> a @~ rate_your_experience
    REPORT_ISSUE_ -> \a -> a @~ report_issue_
    DONE -> \a -> a @~ done
    PLEASE_TELL_US_WHAT_WENT_WRONG -> \a -> a @~ please_tell_us_what_went_wrong
    YOUR_FEEDBACK_HELPS_US arg1 -> \a -> (a @~ your_feedback_helps_us) arg1
    DID_YOU_FACE_ANY_ISSUE -> \a -> a @~ did_you_face_any_issue
    DID_THE_DRIVER_OFFER_ASSISTANCE -> \a -> a @~ did_the_driver_offer_assistance
    WAS_THE_DRIVER_UNDERSTANDING_OF_YOUR_NEEDS -> \a -> a @~ was_the_driver_understanding_of_your_needs
    WE_NOTICED_YOUR_RIDE_ENDED_AWAY -> \a -> a @~ we_noticed_your_ride_ended_away
    GET_CALLBACK_FROM_US -> \a -> a @~ get_callback_from_us
    DRIVER_WAS_NOT_READY_TO_GO -> \a -> a @~ driver_was_not_ready_to_go
    ASKING_FOR_MORE_MONEY -> \a -> a @~ asking_for_more_money
    VEHICLE_BROKEN -> \a -> a @~ vehicle_broken
    WE_WILL_GIVE_YOU_CALLBACK -> \a -> a @~ we_will_give_you_callback
    YOUR_ISSUE_HAS_BEEN_REPORTED -> \a -> a @~ your_issue_has_been_reported
    ISSUE_REPORT_ALREADY_EXISTS -> \a -> a @~ issue_report_already_exists
    OTP_RESENT_SUCCESSFULLY -> \a -> a @~ otp_resent_successfully
    DESCRIPTION_SHOULD_BE_MORE_THAN_10_ALPHABETIC_CHARACTERS -> \a -> a @~ description_should_be_more_than_10_alphabetic_characters
    INCORRECT_OTP_PLEASE_TRY_AGAIN -> \a -> a @~ incorrect_otp_please_try_again
    N_MORE_ATTEMPTS_LEFT -> \a -> a @~ n_more_attempts_left
    GO_TO_SELECTED_PICKUP_SPOT -> \a -> a @~ go_to_selected_pickup_spot
    GO_TO_SELECTED_PICKUP_SPOT_AS_AUTOS_ARE_RESTRICTED -> \a -> a @~ go_to_selected_pickup_spot_as_autos_are_restricted
    UNPROFESSIONAL_DRIVER -> \a -> a @~ unprofessional_driver
    RASH_DRIVING -> \a -> a @~ rash_driving
    DRIVER_CHARGED_MORE -> \a -> a @~ driver_charged_more
    UNCOMFORTABLE_AUTO -> \a -> a @~ uncomfortable_auto
    UNCOMFORTABLE_CAB -> \a -> a @~ uncomfortable_cab
    TRIP_GOT_DELAYED -> \a -> a @~ trip_got_delayed
    FELT_UNSAFE -> \a -> a @~ felt_unsafe
    POLITE_DRIVER -> \a -> a @~ polite_driver
    EXPERT_DRIVING -> \a -> a @~ expert_driving
    SAFE_RIDE -> \a -> a @~ safe_ride
    CLEAN_AUTO -> \a -> a @~ clean_auto
    CLEAN_CAB -> \a -> a @~ clean_cab
    ON_TIME -> \a -> a @~ on_time
    SKILLED_NAVIGATOR -> \a -> a @~ skilled_navigator
    RUDE_DRIVER -> \a -> a @~ rude_driver
    TOO_MANY_CALLS -> \a -> a @~ too_many_calls
    RECKLESS_DRIVING -> \a -> a @~ reckless_driving
    LATE_DROP_OFF -> \a -> a @~ late_drop_off
    LATE_PICK_UP -> \a -> a @~ late_pick_up
    POOR_EXPERIENCE -> \a -> a @~ poor_experience
    TERRIBLE_EXPERIENCE -> \a -> a @~ terrible_experience
    NEEDS_IMPROVEMENT -> \a -> a @~ needs_improvement
    AMAZING -> \a -> a @~ amazing
    ALMOST_PERFECT -> \a -> a @~ almost_perfect
    ASKED_FOR_EXTRA_FARE -> \a -> a @~ asked_for_extra_fare
    ANYTHING_THAT_YOU_WOULD_LIKE_TO_TELL_US -> \a -> a @~ anything_that_you_would_like_to_tell_us
    PLATFORM_FEE -> \a -> a @~ platform_fee
    FINDING_QUOTES_TEXT -> \a -> a @~ finding_quotes_text
    PLEASE_WAIT -> \a -> a @~ please_wait
    PAY_DRIVER_USING_WALLET -> \a -> a @~ pay_driver_using_wallet
    FASTER -> \a -> a @~ faster
    NEW_ -> \a -> a @~ new_
    SGST -> \a -> a @~ sgst
    OTP_EXPIRED -> \a -> a @~ otp_expired
    OTP_EXPIRED_DESCRIPTION -> \a -> a @~ otp_expired_description
    PLATFORM_GST -> \a -> a @~ platform_gst
    MISC_WAITING_CHARGE -> \a -> a @~ misc_waiting_charge
    TAXI_FROM_ZONE arg1 -> \a -> (a @~ taxi_from_zone) arg1
    TAXI -> \a -> a @~ taxi
    AC -> \a -> a @~ ac
    NON_AC -> \a -> a @~ non_ac
    AC_TAXI -> \a -> a @~ ac_taxi
    NON_AC_TAXI -> \a -> a @~ non_ac_taxi
    GET_OTP_VIA_WHATSAPP -> \a -> a @~ get_otp_via_whatsapp
    OR -> \a -> a @~ or
    HELPS_DRIVER_CONFIRM_ITS_YOU -> \a -> a @~ helps_driver_confirm_its_you
    LETS_GET_YOU_TRIP_READY -> \a -> a @~ lets_get_you_trip_ready
    GOT_AN_OTP -> \a -> a @~ got_an_otp
    JUST_ONE_LAST_THING -> \a -> a @~ just_one_last_thing
    TOLL_CHARGES_WILL_BE_EXTRA -> \a -> a @~ toll_charges_will_be_extra
    AUTO_RICKSHAW -> \a -> a @~ auto_rickshaw
    CABS_AVAILABLE -> \a -> a @~ cabs_available
    GENERAL_DISABILITY_DESCRIPTION -> \a -> a @~ general_disability_description
    PI_POINTER_1 -> \a -> a @~ pi_pointer_1
    PI_POINTER_2 -> \a -> a @~ pi_pointer_2
    VI_POINTER_1 -> \a -> a @~ vi_pointer_1
    VI_POINTER_2 -> \a -> a @~ vi_pointer_2
    HI_POINTER_1 -> \a -> a @~ hi_pointer_1
    HI_POINTER_2 -> \a -> a @~ hi_pointer_2
    ACCESSIBILITY_TEXT arg1 -> \a -> (a @~ accessibility_text) arg1
    TO_CATER_YOUR_SPECIFIC_NEEDS arg1 -> \a -> (a @~ to_cater_your_specific_needs) arg1
    SPECIAL_ASSISTANCE -> \a -> a @~ special_assistance
    SELECT_THE_CONDITION_THAT_IS_APPLICABLE -> \a -> a @~ select_the_condition_that_is_applicable
    DISABILITY_CLAIMER_TEXT -> \a -> a @~ disability_claimer_text
    ARE_YOU_A_PERSON_WITH_DISABILITY -> \a -> a @~ are_you_a_person_with_disability
    DO_YOU_NEEED_SPECIAL_ASSISTANCE -> \a -> a @~ do_you_neeed_special_assistance
    ASSISTANCE_REQUIRED -> \a -> a @~ assistance_required
    NO_DISABILITY -> \a -> a @~ no_disability
    LEARN_HOW_TEXT arg1 -> \a -> (a @~ learn_how_text) arg1
    UPDATE_PROFILE -> \a -> a @~ update_profile
    NOW_GET_ASSISTED_RIDES -> \a -> a @~ now_get_assisted_rides
    SENT_OTP_VIA_SMS -> \a -> a @~ sent_otp_via_sms
    SENT_OTP_VIA_WHATSAPP -> \a -> a @~ sent_otp_via_whatsapp
    PLEASE_ENABLE_LOCATION_PERMISSION arg1 -> \a -> (a @~ please_enable_location_permission) arg1
    ENABLE_LOCATION_PERMISSION_TO -> \a -> a @~ enable_location_permission_to
    AC_SUV -> \a -> a @~ ac_suv
    AC_CAB -> \a -> a @~ ac_cab
    RIDE_TYPE -> \a -> a @~ ride_type
    ERNAKULAM_LIMIT_CHARGE -> \a -> a @~ ernakulam_limit_charge
    SELECT_LOCATION_ON_MAP -> \a -> a @~ select_location_on_map
    DOWNLOAD_DRIVER_RECEIPT -> \a -> a @~ download_driver_receipt
    VIEW_DRIVER_RECEIPT -> \a -> a @~ view_driver_receipt
    DRIVER_RECEIPT -> \a -> a @~ driver_receipt
    HELP -> \a -> a @~ help
    FARE_INFO_TEXT arg1 -> \a -> (a @~ fare_info_text) arg1
    EDUCATIONAL_POP_UP_SLIDE_1_TITLE -> \a -> a @~ educational_pop_up_slide_1_title
    EDUCATIONAL_POP_UP_SLIDE_2_TITLE -> \a -> a @~ educational_pop_up_slide_2_title
    EDUCATIONAL_POP_UP_SLIDE_3_TITLE -> \a -> a @~ educational_pop_up_slide_3_title
    EDUCATIONAL_POP_UP_SLIDE_4_TITLE -> \a -> a @~ educational_pop_up_slide_4_title
    EDUCATIONAL_POP_UP_SLIDE_5_TITLE -> \a -> a @~ educational_pop_up_slide_5_title
    EDUCATIONAL_POP_UP_SLIDE_1_SUBTITLE -> \a -> a @~ educational_pop_up_slide_1_subtitle
    EDUCATIONAL_POP_UP_SLIDE_2_SUBTITLE -> \a -> a @~ educational_pop_up_slide_2_subtitle
    EDUCATIONAL_POP_UP_SLIDE_3_SUBTITLE -> \a -> a @~ educational_pop_up_slide_3_subtitle
    EDUCATIONAL_POP_UP_SLIDE_4_SUBTITLE -> \a -> a @~ educational_pop_up_slide_4_subtitle
    EDUCATIONAL_POP_UP_SLIDE_5_SUBTITLE -> \a -> a @~ educational_pop_up_slide_5_subtitle
    INCLUSIVE_AND_ACCESSIBLE -> \a -> a @~ inclusive_and_accessible
    YOU_SEEM_TO_BE_FAR_FROM_PICK_UP -> \a -> a @~ you_seem_to_be_far_from_pick_up
    ARE_YOU_SURE_YOU_WANT_TO_PROCEED_WITH_THE_BOOKING -> \a -> a @~ are_you_sure_you_want_to_proceed_with_the_booking
    MY_TICKETS -> \a -> a @~ my_tickets
    SOMETHING_WENT_WRONG_TRY_AGAIN_LATER -> \a -> a @~ something_went_wrong_try_again_later
    YOU_CAN_BOOK_TICKETS_TO_THE_ZOO_BY_CLICKING_THE_BUTTON -> \a -> a @~ you_can_book_tickets_to_the_zoo_by_clicking_the_button
    CHARGES_APPLICABLE_AFTER_3_MINS -> \a -> a @~ charges_applicable_after_3_mins
    WAITING_AT_PICKUP -> \a -> a @~ waiting_at_pickup
    REACHING_YOUR_DESTINATION_IN_ -> \a -> a @~ reaching_your_destination_in_
    LEARN_MORE -> \a -> a @~ learn_more
    PICKUP -> \a -> a @~ pickup
    PAY_BY_CASH_OR_UPI -> \a -> a @~ pay_by_cash_or_upi
    WAIT_TIMER -> \a -> a @~ wait_timer
    HOW_LONG_DRIVER_WAITED_FOR_PICKUP -> \a -> a @~ how_long_driver_waited_for_pickup
    YOU_WILL_PAY_FOR_EVERY_MINUTE arg1 arg2 -> \a -> (a @~ you_will_pay_for_every_minute) arg1 arg2
    CHAT_WITH -> \a -> a @~ chat_with
    QUICK -> \a -> a @~ quick
    CHATS -> \a -> a @~ chats
    REPLIES -> \a -> a @~ replies
    NAMMA_SAFETY -> \a -> a @~ namma_safety
    YOU_SENT -> \a -> a @~ you_sent
    MESSAGE_YOUR_DRIVER -> \a -> a @~ message_your_driver
    CHECK_IN_WITH_YOUR_DRIVER -> \a -> a @~ check_in_with_your_driver
    CHECK_IN_WITH_YOUR_EM arg1 -> \a -> (a @~ check_in_with_your_em) arg1
    TRACK_ON_GOOGLE_MAPS -> \a -> a @~ track_on_google_maps
    OTP_EXPIRE_TIMER -> \a -> a @~ otp_expire_timer
    SHOWS_FOR_HOW_LONG_YOUR_OTP_ -> \a -> a @~ shows_for_how_long_your_otp_
    IF_YOUR_OTP_EXPIRES_ -> \a -> a @~ if_your_otp_expires_
    YOU_HAVE_REACHED_DESTINATION -> \a -> a @~ you_have_reached_destination
    PLACES_YOU_MIGHT_LIKE_TO_GO_TO -> \a -> a @~ places_you_might_like_to_go_to
    SUGGESTED_DESTINATION -> \a -> a @~ suggested_destination
    RECENT_RIDES -> \a -> a @~ recent_rides
    ONE_CLICK_BOOKING_FOR_YOUR_FAVOURITE_JOURNEYS -> \a -> a @~ one_click_booking_for_your_favourite_journeys
    VIEW_MORE -> \a -> a @~ view_more
    VIEW_LESS -> \a -> a @~ view_less
    HAVE_A_REFFERAL -> \a -> a @~ have_a_refferal
    YOUR_SUGGESTED_DESTINATIONS_AND_RECENT_RIDES_WILL_APPEAR_HERE -> \a -> a @~ your_suggested_destinations_and_recent_rides_will_appear_here
    WELCOME_TO_NAMMA_YATRI_ -> \a -> a @~ welcome_to_namma_yatri_
    BOOK_AND_MOVE -> \a -> a @~ book_and_move
    ANYWHERE_IN_THE_CITY -> \a -> a @~ anywhere_in_the_city
    CHECKOUT_OUR_LIVE_STATS -> \a -> a @~ checkout_our_live_stats
    MOST_LOVED_APP arg1 -> \a -> (a @~ most_loved_app) arg1
    PICKUP_ -> \a -> a @~ pickup_
    PAST_SEARCHES -> \a -> a @~ past_searches
    SEARCH_RESULTS -> \a -> a @~ search_results
    EDIT_DESTINATION -> \a -> a @~ edit_destination
    REQUESTING_RIDE_IN -> \a -> a @~ requesting_ride_in
    CONFIRM_FARE -> \a -> a @~ confirm_fare
    REQUEST_CHANGE -> \a -> a @~ request_change
    REQUESTING_RIDE -> \a -> a @~ requesting_ride
    TAP_HERE_TO_STOP_AUTO_REQUESTING -> \a -> a @~ tap_here_to_stop_auto_requesting
    POWERED_BY -> \a -> a @~ powered_by
    BOOK_YOUR_RIDE -> \a -> a @~ book_your_ride
    START_TYPING_TO_SEARCH_PLACES -> \a -> a @~ start_typing_to_search_places
    FARE_UPDATED_WITH_CHARGES -> \a -> a @~ fare_updated_with_charges
    FARE_UPDATED_WITH_SHORTER_DIST -> \a -> a @~ fare_updated_with_shorter_dist
    FARE_UPDATED_WITH_LONGER_DIST -> \a -> a @~ fare_updated_with_longer_dist
    FARE_UPDATED_WITH_CHARGES_SHORTER_DIST -> \a -> a @~ fare_updated_with_charges_shorter_dist
    FARE_UPDATED_WITH_CHARGES_LONGER_DIST -> \a -> a @~ fare_updated_with_charges_longer_dist
    DID_YOU_HAVE_A_SAFE_JOURNEY -> \a -> a @~ did_you_have_a_safe_journey
    TRIP_WAS_SAFE_AND_WORRY_FREE -> \a -> a @~ trip_was_safe_and_worry_free
    DRIVER_BEHAVED_INAPPROPRIATELY -> \a -> a @~ driver_behaved_inappropriately
    I_DID_NOT_FEEL_SAFE -> \a -> a @~ i_did_not_feel_safe
    LOOKING_FOR_ANOTHER_RIDE -> \a -> a @~ looking_for_another_ride
    THE_RIDE_HAD_BEEN_CANCELLED_WE_ARE_FINDING_YOU_ANOTHER -> \a -> a @~ the_ride_had_been_cancelled_we_are_finding_you_another
    ENJOY_THE_RIDE -> \a -> a @~ enjoy_the_ride
    RIDE_STARTED -> \a -> a @~ ride_started
    DISCOVER_AWESOME_SPOTS_TAILORED_JUST_FOR_YOU -> \a -> a @~ discover_awesome_spots_tailored_just_for_you
    SMART -> \a -> a @~ smart
    ONE_CLICK -> \a -> a @~ one_click
    NOT_SERVICEABLE -> \a -> a @~ not_serviceable
    WE_ARE_NOT_LIVE_IN_YOUR_AREA -> \a -> a @~ we_are_not_live_in_your_area
    ACCOUNT_BLOCKED -> \a -> a @~ account_blocked
    YOU_CAN_STILL_ACCESS -> \a -> a @~ you_can_still_access
    FACING_PROBLEM_WITH_APP -> \a -> a @~ facing_problem_with_app
    TAP_HERE_TO_REPORT -> \a -> a @~ tap_here_to_report
    CONFIRM_YOUR_RIDE -> \a -> a @~ confirm_your_ride
    RIDE_SCHEDULED -> \a -> a @~ ride_scheduled
    RIDE_STARTS_ON -> \a -> a @~ ride_starts_on
    RENTAL_PACKAGE -> \a -> a @~ rental_package
    GO_HOME -> \a -> a @~ go_home
    CANCEL_RENTAL_BOOKING -> \a -> a @~ cancel_rental_booking
    ADD_FIRST_STOP -> \a -> a @~ add_first_stop
    DRIVER_WILL_BE_ASSIGNED_MINUTES_BEFORE_STARTING_THE_RIDE -> \a -> a @~ driver_will_be_assigned_minutes_before_starting_the_ride
    YEARS_AGO -> \a -> a @~ years_ago
    REPORTED_ISSUES -> \a -> a @~ reported_issues
    RESOLVED_ISSUES -> \a -> a @~ resolved_issues
    ISSUE_NO -> \a -> a @~ issue_no
    REPORTED -> \a -> a @~ reported
    RESOLVED -> \a -> a @~ resolved
    MONTHS_AGO -> \a -> a @~ months_ago
    DAYS_AGO -> \a -> a @~ days_ago
    HOURS_AGO -> \a -> a @~ hours_ago
    MIN_AGO -> \a -> a @~ min_ago
    SEC_AGO -> \a -> a @~ sec_ago
    RIDE_RELATED_ISSUE_PAGE_NAME -> \a -> a @~ ride_related_issue_page_name
    APP_RELATED_ISSUE_PAGE_NAME -> \a -> a @~ app_related_issue_page_name
    DRIVER_RELATED_ISSUE_PAGE_NAME -> \a -> a @~ driver_related_issue_page_name
    LOST_AND_FOUND_ISSUE_PAGE_NAME -> \a -> a @~ lost_and_found_issue_page_name
    SELECT_A_RIDE_TO_REPORT -> \a -> a @~ select_a_ride_to_report
    I_DONT_KNOW_WHICH_RIDE -> \a -> a @~ i_dont_know_which_ride
    YOUR_REPORTS -> \a -> a @~ your_reports
    VIEW -> \a -> a @~ view
    ADD_VOICE_NOTE -> \a -> a @~ add_voice_note
    VOICE_NOTE -> \a -> a @~ voice_note
    ADDED_IMAGES -> \a -> a @~ added_images
    NO_IMAGES_ADDED -> \a -> a @~ no_images_added
    SUBMIT_ISSUE_DETAILS -> \a -> a @~ submit_issue_details
    IMAGE_PREVIEW -> \a -> a @~ image_preview
    REPORT_ISSUE_CHAT_PLACEHOLDER arg1 -> \a -> (a @~ report_issue_chat_placeholder) arg1
    ADDED_VOICE_NOTE -> \a -> a @~ added_voice_note
    NO_VOICE_NOTE_ADDED -> \a -> a @~ no_voice_note_added
    CALL_DRIVER_TITLE -> \a -> a @~ call_driver_title
    CALL_DRIVER_DESCRIPTION -> \a -> a @~ call_driver_description
    CALL_SUPPORT_TITLE -> \a -> a @~ call_support_title
    CALL_SUPPORT_DESCRIPTION arg1 -> \a -> (a @~ call_support_description) arg1
    ADD_IMAGE -> \a -> a @~ add_image
    ADD_ANOTHER -> \a -> a @~ add_another
    IMAGES -> \a -> a @~ images
    ISSUE_SUBMITTED_TEXT -> \a -> a @~ issue_submitted_text
    ISSUE_RESOLVED_TEXT -> \a -> a @~ issue_resolved_text
    CHOOSE_AN_OPTION -> \a -> a @~ choose_an_option
    IMAGE -> \a -> a @~ image
    ISSUE_MARKED_AS_RESOLVED -> \a -> a @~ issue_marked_as_resolved
    STILL_HAVING_ISSUE -> \a -> a @~ still_having_issue
    RECORD_VOICE_NOTE -> \a -> a @~ record_voice_note
    CANCEL_BUTTON -> \a -> a @~ cancel_button
    MAX_IMAGES -> \a -> a @~ max_images
    SOS_ISSUE_PAGE_NAME -> \a -> a @~ sos_issue_page_name
    FARE_DISCREPANCIES_ISSUE_PAGE_NAME -> \a -> a @~ fare_discrepancies_issue_page_name
    PAYMENT_RELATED_ISSUE_PAGE_NAME -> \a -> a @~ payment_related_issue_page_name
    ACCOUNT_RELATED_ISSUE_PAGE_NAME -> \a -> a @~ account_related_issue_page_name
    PAYMENT_AND_FARE_RELATED_ISSUE_PAGE_NAME -> \a -> a @~ payment_and_fare_related_issue_page_name
    VEHICLE_RELATED_ISSUE_PAGE_NAME -> \a -> a @~ vehicle_related_issue_page_name
    ISSUE -> \a -> a @~ issue
    OTHER_ISSUES -> \a -> a @~ other_issues
    CANT_FIND_OPTION -> \a -> a @~ cant_find_option
    NEED_HELP -> \a -> a @~ need_help
    SAFETY_ISSUE_PAGE_NAME -> \a -> a @~ safety_issue_page_name
    WE_HOPE_THE_ISSUE_IS_RESOLVED arg1 -> \a -> (a @~ we_hope_the_issue_is_resolved) arg1
    PLEASE_SELECT_THE_RIDE_TO_CALL_DRIVER -> \a -> a @~ please_select_the_ride_to_call_driver
    ADD_IMAGE_S -> \a -> a @~ add_image_s
    ALREADY_HAVE_AN_ACTIVE_RIDE -> \a -> a @~ already_have_an_active_ride
    CONFIRM_STOP_LOCATION -> \a -> a @~ confirm_stop_location
    CONFIRM_DROP -> \a -> a @~ confirm_drop
    BOOK_METRO_WITH_NY_NOW -> \a -> a @~ book_metro_with_ny_now
    LEARN_ABOUT_NAMMA_SAFETY -> \a -> a @~ learn_about_namma_safety
    NAMMA_SAFETY_WILL_ENABLE_ACCESS -> \a -> a @~ namma_safety_will_enable_access
    EDIT_ACTIONS -> \a -> a @~ edit_actions
    EMERGENCY_ACTIONS -> \a -> a @~ emergency_actions
    WHEN_YOU_START_EMERGENCY_SOS -> \a -> a @~ when_you_start_emergency_sos
    RIDE_SHARE_AFTER_SIX_PM -> \a -> a @~ ride_share_after_six_pm
    WHO_CAN_TRACK_YOUR_RIDE arg1 -> \a -> (a @~ who_can_track_your_ride) arg1
    EMERGENCY_SHARING_WITH_CONTACTS -> \a -> a @~ emergency_sharing_with_contacts
    SHARING_WITH -> \a -> a @~ sharing_with
    ADD_A_CONTACT -> \a -> a @~ add_a_contact
    TO_ENSURE_SAFETY_USERS_SHOULD -> \a -> a @~ to_ensure_safety_users_should
    ABOUT_SOS_DESC -> \a -> a @~ about_sos_desc
    FEW_EXAMPLES_OF_SOS_SITUATIONS -> \a -> a @~ few_examples_of_sos_situations
    THINGS_TO_DO_DURING_SOS_SITUATION -> \a -> a @~ things_to_do_during_sos_situation
    EMERGENCY_REQUEST_SENT -> \a -> a @~ emergency_request_sent
    SOS_TRIGGERED_DESC -> \a -> a @~ sos_triggered_desc
    SOS_ACTIONS -> \a -> a @~ sos_actions
    CALL_POLICE -> \a -> a @~ call_police
    CALL_SUPPORT -> \a -> a @~ call_support
    RECORD_VIDEO -> \a -> a @~ record_video
    STOP_AND_SHARE_RECORDING -> \a -> a @~ stop_and_share_recording
    CANCEL_SHARING -> \a -> a @~ cancel_sharing
    START_RECORDING -> \a -> a @~ start_recording
    SHARING_THE_VIDEO_IN -> \a -> a @~ sharing_the_video_in
    EMERGENCY_INFO_SHARED -> \a -> a @~ emergency_info_shared
    EMERGENCY_INFO_SHARED_ACTION -> \a -> a @~ emergency_info_shared_action
    SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS -> \a -> a @~ set_up_your_personal_safety_settings
    ACTIVATE_LIVE_VIDEO_RECORDING_FEATURES -> \a -> a @~ activate_live_video_recording_features
    CHOOSE_RESPONSIVE_CONTACTS -> \a -> a @~ choose_responsive_contacts
    SHARE_LOCATION_AND_RIDE_DETAILS_EMERGENCY_CONTACT -> \a -> a @~ share_location_and_ride_details_emergency_contact
    NAMMA_SAFETY_MEASURES -> \a -> a @~ namma_safety_measures
    SAFETY_GUIDELINES_FOR_YOU -> \a -> a @~ safety_guidelines_for_you
    ABOUT_SOS -> \a -> a @~ about_sos
    NIGHT_TIME_SAFETY_CHECKS -> \a -> a @~ night_time_safety_checks
    SHARE_INFO_WITH_EMERGENCY_CONTACTS_TITLE -> \a -> a @~ share_info_with_emergency_contacts_title
    SHARE_INFO_WITH_EMERGENCY_CONTACTS_DESC -> \a -> a @~ share_info_with_emergency_contacts_desc
    TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE arg1 -> \a -> (a @~ trigger_alert_to_nammayatri_support_title) arg1
    TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_DESC -> \a -> a @~ trigger_alert_to_nammayatri_support_desc
    ENABLE_NIGHT_TIME_SAFETY_ALERTS_TITLE -> \a -> a @~ enable_night_time_safety_alerts_title
    ENABLE_NIGHT_TIME_SAFETY_ALERTS_DESC -> \a -> a @~ enable_night_time_safety_alerts_desc
    ALMOST_DONE_TITLE -> \a -> a @~ almost_done_title
    ALMOST_DONE_DESC -> \a -> a @~ almost_done_desc
    SAFETY_MEASURE_1 -> \a -> a @~ safety_measure_1
    SAFETY_MEASURE_2 -> \a -> a @~ safety_measure_2
    SAFETY_MEASURE_3 -> \a -> a @~ safety_measure_3
    SAFETY_MEASURE_4 -> \a -> a @~ safety_measure_4
    SAFETY_MEASURE_5 arg1 -> \a -> (a @~ safety_measure_5) arg1
    SAFETY_MEASURE_6 -> \a -> a @~ safety_measure_6
    SAFETY_GUIDELINES_1 -> \a -> a @~ safety_guidelines_1
    SAFETY_GUIDELINES_2 -> \a -> a @~ safety_guidelines_2
    SAFETY_GUIDELINES_3 -> \a -> a @~ safety_guidelines_3
    SAFETY_GUIDELINES_4 -> \a -> a @~ safety_guidelines_4
    SAFETY_GUIDELINES_5 -> \a -> a @~ safety_guidelines_5
    SAFETY_GUIDELINES_6 -> \a -> a @~ safety_guidelines_6
    SAFETY_GUIDELINES_7 -> \a -> a @~ safety_guidelines_7
    ABOUT_SOS_1 -> \a -> a @~ about_sos_1
    ABOUT_SOS_2 -> \a -> a @~ about_sos_2
    ABOUT_SOS_3 -> \a -> a @~ about_sos_3
    ABOUT_SOS_4 -> \a -> a @~ about_sos_4
    ABOUT_SOS_5 -> \a -> a @~ about_sos_5
    ABOUT_SOS_6 -> \a -> a @~ about_sos_6
    ABOUT_SOS_7 -> \a -> a @~ about_sos_7
    ABOUT_SOS_8 -> \a -> a @~ about_sos_8
    ABOUT_SOS_9 -> \a -> a @~ about_sos_9
    ABOUT_SOS_10 -> \a -> a @~ about_sos_10
    ABOUT_SOS_11 arg1 -> \a -> (a @~ about_sos_11) arg1
    ABOUT_SOS_12 -> \a -> a @~ about_sos_12
    ABOUT_SOS_13 -> \a -> a @~ about_sos_13
    THE_VIDEO_WILL_BE_RECORDED -> \a -> a @~ the_video_will_be_recorded
    EMERGENCY_VIDEO -> \a -> a @~ emergency_video
    NAMMA_SAFETY_IS_SET_UP -> \a -> a @~ namma_safety_is_set_up
    PERSONAL_SAFETY_SETTINGS_PERMISSION_REQUEST -> \a -> a @~ personal_safety_settings_permission_request
    ACTIVATE_NAMMA_SAFETY_POPUP_TITLE -> \a -> a @~ activate_namma_safety_popup_title
    ACTIVATE_NAMMA_SAFETY_POPUP_DESC -> \a -> a @~ activate_namma_safety_popup_desc
    ACTIVATE_NAMMA_SAFETY_POPUP_ACTION -> \a -> a @~ activate_namma_safety_popup_action
    DISMISS -> \a -> a @~ dismiss
    SEND_SILENT_SOS_TO_POLICE -> \a -> a @~ send_silent_sos_to_police
    OUR_SAFETY_PARTNER -> \a -> a @~ our_safety_partner
    BANGALURU_CITY_POLICE -> \a -> a @~ bangaluru_city_police
    GET_OPTIONS_TO_DIRECTLY_CALL_POLICE -> \a -> a @~ get_options_to_directly_call_police
    SHARE_SOS_SILENTLY_WITH_POLICE -> \a -> a @~ share_sos_silently_with_police
    CALL_AND_ALERT_THE_NEAREST_POLICE_CENTRE -> \a -> a @~ call_and_alert_the_nearest_police_centre
    SEND_A_SILENT_SOS_TO_THE_POLICE -> \a -> a @~ send_a_silent_sos_to_the_police
    SEND_A_VIDEO_RECORDING_TO_POLICE -> \a -> a @~ send_a_video_recording_to_police
    PERSONAL_SAFETY_ACTION_1 -> \a -> a @~ personal_safety_action_1
    PERSONAL_SAFETY_ACTION_2 arg1 -> \a -> (a @~ personal_safety_action_2) arg1
    PERSONAL_SAFETY_ACTION_2_POLICE -> \a -> a @~ personal_safety_action_2_police
    PERSONAL_SAFETY_ACTION_3 -> \a -> a @~ personal_safety_action_3
    SEND_VIDEO_TO_POLICE -> \a -> a @~ send_video_to_police
    FINISH_SETUP -> \a -> a @~ finish_setup
    MARK_RIDE_AS_SAFE -> \a -> a @~ mark_ride_as_safe
    ACTIVATE_SOS -> \a -> a @~ activate_sos
    EMERGENCY_INFO_SHARED_ACTION_POLICE -> \a -> a @~ emergency_info_shared_action_police
    START_SETUP -> \a -> a @~ start_setup
    CALL_SUPPORT_FOR_SAFETY arg1 -> \a -> (a @~ call_support_for_safety) arg1
    WE_NOTICED_YOUR_RIDE_HASNT_MOVED -> \a -> a @~ we_noticed_your_ride_hasnt_moved
    WE_NOTICED_YOUR_RIDE_IS_ON_DIFFERENT_ROUTE -> \a -> a @~ we_noticed_your_ride_is_on_different_route
    WE_ARE_HERE_FOR_YOU -> \a -> a @~ we_are_here_for_you
    I_NEED_HELP -> \a -> a @~ i_need_help
    I_FEEL_SAFE -> \a -> a @~ i_feel_safe
    EVERYTHING_OKAY_Q -> \a -> a @~ everything_okay_q
    PLEASE_REMAIN_CALM_YOU_CAN_REQUEST_AN_IMMEDIATE_CALL -> \a -> a @~ please_remain_calm_you_can_request_an_immediate_call
    RECEIVE_CALL_FROM_SUPPORT -> \a -> a @~ receive_call_from_support
    ADD_CONTACTS -> \a -> a @~ add_contacts
    ADD_CONTACTS_MANUALLY -> \a -> a @~ add_contacts_manually
    VIDEO_SHARE_INFO_TO_POLICE -> \a -> a @~ video_share_info_to_police
    CALL_POLICE_HELPLINE -> \a -> a @~ call_police_helpline
    PLEASE_REMAIN_CALM_CALL_POLICE -> \a -> a @~ please_remain_calm_call_police
    PLEASE_ALLOW_CAMERA_AND_MICROPHONE_PERMISSIONS -> \a -> a @~ please_allow_camera_and_microphone_permissions
    ACTIVATE_NAMMA_SAFETY_WILL_ENABLE_ACCESS -> \a -> a @~ activate_namma_safety_will_enable_access
    SELECT_PREFERRED_CONTACTS -> \a -> a @~ select_preferred_contacts
    NEW -> \a -> a @~ new
    SAFETY_CENTER -> \a -> a @~ safety_center
    EMERGENCY_SOS -> \a -> a @~ emergency_sos
    AUTOMATIC_CALL_PLACED_TO_EMERGENCY_CONTACTS -> \a -> a @~ automatic_call_placed_to_emergency_contacts
    EMERGENCY_CONTACTS_CAN_FOLLOW arg1 -> \a -> (a @~ emergency_contacts_can_follow) arg1
    ALERT_SAFETY_TEAM arg1 -> \a -> (a @~ alert_safety_team) arg1
    OPTION_TO_REPORT_A_SAFETY_ISSUE -> \a -> a @~ option_to_report_a_safety_issue
    RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL arg1 -> \a -> (a @~ recommend_emergency_contacts_to_install) arg1
    TEST_SAFETY_DRILL -> \a -> a @~ test_safety_drill
    START_TEST_DRILL -> \a -> a @~ start_test_drill
    REPORT_SAFETY_ISSUE -> \a -> a @~ report_safety_issue
    SAFETY_TEAM_WILL_BE_ALERTED arg1 -> \a -> (a @~ safety_team_will_be_alerted) arg1
    EMERGENCY_CONTACTS_CAN_TAKE_ACTION arg1 -> \a -> (a @~ emergency_contacts_can_take_action) arg1
    SHARE_RIDE -> \a -> a @~ share_ride
    SHARE_RIDE_DESCRIPTION arg1 -> \a -> (a @~ share_ride_description) arg1
    SHARE_RIDE_WITH_CONTACT arg1 -> \a -> (a @~ share_ride_with_contact) arg1
    SHARE_LINK -> \a -> a @~ share_link
    GLAD_TO_KNOW_YOU_ARE_SAFE -> \a -> a @~ glad_to_know_you_are_safe
    PLEASE_STAY_CALM_TEAM_ALERTED arg1 -> \a -> (a @~ please_stay_calm_team_alerted) arg1
    TRY_ANOTHER_CONTACT -> \a -> a @~ try_another_contact
    YOUR_CURRENT_LOCATION -> \a -> a @~ your_current_location
    THIS_IS_NOT_A_REAL_SOS_SITUATION -> \a -> a @~ this_is_not_a_real_sos_situation
    YOUR_VEHICLE_INFO -> \a -> a @~ your_vehicle_info
    POLICE_VIEW_INSTRUCTION -> \a -> a @~ police_view_instruction
    TEST_SOS_ACTIVATING_IN -> \a -> a @~ test_sos_activating_in
    SOS -> \a -> a @~ sos
    TEST_SOS -> \a -> a @~ test_sos
    SELECT_CONTACT_TO_CALL -> \a -> a @~ select_contact_to_call
    EMERGENCY_SOS_ACTIVATING -> \a -> a @~ emergency_sos_activating
    PRESS_TO_START_TEST_DRILL -> \a -> a @~ press_to_start_test_drill
    PRESS_IN_CASE_OF_EMERGENCY -> \a -> a @~ press_in_case_of_emergency
    INFORM_EMERGENCY_CONTACTS -> \a -> a @~ inform_emergency_contacts
    AVAILABLE_IN_REAL_EMERGENCY -> \a -> a @~ available_in_real_emergency
    OTHER_SAFETY_ACTIONS -> \a -> a @~ other_safety_actions
    DISCLAIMER -> \a -> a @~ disclaimer
    USE_ONLY_IN_EMERGENCY -> \a -> a @~ use_only_in_emergency
    MISUSE_MAY_LEAD_TO_LEGAL_ACTION -> \a -> a @~ misuse_may_lead_to_legal_action
    USE_TEST_DRILL -> \a -> a @~ use_test_drill
    INDICATION_TO_EMERGENCY_CONTACTS arg1 -> \a -> (a @~ indication_to_emergency_contacts) arg1
    ARE_YOU_READY_TO_START_DRILL -> \a -> a @~ are_you_ready_to_start_drill
    TEST_DRILL_DESC -> \a -> a @~ test_drill_desc
    LEARN_ABOUT_SAFETY_MODE -> \a -> a @~ learn_about_safety_mode
    TEST_EMERGENCY_REQUEST_SENT -> \a -> a @~ test_emergency_request_sent
    TEST_SOS_TRIGGERED_DESC -> \a -> a @~ test_sos_triggered_desc
    SOS_WILL_BE_DISABLED -> \a -> a @~ sos_will_be_disabled
    DIAL_NOW -> \a -> a @~ dial_now
    FOLLOWING arg1 -> \a -> (a @~ following) arg1
    TURN_OFF_ALARM -> \a -> a @~ turn_off_alarm
    CHOOSE_A_PERSON_TO_FOLLOW -> \a -> a @~ choose_a_person_to_follow
    IS_IN_SOS_SITUATION arg1 -> \a -> (a @~ is_in_sos_situation) arg1
    MARKED_RIDE_SAFE arg1 -> \a -> (a @~ marked_ride_safe) arg1
    STAY_CALM_KEEP_TRACKING arg1 -> \a -> (a @~ stay_calm_keep_tracking) arg1
    YOU_WILL_BE_NOTIFIED -> \a -> a @~ you_will_be_notified
    TAP_HERE_TO_FOLLOW arg1 -> \a -> (a @~ tap_here_to_follow) arg1
    HAVE_SHARED_RIDE_WITH_YOU arg1 -> \a -> (a @~ have_shared_ride_with_you) arg1
    SOS_LOCATION -> \a -> a @~ sos_location
    THIS_IS_A_TEST_MOCK_DRILL arg1 -> \a -> (a @~ this_is_a_test_mock_drill) arg1
    THIS_IS_NOT_REAL_DRILL -> \a -> a @~ this_is_not_real_drill
    REACHED_DESTINATION_SAFELY arg1 -> \a -> (a @~ reached_destination_safely) arg1
    RIDE_ENDED arg1 -> \a -> (a @~ ride_ended) arg1
    COMPLETE_YOUR_TEST_DRILL -> \a -> a @~ complete_your_test_drill
    TEST_DRILL -> \a -> a @~ test_drill
    RIDE_SHARED_WITH_SELECTED_CONTACTS -> \a -> a @~ ride_shared_with_selected_contacts
    TERMS_AND_CONDITIONS_UPDATED -> \a -> a @~ terms_and_conditions_updated
    OKAY -> \a -> a @~ okay
    TRY_LATER -> \a -> a @~ try_later
    REFERRAL_CODE_IS_APPLIED -> \a -> a @~ referral_code_is_applied
    YOU_HAVE_ALREADY_USED_DIFFERENT_REFERRAL_CODE -> \a -> a @~ you_have_already_used_different_referral_code
    INVALID_REFERRAL_CODE -> \a -> a @~ invalid_referral_code
    STOPS -> \a -> a @~ stops
    GREEN_LINE -> \a -> a @~ green_line
    BLUE_LINE -> \a -> a @~ blue_line
    RED_LINE -> \a -> a @~ red_line
    VIEW_ROUTE_INFO -> \a -> a @~ view_route_info
    VALID_UNTIL -> \a -> a @~ valid_until
    TICKET_NUMBER -> \a -> a @~ ticket_number
    TICKET -> \a -> a @~ ticket
    TICKETS -> \a -> a @~ tickets
    ONWORD_JOURNEY -> \a -> a @~ onword_journey
    ROUND_TRIP_STR -> \a -> a @~ round_trip_str
    TICKETS_FOR_CHENNAI_METRO -> \a -> a @~ tickets_for_chennai_metro
    ACTIVE_STR -> \a -> a @~ active_str
    EXPIRED_STR -> \a -> a @~ expired_str
    USED_STR -> \a -> a @~ used_str
    MAP_STR -> \a -> a @~ map_str
    TICKET_DETAILS -> \a -> a @~ ticket_details
    ROUTE_DETAILS -> \a -> a @~ route_details
    UNCERTAIN_ABOUT_METRO_ROUTES -> \a -> a @~ uncertain_about_metro_routes
    SEE_MAP -> \a -> a @~ see_map
    CHENNAI_METRO_TERM_1 -> \a -> a @~ chennai_metro_term_1
    CHENNAI_METRO_TERM_2 -> \a -> a @~ chennai_metro_term_2
    CHENNAI_METRO_TERM_EVENT -> \a -> a @~ chennai_metro_term_event
    FREE_TICKET_CASHBACK -> \a -> a @~ free_ticket_cashback
    NO_OF_PASSENGERS -> \a -> a @~ no_of_passengers
    MAXIMUM -> \a -> a @~ maximum
    TICKETS_ALLOWED_PER_USER -> \a -> a @~ tickets_allowed_per_user
    STARTING_FROM -> \a -> a @~ starting_from
    FROM -> \a -> a @~ from
    TO -> \a -> a @~ to
    BOOKING_ID -> \a -> a @~ booking_id
    PLEASE_WHILE_GEN_TICKET -> \a -> a @~ please_while_gen_ticket
    PAYMENT_RECEIVED -> \a -> a @~ payment_received
    PLEASE_CHECK_BACK_FEW_MIN -> \a -> a @~ please_check_back_few_min
    YOUR_BOOKING_PENDING -> \a -> a @~ your_booking_pending
    PLEASE_RETRY_BOOKING -> \a -> a @~ please_retry_booking
    BOOKING_FAILED -> \a -> a @~ booking_failed
    INCASE_OF_FAIL -> \a -> a @~ incase_of_fail
    REFRESH_STATUS -> \a -> a @~ refresh_status
    DATE -> \a -> a @~ date
    NO_OF_TICKETS -> \a -> a @~ no_of_tickets
    ACTIVE_TICKETS -> \a -> a @~ active_tickets
    CONFIRMING_STR -> \a -> a @~ confirming_str
    FAILED_STR -> \a -> a @~ failed_str
    CONFIRMED_STR -> \a -> a @~ confirmed_str
    BUY_METRO_TICKETS -> \a -> a @~ buy_metro_tickets
    GET_FARE -> \a -> a @~ get_fare
    METRO_BOOKING_TIMINGS -> \a -> a @~ metro_booking_timings
    CHENNAI_METRO_TIME arg1 arg2 -> \a -> (a @~ chennai_metro_time) arg1 arg2
    DELHI_METRO_TIME arg1 arg2 -> \a -> (a @~ delhi_metro_time) arg1 arg2
    PLEASE_COME_BACK_LATER_METRO -> \a -> a @~ please_come_back_later_metro
    NO_QOUTES_AVAILABLE -> \a -> a @~ no_qoutes_available
    I_AGREE_TO_THE -> \a -> a @~ i_agree_to_the
    HERE_IS_METRO_TICKET -> \a -> a @~ here_is_metro_ticket
    VIEW_TICKET -> \a -> a @~ view_ticket
    DESTINATION -> \a -> a @~ destination
    PAY -> \a -> a @~ pay
    PENDING_STR -> \a -> a @~ pending_str
    PAST_TICKETS -> \a -> a @~ past_tickets
    ONE_WAY_STR -> \a -> a @~ one_way_str
    SHARE_TICKET -> \a -> a @~ share_ticket
    ORIGIN -> \a -> a @~ origin
    HISTORY -> \a -> a @~ history
    ALWAYS -> \a -> a @~ always
    ALWAYS_SHARE_DESC -> \a -> a @~ always_share_desc
    NIGHT_RIDES_SHARE -> \a -> a @~ night_rides_share
    NIGHT_RIDES_DESC -> \a -> a @~ night_rides_desc
    NEVER -> \a -> a @~ never
    NEVER_SHARE_DESC -> \a -> a @~ never_share_desc
    SHARE_TRIP_NOTIFICATONS -> \a -> a @~ share_trip_notificatons
    CALL_CUSTOMER_SUPPORT -> \a -> a @~ call_customer_support
    YET_TO_START arg1 -> \a -> (a @~ yet_to_start) arg1
    MESSAGE_FROM arg1 -> \a -> (a @~ message_from) arg1
    RIDE_CANCELLED -> \a -> a @~ ride_cancelled
    TRACK_RIDE_STRING arg1 arg2 arg3 arg4 -> \a -> (a @~ track_ride_string) arg1 arg2 arg3 arg4
    SAFETY_CENTER_IS_DISABLED -> \a -> a @~ safety_center_is_disabled
    TRACK_ON_GOOGLE_MAP -> \a -> a @~ track_on_google_map
    SHOW_WALKING_DIRECTION -> \a -> a @~ show_walking_direction
    SPECIAL_PICKUP_ZONE -> \a -> a @~ special_pickup_zone
    SPECIAL_PICKUP_ZONE_RIDE -> \a -> a @~ special_pickup_zone_ride
    WE_WILL_TRY_TO_CONNECT_YOU_WITH_DRIVER_IN_CLOSEST_PICKUP_ZONE -> \a -> a @~ we_will_try_to_connect_you_with_driver_in_closest_pickup_zone
    THIS_PROVIDES_YOU_AN_INSTANT_PICKUP_EXPERIENCE -> \a -> a @~ this_provides_you_an_instant_pickup_experience
    DRIVER_AT_PICKUP_LOCATION -> \a -> a @~ driver_at_pickup_location
    DRIVER_ALMOST_AT_PICKUP -> \a -> a @~ driver_almost_at_pickup
    MAXIMUM_EDIT_PICKUP_ATTEMPTS_REACHED -> \a -> a @~ maximum_edit_pickup_attempts_reached
    MOVE_PIN_TO_THE_DESIRED_PICKUP_POINT -> \a -> a @~ move_pin_to_the_desired_pickup_point
    CHANGE_PICKUP_LOCATION -> \a -> a @~ change_pickup_location
    LOCATION_IS_TOO_FAR -> \a -> a @~ location_is_too_far
    A_TIP_HELPS_FIND_A_RIDE_QUICKER -> \a -> a @~ a_tip_helps_find_a_ride_quicker
    TIP_ADDED -> \a -> a @~ tip_added
    CONTINUE_SEARCH_WITH_NO_TIP -> \a -> a @~ continue_search_with_no_tip
    SEARCHING_WITH_NO_TIP -> \a -> a @~ searching_with_no_tip
    SEARCH_AGAIN -> \a -> a @~ search_again
    DRIVER_IS_ON_THE_WAY -> \a -> a @~ driver_is_on_the_way
    DRIVER_IS_WAITING_AT_PICKUP -> \a -> a @~ driver_is_waiting_at_pickup
    IS_AT_PICKUP_LOCATION -> \a -> a @~ is_at_pickup_location
    GO_TO_SELECTED_SPOT_FOR_PICKUP -> \a -> a @~ go_to_selected_spot_for_pickup
    SELECT_POPULAR_SPOT_FOR_HASSLE_FREE_PICKUP -> \a -> a @~ select_popular_spot_for_hassle_free_pickup
    TICKET_IS_NON_CANCELLABLE -> \a -> a @~ ticket_is_non_cancellable
    CANCEL_BOOKING -> \a -> a @~ cancel_booking
    BOOKING_NOT_CANCELLABLE -> \a -> a @~ booking_not_cancellable
    BOOKINGS_WILL_BE_CANCELLED -> \a -> a @~ bookings_will_be_cancelled
    BOOKINGS_WILL_BE_CANCELLED_WITH_REFUND arg1 -> \a -> (a @~ bookings_will_be_cancelled_with_refund) arg1
    REFUND_NOT_APPLICABLE -> \a -> a @~ refund_not_applicable
    YES_CANCEL_BOOKING -> \a -> a @~ yes_cancel_booking
    WOULD_YOU_LIKE_TO_PROCEED -> \a -> a @~ would_you_like_to_proceed
    BOOKING_CANCELLED -> \a -> a @~ booking_cancelled
    REFUND_IS_IN_PROCESS -> \a -> a @~ refund_is_in_process
    TOTAL_REFUND -> \a -> a @~ total_refund
    NUMBER_OF_TICKETS -> \a -> a @~ number_of_tickets
    CANCELLATION_DATE -> \a -> a @~ cancellation_date
    TICKETS_FOR_KOCHI_METRO -> \a -> a @~ tickets_for_kochi_metro
    YOUR_BOOKED_TICKETS -> \a -> a @~ your_booked_tickets
    PLAN_YOUR_JOURNEY -> \a -> a @~ plan_your_journey
    BOOK_ROUND_TRIP -> \a -> a @~ book_round_trip
    BY_PROCEEDING_YOU_AGREE -> \a -> a @~ by_proceeding_you_agree
    TERMS_AND_CONDITIONS_FULL -> \a -> a @~ terms_and_conditions_full
    EXPERIENCE_HASSLE_FREE_METRO_BOOKING arg1 -> \a -> (a @~ experience_hassle_free_metro_booking) arg1
    KOCHI_METRO_TERM_1 -> \a -> a @~ kochi_metro_term_1
    KOCHI_METRO_TERM_2 -> \a -> a @~ kochi_metro_term_2
    KOCHI_METRO_TIME arg1 arg2 -> \a -> (a @~ kochi_metro_time) arg1 arg2
    BOOK_TICKET -> \a -> a @~ book_ticket
    PREPARE_EMERGENCY_CONTACTS -> \a -> a @~ prepare_emergency_contacts
    EMERGENCY_CONTACTS_WILL_BE_NOTIFIED -> \a -> a @~ emergency_contacts_will_be_notified
    INFORM_EMERGENCY_CONTACTS_ABOUT_TEST -> \a -> a @~ inform_emergency_contacts_about_test
    RECENT_RIDE_ISSUE_DESC -> \a -> a @~ recent_ride_issue_desc
    I_NEED_HELP_WITH_MY_RECENT_RIDE -> \a -> a @~ i_need_help_with_my_recent_ride
    CONTINUE_WITH_SAFETY_SETTINGS -> \a -> a @~ continue_with_safety_settings
    TAP_WHERE_TO_TO_BOOK_RIDE -> \a -> a @~ tap_where_to_to_book_ride
    LAST_CHOSEN_VARIANT_NOT_AVAILABLE -> \a -> a @~ last_chosen_variant_not_available
    TOLL_CHARGES -> \a -> a @~ toll_charges
    TOLL_CHARGES_DESC -> \a -> a @~ toll_charges_desc
    TOLL_CHARGES_INCLUDING arg1 -> \a -> (a @~ toll_charges_including) arg1
    TOLL_ROAD_CHANGED -> \a -> a @~ toll_road_changed
    PARKING_CHARGE -> \a -> a @~ parking_charge
    TOLL_OR_PARKING_CHARGES -> \a -> a @~ toll_or_parking_charges
    TOLL_CHARGES_ESTIMATED -> \a -> a @~ toll_charges_estimated
    ADD_TIP -> \a -> a @~ add_tip
    CHANGE_RIDE_TYPE -> \a -> a @~ change_ride_type
    TRY_ADDING_TIP_OR_CHANGE_RIDE_TYPE -> \a -> a @~ try_adding_tip_or_change_ride_type
    APPLICABLE_TOLL_CHARGES -> \a -> a @~ applicable_toll_charges
    UPDATE_TIP_STR -> \a -> a @~ update_tip_str
    BOOK arg1 -> \a -> (a @~ book) arg1
    FARE_FOR arg1 -> \a -> (a @~ fare_for) arg1
    WAITING_CHARGE_LIMIT arg1 -> \a -> (a @~ waiting_charge_limit) arg1
    TIME_TAKEN -> \a -> a @~ time_taken
    TRIP_DISTANCE -> \a -> a @~ trip_distance
    UNABLE_TO_CANCEL_RIDE -> \a -> a @~ unable_to_cancel_ride
    GOT_A_REFERRAL_FROM_A_DRIVER_OR_FRIEND -> \a -> a @~ got_a_referral_from_a_driver_or_friend
    ENTER_REFERRAL_CODE_ -> \a -> a @~ enter_referral_code_
    REFERRED_USERS -> \a -> a @~ referred_users
    SHOW_APP_QR -> \a -> a @~ show_app_qr
    SHARE_AND_REFER -> \a -> a @~ share_and_refer
    YOUR_REFERRAL_CODE -> \a -> a @~ your_referral_code
    REFER_YOUR_FRIENDS -> \a -> a @~ refer_your_friends
    REFERRALS -> \a -> a @~ referrals
    ENTER_NOW -> \a -> a @~ enter_now
    WHAT_IS_REFERRAL_PROGRAM -> \a -> a @~ what_is_referral_program
    USERS_WHO_DOWNLOAD_APP_AND_COMPLETE_THEIR_FIRST_RIDE_USING_REFERRAL_CODE arg1 -> \a -> (a @~ users_who_download_app_and_complete_their_first_ride_using_referral_code) arg1
    THE_REFERRAL_PROGRAM_INCENTIVISES_DRIVERS_TO_ACCEPT_MORE_RIDES arg1 -> \a -> (a @~ the_referral_program_incentivises_drivers_to_accept_more_rides) arg1
    INVALID_CODE -> \a -> a @~ invalid_code
    ENTER_6_DIGIT_REFERRAL_CODE_BELOW -> \a -> a @~ enter_6_digit_referral_code_below
    APPLY -> \a -> a @~ apply
    TOLL_CHARGES_INCLUDED -> \a -> a @~ toll_charges_included
    ONE_TAP_BOOKINGS -> \a -> a @~ one_tap_bookings
    HAS_YOUR_DRIVER_SET_THE_AC_AS_PER_YOUR_PREFERENCE -> \a -> a @~ has_your_driver_set_the_ac_as_per_your_preference
    NO_REPORT_AN_ISSUE -> \a -> a @~ no_report_an_issue
    GREAT_ENJOY_THE_TRIP -> \a -> a @~ great_enjoy_the_trip
    ENJOY_YOUR_BUDGET_FRIENDLY_NON_AC_RIDE -> \a -> a @~ enjoy_your_budget_friendly_non_ac_ride
    AC_IS_NOT_AVAILABLE_ON_THIS_RIDE -> \a -> a @~ ac_is_not_available_on_this_ride
    AC_NOT_WORKING_DESC -> \a -> a @~ ac_not_working_desc
    SHOWING_FARE_FROM_MULTI_PROVIDER -> \a -> a @~ showing_fare_from_multi_provider
    LIVE_CHAT -> \a -> a @~ live_chat
    DRIVER_TIP_ADDITION -> \a -> a @~ driver_tip_addition
    LIVE_RIDE_SHARING -> \a -> a @~ live_ride_sharing
    ENHANCED_SAFETY -> \a -> a @~ enhanced_safety
    CONFIRM_PROVIDER -> \a -> a @~ confirm_provider
    SELECT_A_PROVIDER -> \a -> a @~ select_a_provider
    CONFIRMING_SELECTED_PROVIDER -> \a -> a @~ confirming_selected_provider
    BOOK_TOP_PROVIDER -> \a -> a @~ book_top_provider
    CHOOSE_FROM_PROVIDERS -> \a -> a @~ choose_from_providers
    CHOOSE_BETWEEN_PROVIDERS -> \a -> a @~ choose_between_providers
    CHOOSE_BETWEEN_PROVIDERS_DESC -> \a -> a @~ choose_between_providers_desc
    GUARANTEED_RIDE -> \a -> a @~ guaranteed_ride
    THIS_RIDE_FULFILLED_BY arg1 -> \a -> (a @~ this_ride_fulfilled_by) arg1
    ADDITIONAL_FEATURES_ON arg1 -> \a -> (a @~ additional_features_on) arg1
    NOTIFY_YOUR_EC -> \a -> a @~ notify_your_ec
    EC_CAN_RESPOND -> \a -> a @~ ec_can_respond
    QUICK_SUPPORT arg1 -> \a -> (a @~ quick_support) arg1
    LEARN_ABOUT_APP_SAFETY_FEAT arg1 -> \a -> (a @~ learn_about_app_safety_feat) arg1
    OTHER_PROVIDER_NO_RECEIPT -> \a -> a @~ other_provider_no_receipt
    RIDE_FULFILLED_BY arg1 -> \a -> (a @~ ride_fulfilled_by) arg1
    CONGESTION_CHARGES -> \a -> a @~ congestion_charges
    TIP_CAN_BE_ADDED arg1 -> \a -> (a @~ tip_can_be_added) arg1
    CONGESTION_CHARGES_DESC arg1 -> \a -> (a @~ congestion_charges_desc) arg1
    AC_TURNED_OFF -> \a -> a @~ ac_turned_off
    BOOK_ANY -> \a -> a @~ book_any
    ESTIMATES_EXPIRY_ERROR -> \a -> a @~ estimates_expiry_error
    ESTIMATES_EXPIRY_ERROR_AND_FETCH_AGAIN -> \a -> a @~ estimates_expiry_error_and_fetch_again
    PAY_YOUR_DRIVER_BY_CASH_OR_UPI -> \a -> a @~ pay_your_driver_by_cash_or_upi
    TRIP_DELAYED -> \a -> a @~ trip_delayed
    SELECT_VEHICLE -> \a -> a @~ select_vehicle
    BOOK_RENTAL -> \a -> a @~ book_rental
    CONFIRM_RENTAL -> \a -> a @~ confirm_rental
    RENTAL_RIDE -> \a -> a @~ rental_ride
    SELECT_DURATION -> \a -> a @~ select_duration
    SELECT_DISTANCE -> \a -> a @~ select_distance
    RENTAL_OPTIONS -> \a -> a @~ rental_options
    BOOKING_ON -> \a -> a @~ booking_on
    INCLUDED_KMS -> \a -> a @~ included_kms
    BASE_FARE -> \a -> a @~ base_fare
    TOLLS_AND_PARKING_FEES -> \a -> a @~ tolls_and_parking_fees
    FINAL_FARE_DESCRIPTION -> \a -> a @~ final_fare_description
    EXCESS_DISTANCE_CHARGE_DESCRIPTION arg1 -> \a -> (a @~ excess_distance_charge_description) arg1
    ADDITIONAL_CHARGES_DESCRIPTION -> \a -> a @~ additional_charges_description
    PARKING_FEES_AND_TOLLS_NOT_INCLUDED -> \a -> a @~ parking_fees_and_tolls_not_included
    NIGHT_TIME_FEE_DESCRIPTION -> \a -> a @~ night_time_fee_description
    CHOOSE_YOUR_RENTAL_RIDE -> \a -> a @~ choose_your_rental_ride
    FIRST_STOP_OPTIONAL -> \a -> a @~ first_stop_optional
    JANUARY -> \a -> a @~ january
    FEBRUARY -> \a -> a @~ february
    MARCH -> \a -> a @~ march
    APRIL -> \a -> a @~ april
    MAY -> \a -> a @~ may
    JUNE -> \a -> a @~ june
    JULY -> \a -> a @~ july
    AUGUST -> \a -> a @~ august
    SEPTEMBER -> \a -> a @~ september
    OCTOBER -> \a -> a @~ october
    NOVEMBER -> \a -> a @~ november
    DECEMBER -> \a -> a @~ december
    HOURS -> \a -> a @~ hours
    NOT_ADDED_YET -> \a -> a @~ not_added_yet
    NEXT_STOP -> \a -> a @~ next_stop
    TIME -> \a -> a @~ time
    DISTANCE -> \a -> a @~ distance
    STARTING_ODO -> \a -> a @~ starting_odo
    END_OTP -> \a -> a @~ end_otp
    ONLY_LOCATION_WITHIN_CITY_LIMITS -> \a -> a @~ only_location_within_city_limits
    RIDE_TIME -> \a -> a @~ ride_time
    RIDE_DISTANCE -> \a -> a @~ ride_distance
    RIDE_STARTED_AT -> \a -> a @~ ride_started_at
    RIDE_ENDED_AT -> \a -> a @~ ride_ended_at
    ESTIMATED_FARE -> \a -> a @~ estimated_fare
    EXTRA_TIME_FARE -> \a -> a @~ extra_time_fare
    TOTAL_FARE -> \a -> a @~ total_fare
    FARE_UPDATE -> \a -> a @~ fare_update
    NOW -> \a -> a @~ now
    DATE_INVALID_MESSAGE -> \a -> a @~ date_invalid_message
    EDIT_PICKUP -> \a -> a @~ edit_pickup
    ADD_STOP -> \a -> a @~ add_stop
    ENTER_PICKUP_LOC -> \a -> a @~ enter_pickup_loc
    INTERCITY_OPTIONS -> \a -> a @~ intercity_options
    PROCEED -> \a -> a @~ proceed
    SCHEDULE_RIDE_AVAILABLE -> \a -> a @~ schedule_ride_available
    RENTAL_RIDE_UNTIL -> \a -> a @~ rental_ride_until
    EXTRA_TIME_CHARGES -> \a -> a @~ extra_time_charges
    DIST_BASED_CHARGES -> \a -> a @~ dist_based_charges
    TIME_BASED_CHARGES -> \a -> a @~ time_based_charges
    RENTAL_POLICY -> \a -> a @~ rental_policy
    SELECT_PACKAGE -> \a -> a @~ select_package
    RENTAL_POLICY_DESC -> \a -> a @~ rental_policy_desc
    RENTAL_POLICY_DESC_1 -> \a -> a @~ rental_policy_desc_1
    RENTALS_INTERCITY_AVAILABLE arg1 -> \a -> (a @~ rentals_intercity_available) arg1
    CHECK_IT_OUT -> \a -> a @~ check_it_out
    FAILED_TO_CANCEL -> \a -> a @~ failed_to_cancel
    SCHEDULING_ALLOWED_IN_INTERCITY_RENTAL -> \a -> a @~ scheduling_allowed_in_intercity_rental
    SPECIAL_ZONE_INTERCITY_INELIGIBLE -> \a -> a @~ special_zone_intercity_ineligible
    NO_RIDES_SCHEDULED_YET -> \a -> a @~ no_rides_scheduled_yet
    RIDE_BOOKING -> \a -> a @~ ride_booking
    SPECIAL_ZONE_RENTAL_INELIGIBLE -> \a -> a @~ special_zone_rental_ineligible
    SERVICES -> \a -> a @~ services
    YOU_HAVE_UPCOMING_RENTAL_BOOKING arg1 -> \a -> (a @~ you_have_upcoming_rental_booking) arg1
    SCHEDULED -> \a -> a @~ scheduled
    UPCOMING_BOOKINGS -> \a -> a @~ upcoming_bookings
    RENTALS_ -> \a -> a @~ rentals_
    INTER_CITY_ -> \a -> a @~ inter_city_
    YOU_HAVE_UPCOMING_INTERCITY_BOOKING arg1 -> \a -> (a @~ you_have_upcoming_intercity_booking) arg1
    A_RIDE_ALREADY_EXISTS -> \a -> a @~ a_ride_already_exists
    YOU_HAVE_AN_RIDE_FROM_TO_SCHEDULED_FROM_TILL -> \a -> a @~ you_have_an_ride_from_to_scheduled_from_till
    EXTRA_PER_KM_FARE -> \a -> a @~ extra_per_km_fare
    EXTRA_PER_MINUTE_FARE -> \a -> a @~ extra_per_minute_fare
    PICKUP_CHARGES -> \a -> a @~ pickup_charges
    WAITING_CHARGES_AFTER_3_MINS -> \a -> a @~ waiting_charges_after_3_mins
    FARE_DETERMINED_AS_PER_KARNATAKA_GUIDELINES -> \a -> a @~ fare_determined_as_per_karnataka_guidelines
    RENTAL_CHARGES -> \a -> a @~ rental_charges
    RENTAL_INFO_POLICY_DESC arg1 -> \a -> (a @~ rental_info_policy_desc) arg1
    RENTAL_INFO_POLICY_DESC_ -> \a -> a @~ rental_info_policy_desc_
    RENTAL_SCREEN_EXPLAINER -> \a -> a @~ rental_screen_explainer
    INSTANT -> \a -> a @~ instant
    COMING_SOON -> \a -> a @~ coming_soon
    CANCEL_SCHEDULED_RIDE -> \a -> a @~ cancel_scheduled_ride
    CANCEL_SCHEDULED_RIDE_DESC -> \a -> a @~ cancel_scheduled_ride_desc
    CONFIRM_CANCELLATION -> \a -> a @~ confirm_cancellation
    INTERCITY_RIDES_COMING_SOON -> \a -> a @~ intercity_rides_coming_soon
    VIEW_FARES -> \a -> a @~ view_fares
    EXCESS_TIME_DESCRIPTION arg1 -> \a -> (a @~ excess_time_description) arg1
    ESTIMATED_CHARGES -> \a -> a @~ estimated_charges
    YOUR_CANCELLATION_RATE_IS_HIGH -> \a -> a @~ your_cancellation_rate_is_high
    AVOID_FURTHER_CANCELLATIONS_TO_KEEP_USING_APP arg1 -> \a -> (a @~ avoid_further_cancellations_to_keep_using_app) arg1
    NIGHT_TIME_FEES -> \a -> a @~ night_time_fees
    PARKING_AND_OTHER_CHARGES -> \a -> a @~ parking_and_other_charges
    ADDITIONAL_CHARGES -> \a -> a @~ additional_charges
    ESTIMATED_BASE_FARE -> \a -> a @~ estimated_base_fare
    INCLUDED_DISTANCE -> \a -> a @~ included_distance
    INCLUDED_TIME -> \a -> a @~ included_time
    TOLL_CHARGES_DESCRIPTION -> \a -> a @~ toll_charges_description
    WILL_BE_ADDED_TO_FINAL_FARE -> \a -> a @~ will_be_added_to_final_fare
    EXTRA_DISTANCE_FARE -> \a -> a @~ extra_distance_fare
    NETWORK_ERROR -> \a -> a @~ network_error
    SERVER_ERROR -> \a -> a @~ server_error
    UNKNOWN_ERROR -> \a -> a @~ unknown_error
    CONNECTION_REFUSED -> \a -> a @~ connection_refused
    TIMEOUT -> \a -> a @~ timeout
    WAS_TOLL_EXP_SMOOTH -> \a -> a @~ was_toll_exp_smooth
    WAS_TOLL_EXP_SMOOTH_DESC -> \a -> a @~ was_toll_exp_smooth_desc
    WAS_DRIVER_HELPFUL -> \a -> a @~ was_driver_helpful
    WAS_RIDE_SAFE_DESC -> \a -> a @~ was_ride_safe_desc
    WAS_RIDE_SAFE -> \a -> a @~ was_ride_safe
    WAS_DRIVER_HELPFUL_DESC -> \a -> a @~ was_driver_helpful_desc
    COLLECT_TOLL_SEP -> \a -> a @~ collect_toll_sep
    FINAL_FARE_EXCLUDES_TOLL -> \a -> a @~ final_fare_excludes_toll
    TOLL_CHARGES_MAYBE_APPLICABLE -> \a -> a @~ toll_charges_maybe_applicable
    METRO_BANNER_TITLE arg1 -> \a -> (a @~ metro_banner_title) arg1
    VIEW_ON_GOOGLE_MAPS -> \a -> a @~ view_on_google_maps
    WALKING_DIRECTIONS_TO_PICKUP -> \a -> a @~ walking_directions_to_pickup
    EXPLORE_CITY_WITH_US arg1 -> \a -> (a @~ explore_city_with_us) arg1
    GO_TO_DESTINATION arg1 -> \a -> (a @~ go_to_destination) arg1
    WALK_TO arg1 -> \a -> (a @~ walk_to) arg1
    BANGALORE -> \a -> a @~ bangalore
    KOLKATA -> \a -> a @~ kolkata
    PARIS -> \a -> a @~ paris
    KOCHI -> \a -> a @~ kochi
    DELHI -> \a -> a @~ delhi
    HYDERABAD -> \a -> a @~ hyderabad
    MUMBAI -> \a -> a @~ mumbai
    CHENNAI -> \a -> a @~ chennai
    COIMBATORE -> \a -> a @~ coimbatore
    PONDICHERRY -> \a -> a @~ pondicherry
    GOA -> \a -> a @~ goa
    PUNE -> \a -> a @~ pune
    MYSORE -> \a -> a @~ mysore
    TUMAKURU -> \a -> a @~ tumakuru
    NOIDA -> \a -> a @~ noida
    GURUGRAM -> \a -> a @~ gurugram
    WAITING_CHARGES -> \a -> a @~ waiting_charges
    QUOTES_EXPIRY_ERROR_AND_FETCH_AGAIN -> \a -> a @~ quotes_expiry_error_and_fetch_again
    PLACE_A_CALL -> \a -> a @~ place_a_call
    YOU_CAN_WRITE_TO_US_AT -> \a -> a @~ you_can_write_to_us_at
    CHARGEABLE -> \a -> a @~ chargeable
    BOOKED -> \a -> a @~ booked
    SURCHARGES -> \a -> a @~ surcharges
    SILIGURI -> \a -> a @~ siliguri
    KOZHIKODE -> \a -> a @~ kozhikode
    THRISSUR -> \a -> a @~ thrissur
    TRIVANDRUM -> \a -> a @~ trivandrum
    METRO_FREE_TICKET_EVENT arg1 -> \a -> (a @~ metro_free_ticket_event) arg1
    METRO_FREE_TICKET_EVENT_DESC arg1 arg2 -> \a -> (a @~ metro_free_ticket_event_desc) arg1 arg2
    NEXT_FREE_TICKET -> \a -> a @~ next_free_ticket
    FREE_TICKET_AVAILABLE arg1 arg2 -> \a -> (a @~ free_ticket_available) arg1 arg2
    ADDITIONAL_CHARGES_WILL_BE_APPLICABLE -> \a -> a @~ additional_charges_will_be_applicable
    PARKING_CHARGES_INCLUDED arg1 -> \a -> (a @~ parking_charges_included) arg1
    APP_TOLL_CHARGES -> \a -> a @~ app_toll_charges
    APP_PARKING_CHARGES -> \a -> a @~ app_parking_charges
    APP_TOLL_PARKING_CHARGES -> \a -> a @~ app_toll_parking_charges
    PARKING_CHARGES_DESC -> \a -> a @~ parking_charges_desc
    TOLL_CHARGES_INCLUDED_IN_FAIR -> \a -> a @~ toll_charges_included_in_fair
    PLEASE_DO_NOT_PAY_EXTRA_TO_DRIVER -> \a -> a @~ please_do_not_pay_extra_to_driver
    VELLORE -> \a -> a @~ vellore
    HOSUR -> \a -> a @~ hosur
    MADURAI -> \a -> a @~ madurai
    THANJAVUR -> \a -> a @~ thanjavur
    TIRUNELVELI -> \a -> a @~ tirunelveli
    SALEM -> \a -> a @~ salem
    TRICHY -> \a -> a @~ trichy
    DAVANAGERE -> \a -> a @~ davanagere
    SHIVAMOGGA -> \a -> a @~ shivamogga
    HUBLI -> \a -> a @~ hubli
    MANGALORE -> \a -> a @~ mangalore
    GULBARGA -> \a -> a @~ gulbarga
    UDUPI -> \a -> a @~ udupi
    CANCEL_BOOKING_ -> \a -> a @~ cancel_booking_
    CANCEL_INTERCITY_BOOKING -> \a -> a @~ cancel_intercity_booking
    RENTAL_BOOKING -> \a -> a @~ rental_booking
    INTERCITY_BOOKING -> \a -> a @~ intercity_booking
    BOOKING -> \a -> a @~ booking
    BY -> \a -> a @~ by
    CUSTOMERS -> \a -> a @~ customers
    RATING -> \a -> a @~ rating
    CANCELLATION -> \a -> a @~ cancellation
    TRIPS -> \a -> a @~ trips
    I_SPEAK -> \a -> a @~ i_speak
    AND -> \a -> a @~ and
    WITH_NAMMAYATRI_FOR arg1 -> \a -> (a @~ with_nammayatri_for) arg1
    YEARS -> \a -> a @~ years
    VEHICLE_NUMBER -> \a -> a @~ vehicle_number
    WHAT_PEOPLE_SAY -> \a -> a @~ what_people_say
    STAR_RATING -> \a -> a @~ star_rating
    CARD_TEXTS -> \a -> a @~ card_texts
    TRAININGS_I_COMPLETED -> \a -> a @~ trainings_i_completed
    I_PLEDGE -> \a -> a @~ i_pledge
    CLEAN_BIKE -> \a -> a @~ clean_bike
    UNCOMFORTABLE_BIKE -> \a -> a @~ uncomfortable_bike
    DRIVER_AVAILABLE -> \a -> a @~ driver_available
    DRIVERS_AVAILABLE -> \a -> a @~ drivers_available
    MORE_SAFETY_MEASURES -> \a -> a @~ more_safety_measures
    SAFETY_SETUP -> \a -> a @~ safety_setup
    COMPLETE -> \a -> a @~ complete
    TRUSTED_CONTACT_HELP -> \a -> a @~ trusted_contact_help
    DRIVER_SAFETY_STANDARDS -> \a -> a @~ driver_safety_standards
    TRUSTED_CONTACT -> \a -> a @~ trusted_contact
    TRUSTED_CONTACT_HIGHLIGHT -> \a -> a @~ trusted_contact_highlight
    SAFETY_DRILL -> \a -> a @~ safety_drill
    DEFAULT_CONTACT -> \a -> a @~ default_contact
    APP_CALL_CHAT -> \a -> a @~ app_call_chat
    TRUSTED_CONTACT_DESC -> \a -> a @~ trusted_contact_desc
    ENABLE_LIVE_TRACKING -> \a -> a @~ enable_live_tracking
    UNEXPECTED_EVENT_CHECK -> \a -> a @~ unexpected_event_check
    UNEXPECTED_EVENT_CHECK_DESC -> \a -> a @~ unexpected_event_check_desc
    UNEXPECTED_EVENT_CHECK_TIMINGS -> \a -> a @~ unexpected_event_check_timings
    NEXT -> \a -> a @~ next
    POST_RIDE_CHECK -> \a -> a @~ post_ride_check
    POST_RIDE_CHECK_DESC -> \a -> a @~ post_ride_check_desc
    POST_RIDE_CHECK_TIMINGS -> \a -> a @~ post_ride_check_timings
    SAFETY_TEAM_NOTIFICATION -> \a -> a @~ safety_team_notification
    NOTIFY_SAFETY_TEAM -> \a -> a @~ notify_safety_team
    NOTIFY_SAFETY_TEAM_SUB -> \a -> a @~ notify_safety_team_sub
    NOTIFY_SAFETY_TEAM_NOTE -> \a -> a @~ notify_safety_team_note
    EMERGENCY_SOS_NEW -> \a -> a @~ emergency_sos_new
    EMERGENCY_SOS_SUB -> \a -> a @~ emergency_sos_sub
    SHAKE_TO_ACTIVATE -> \a -> a @~ shake_to_activate
    SHAKE_TO_ACTIVATE_SUB arg1 -> \a -> (a @~ shake_to_activate_sub) arg1
    AUTOMATIC_CALL_SOS -> \a -> a @~ automatic_call_sos
    AUTOMATIC_CALL_SOS_SUB -> \a -> a @~ automatic_call_sos_sub
    PLACE_DEFAULT_CALL -> \a -> a @~ place_default_call
    DEFAULT_CALL_CONTACT -> \a -> a @~ default_call_contact
    DEFAULT_CONTACT_DESC -> \a -> a @~ default_contact_desc
    MORE_EMERGENCY_ACTIONS -> \a -> a @~ more_emergency_actions
    SIREN -> \a -> a @~ siren
    CALL_POLICE_DESC -> \a -> a @~ call_police_desc
    RECORD_AUDIO_DESC -> \a -> a @~ record_audio_desc
    SIREN_DESC -> \a -> a @~ siren_desc
    CALL_SAFETY_TEAM_DESC -> \a -> a @~ call_safety_team_desc
    SAFETY_DRILL_DESC -> \a -> a @~ safety_drill_desc
    SAFETY_DRILL_SUB -> \a -> a @~ safety_drill_sub
    SAFETY_DRILL_NOTE -> \a -> a @~ safety_drill_note
    RIDE_ACTIONS -> \a -> a @~ ride_actions
    RIDE_ACTIONS_SUB -> \a -> a @~ ride_actions_sub
    LIVE_TRACKING -> \a -> a @~ live_tracking
    LIVE_TRACKING_SUB -> \a -> a @~ live_tracking_sub
    CHAT_WITH_RIDER -> \a -> a @~ chat_with_rider
    CHAT_WITH_RIDER_SUB -> \a -> a @~ chat_with_rider_sub
    EMERGENCY_ACTIONS_SUB -> \a -> a @~ emergency_actions_sub
    CURRENT_INITIATIVES -> \a -> a @~ current_initiatives
    CURRENT_INITIATIVES_SUB -> \a -> a @~ current_initiatives_sub
    DRIVER_VERIFICATION -> \a -> a @~ driver_verification
    DRIVER_VERIFICATION_SUB -> \a -> a @~ driver_verification_sub
    SAFETY_FEEDBACK -> \a -> a @~ safety_feedback
    SAFETY_FEEDBACK_SUB -> \a -> a @~ safety_feedback_sub
    SAFETY_TRAINING -> \a -> a @~ safety_training
    SAFETY_TRAINING_SUB -> \a -> a @~ safety_training_sub
    DRIVER_ID_CHECK -> \a -> a @~ driver_id_check
    DRIVER_ID_CHECK_SUB -> \a -> a @~ driver_id_check_sub
    DATA_PRIVACY -> \a -> a @~ data_privacy
    DATA_PRIVACY_SUB -> \a -> a @~ data_privacy_sub
    FAVOURITE_DRIVER -> \a -> a @~ favourite_driver
    FAVOURITE_DRIVER_SUB -> \a -> a @~ favourite_driver_sub
    WOMEN_DRIVERS -> \a -> a @~ women_drivers
    WOMEN_DRIVERS_SUB -> \a -> a @~ women_drivers_sub
    DASHCAM -> \a -> a @~ dashcam
    DASHCAM_SUB -> \a -> a @~ dashcam_sub
    NEVER_SHARE_LN -> \a -> a @~ never_share_ln
    ALWAYS_SHARE_LN -> \a -> a @~ always_share_ln
    SHARE_WITH_TIME_CONSTRAINTS_LN -> \a -> a @~ share_with_time_constraints_ln
    NEVER_SHARE_EM -> \a -> a @~ never_share_em
    ALWAYS_SHARE_EM -> \a -> a @~ always_share_em
    SHARE_WITH_TIME_CONSTRAINTS_EM -> \a -> a @~ share_with_time_constraints_em
    LIVE_RIDE_TRACKING -> \a -> a @~ live_ride_tracking
    LIVE_RIDE_TRACKING_DESC -> \a -> a @~ live_ride_tracking_desc
    UPCOMING_INITIATIVES -> \a -> a @~ upcoming_initiatives
    UPCOMING_INITIATIVES_DESC -> \a -> a @~ upcoming_initiatives_desc
    RECEIVE_CALL_FROM_SAFETY_TEAM -> \a -> a @~ receive_call_from_safety_team
    NOTIFY_ALL_EMERGENCY_CONTACTS -> \a -> a @~ notify_all_emergency_contacts
    RECORD_AUDIO -> \a -> a @~ record_audio
    CALL_SAFETY_TEAM -> \a -> a @~ call_safety_team
    SAFETY_TEAM_CALLBACK_REQUESTED -> \a -> a @~ safety_team_callback_requested
    EMERGENCY_CONTACTS_NOTIFIED -> \a -> a @~ emergency_contacts_notified
    CALL_PLACED -> \a -> a @~ call_placed
    EMERGENCY_SOS_ACTIVATED -> \a -> a @~ emergency_sos_activated
    TAP_TO_CALL_OTHER_EMERGENCY_CONTACTS -> \a -> a @~ tap_to_call_other_emergency_contacts
    RECORDING_AUDIO -> \a -> a @~ recording_audio
    RECORDED_AUDIO -> \a -> a @~ recorded_audio
    SHARE_WITH_SAFETY_TEAM -> \a -> a @~ share_with_safety_team
    EMERGENCY -> \a -> a @~ emergency
    MANUAL_LIVE_TRACKING -> \a -> a @~ manual_live_tracking
    MANUAL_LIVE_TRACKING_DESC -> \a -> a @~ manual_live_tracking_desc
    AUTOMATIC_LIVE_TRACKING -> \a -> a @~ automatic_live_tracking
    AUTOMATIC_LIVE_TRACKING_DESC -> \a -> a @~ automatic_live_tracking_desc
    TRACKING_NO_SETUP -> \a -> a @~ tracking_no_setup
    FOLLOWING_STR -> \a -> a @~ following_str
    DIALING_POLICE_IN_TIME arg1 -> \a -> (a @~ dialing_police_in_time) arg1
    REACHED_DESTINATION arg1 -> \a -> (a @~ reached_destination) arg1
    MINS -> \a -> a @~ mins
    UPDATED_FARE -> \a -> a @~ updated_fare
    HOW'S_TRIP -> \a -> a @~ how's_trip
    PROVIDED_FEEDBACK -> \a -> a @~ provided_feedback
    FAVOURITE_YOUR_DRIVER -> \a -> a @~ favourite_your_driver
    PREFER_DRIVER -> \a -> a @~ prefer_driver
    WRITE_REVIEW -> \a -> a @~ write_review
    TRANSIT -> \a -> a @~ transit
    INTERCITY_STR -> \a -> a @~ intercity_str
    RENTAL_STR -> \a -> a @~ rental_str
    DELIVERY_STR -> \a -> a @~ delivery_str
    WHERE_ARE_YOU_GOING -> \a -> a @~ where_are_you_going
    TAP_TO_FOLLOW -> \a -> a @~ tap_to_follow
    HAS_SHARED_A_RIDE_WITH_YOU arg1 -> \a -> (a @~ has_shared_a_ride_with_you) arg1
    TEST_SOS_ACTIVATED -> \a -> a @~ test_sos_activated
    CHOOSE_FROM_CONTACTS -> \a -> a @~ choose_from_contacts
    ADD_MANUALLY -> \a -> a @~ add_manually
    DEFAULT_CONTACT_NOT_SET -> \a -> a @~ default_contact_not_set
    DRIVER -> \a -> a @~ driver
    RECOMMEND_SHARE_MANUALLY -> \a -> a @~ recommend_share_manually
    CANNOT_ADD_OWN_NUMBER -> \a -> a @~ cannot_add_own_number
    CONFIRM_PICKUP_AND_DROP_LOCATION -> \a -> a @~ confirm_pickup_and_drop_location
    CONFIRM_YOUR_DELIVERY -> \a -> a @~ confirm_your_delivery
    PAYMENT_AT_RECEIVING_END -> \a -> a @~ payment_at_receiving_end
    PAYMENT_AT_RECEIVING_END_DESC -> \a -> a @~ payment_at_receiving_end_desc
    SENDER -> \a -> a @~ sender
    RECEIVER -> \a -> a @~ receiver
    PHONE arg1 -> \a -> (a @~ phone) arg1
    BUILDING_OR_FLAT arg1 -> \a -> (a @~ building_or_flat) arg1
    OPTIONAL_INSTRUCTION -> \a -> a @~ optional_instruction
    HELP_US_PROVIDE_SMOOTH_PICKUP -> \a -> a @~ help_us_provide_smooth_pickup
    HELP_US_PROVIDE_SMOOTH_DROP -> \a -> a @~ help_us_provide_smooth_drop
    I_AM_THE_SENDER -> \a -> a @~ i_am_the_sender
    I_AM_THE_RECEIVER -> \a -> a @~ i_am_the_receiver
    RECKLESS_HANDLING -> \a -> a @~ reckless_handling
    DELIVERY_DELAYED -> \a -> a @~ delivery_delayed
    ITEMS_MISSING -> \a -> a @~ items_missing
    POLITE_ATTITUDE -> \a -> a @~ polite_attitude
    SMOOTH_EXPERIENCE -> \a -> a @~ smooth_experience
    SECURE_DELIVERY -> \a -> a @~ secure_delivery
    MINIMAL_CALLING -> \a -> a @~ minimal_calling
    RUDE_BEHAVIOUR -> \a -> a @~ rude_behaviour
    PACKAGE_PHOTO_AND_OTP -> \a -> a @~ package_photo_and_otp
    SEND_NOW -> \a -> a @~ send_now
    BOOK_FOR -> \a -> a @~ book_for
    DELIVERY_DETAILS -> \a -> a @~ delivery_details
    DELIVERY_GUIDELINES -> \a -> a @~ delivery_guidelines
    VIEW_ALL_GUIDELINES -> \a -> a @~ view_all_guidelines
    ITEMS_SHOULD_FIT_IN_BACKPACK arg1 -> \a -> (a @~ items_should_fit_in_backpack) arg1
    AVOID_SENDING_HIGH_VALUE_ITEMS -> \a -> a @~ avoid_sending_high_value_items
    ILLEGAL_ITEMS_PROHIBITED -> \a -> a @~ illegal_items_prohibited
    PICKUP_INSTRUCTION -> \a -> a @~ pickup_instruction
    MORE_ABOUT_ME -> \a -> a @~ more_about_me
    DRIVING_SINCE -> \a -> a @~ driving_since
    DRIVER_SECTION_CARD -> \a -> a @~ driver_section_card
    KNOW_YOUR_DRIVER -> \a -> a @~ know_your_driver
    RATED -> \a -> a @~ rated
    DRIVERS -> \a -> a @~ drivers
    LOCATIONS -> \a -> a @~ locations
    KNOWS_YOUR_DRIVER -> \a -> a @~ knows_your_driver
    LAST_TRIP -> \a -> a @~ last_trip
    RUPEES -> \a -> a @~ rupees
    PAID_BY_CASH -> \a -> a @~ paid_by_cash
    REMOVE_FROM_FAVOURITE -> \a -> a @~ remove_from_favourite
    CLEAN_VEHICLE -> \a -> a @~ clean_vehicle
    AC_NOT_TURNED_ON -> \a -> a @~ ac_not_turned_on
    LATE_PICK_UP_ARRIVAL -> \a -> a @~ late_pick_up_arrival
    ASKED_FOR_MORE_FARE -> \a -> a @~ asked_for_more_fare
    UNHYGIENIC_VEHICLE -> \a -> a @~ unhygienic_vehicle
    TRAINING -> \a -> a @~ training
    FINANCIAL -> \a -> a @~ financial
    KIDS_EDUCATION -> \a -> a @~ kids_education
    BUY_NEW_VEHICLE -> \a -> a @~ buy_new_vehicle
    FAILED_TO_REMOVE_DRIVER -> \a -> a @~ failed_to_remove_driver
    NOT_AVAILABLE -> \a -> a @~ not_available
    BUY_NEW_HOME -> \a -> a @~ buy_new_home
    YOU_FAVOURITED -> \a -> a @~ you_favourited
    FAVORITE_YOUR_DRIVER -> \a -> a @~ favorite_your_driver
    FAVOURITE_DRIVER_PREFERENCE -> \a -> a @~ favourite_driver_preference
    RIDE_TYPE_WITH_FAVOURITE_DRIVER -> \a -> a @~ ride_type_with_favourite_driver
    GOTIT -> \a -> a @~ gotit
    NO_FAVOURITE_YET -> \a -> a @~ no_favourite_yet
    FAVOURITE_APPEAR_HERE -> \a -> a @~ favourite_appear_here
    EDIT_YOUR_PICKUP_LOCATION_INSTEAD -> \a -> a @~ edit_your_pickup_location_instead
    ROUND_TRIP_INVALID_MESSAGE -> \a -> a @~ round_trip_invalid_message
    PICKUP_TIME_NOT_SELECTED -> \a -> a @~ pickup_time_not_selected
    BOOKING_DURATION_INVALID -> \a -> a @~ booking_duration_invalid
    RETURN -> \a -> a @~ return
    PICKUP_INPUT -> \a -> a @~ pickup_input
    RETURN_INPUT -> \a -> a @~ return_input
    BOOK_A_ROUND_TRIP -> \a -> a @~ book_a_round_trip
    TOTAL_RIDE_DURATION -> \a -> a @~ total_ride_duration
    TOTAL_RIDE_DISTANCE -> \a -> a @~ total_ride_distance
    RIDE -> \a -> a @~ ride
    ACCEPT -> \a -> a @~ accept
    PASS -> \a -> a @~ pass
    TERM_1A -> \a -> a @~ term_1a
    TERM_2A -> \a -> a @~ term_2a
    TERM_3A -> \a -> a @~ term_3a
    TERM_1B -> \a -> a @~ term_1b
    TERM_2B -> \a -> a @~ term_2b
    TERM_3B -> \a -> a @~ term_3b
    EXCLUDED_CHARGES -> \a -> a @~ excluded_charges
    TOLLS -> \a -> a @~ tolls
    STATE_PERMIT -> \a -> a @~ state_permit
    EXCLUDED_FOOTER -> \a -> a @~ excluded_footer
    INCLUDED_CHARGES -> \a -> a @~ included_charges
    INC_1 -> \a -> a @~ inc_1
    INC_2A -> \a -> a @~ inc_2a
    INC_2B -> \a -> a @~ inc_2b
    PICKUP_DROP -> \a -> a @~ pickup_drop
    INTERCITY_TC_1 -> \a -> a @~ intercity_tc_1
    INTERCITY_TC_2 -> \a -> a @~ intercity_tc_2
    ROUND_TRIP_POLICY_CARD_1 -> \a -> a @~ round_trip_policy_card_1
    ROUND_TRIP_POLICY_CARD_2 -> \a -> a @~ round_trip_policy_card_2
    ROUND_TRIP_POLICY -> \a -> a @~ round_trip_policy
    DISTANCE_FARE -> \a -> a @~ distance_fare
    DRIVER_ALLOWANCE_ -> \a -> a @~ driver_allowance_
    NIGHT_CHARGES -> \a -> a @~ night_charges
    NIGHT_CHARGES_DESCRIPTION -> \a -> a @~ night_charges_description
    STATE_CHARGES_DESCRIPTION -> \a -> a @~ state_charges_description
    PARKING_CHARGES_DESCRIPTION -> \a -> a @~ parking_charges_description
    TOLL_AND_PARKING_CHARGES_DESCRIPTION -> \a -> a @~ toll_and_parking_charges_description
    STATE_PERMIT_CHARGES -> \a -> a @~ state_permit_charges
    DRIVER_ALLOWANCE_DESCRIPTION arg1 arg2 -> \a -> (a @~ driver_allowance_description) arg1 arg2
    DRIVER_ALLOWANCE_REQUIRED -> \a -> a @~ driver_allowance_required
    TOLL_AND_PARKING_CHARGES -> \a -> a @~ toll_and_parking_charges
    NIGHT_SHIFT_CHARGES -> \a -> a @~ night_shift_charges
    ROUND_TRIP_EXPLAINER -> \a -> a @~ round_trip_explainer
    RESERVE -> \a -> a @~ reserve
    LEAVE_NOW -> \a -> a @~ leave_now
    IS_YOUR_DRIVER -> \a -> a @~ is_your_driver
    YOUR_DRIVER_WILL_BE -> \a -> a @~ your_driver_will_be
    RIDE_SUMMARY -> \a -> a @~ ride_summary
    TRIP_INCLUSIONS -> \a -> a @~ trip_inclusions
    PLEASE_PAY_PARKING_OR_OTHER -> \a -> a @~ please_pay_parking_or_other
    TOLL_AND_STATE_PERMIT -> \a -> a @~ toll_and_state_permit
    TRIP_EXCLUSION -> \a -> a @~ trip_exclusion
    CONFIRM -> \a -> a @~ confirm
    YOU_HAVE_AN_RIDE_FROM_WITHOUT_TO -> \a -> a @~ you_have_an_ride_from_without_to
    FOR_EVERY_EXTRA_HOUR_YOU_ADD -> \a -> a @~ for_every_extra_hour_you_add
    BY_DEFAULT_ONE_HOUR -> \a -> a @~ by_default_one_hour
    YOU_HAVE_AN_UPCOMING_BOOKING arg1 -> \a -> (a @~ you_have_an_upcoming_booking) arg1
    ADDON_KM_CHARGE -> \a -> a @~ addon_km_charge
    TIME_FARE -> \a -> a @~ time_fare
    TRIP_FARE_INCLUDES -> \a -> a @~ trip_fare_includes
    TRIP_FARE_EXCLUDES -> \a -> a @~ trip_fare_excludes
    EXTRAS_WILL_BE_CHARGED_AT -> \a -> a @~ extras_will_be_charged_at
    FIXED_CHARGES -> \a -> a @~ fixed_charges
    PICKUP_DATE_AND_TIME -> \a -> a @~ pickup_date_and_time
    DROP_BACK_IN_AT arg1 -> \a -> (a @~ drop_back_in_at) arg1
    PER_KM -> \a -> a @~ per_km
    PER_MIN -> \a -> a @~ per_min
    HOUR -> \a -> a @~ hour
    SEC -> \a -> a @~ sec
    BIKE_TAXI -> \a -> a @~ bike_taxi
    PHONE_NUMBER_PERMISSION -> \a -> a @~ phone_number_permission
    PHONE_NUMBER_PERMISSION_DESC -> \a -> a @~ phone_number_permission_desc
    DENY -> \a -> a @~ deny
    ALLOW -> \a -> a @~ allow
    INTERCITY_BUS -> \a -> a @~ intercity_bus
    DRIVER_ASSIGNED -> \a -> a @~ driver_assigned
    SENDER_NAME -> \a -> a @~ sender_name
    SENDER_PHONE -> \a -> a @~ sender_phone
    SENDERS_BUILDING_FLAT -> \a -> a @~ senders_building_flat
    PICKUP_INSTRUCTIONS -> \a -> a @~ pickup_instructions
    RECEIVER_NAME -> \a -> a @~ receiver_name
    RECEIVER_PHONE -> \a -> a @~ receiver_phone
    RECEIVERS_BUILDING_FLAT -> \a -> a @~ receivers_building_flat
    DROP_INSTRUCTIONS -> \a -> a @~ drop_instructions
    OUT_FOR_DELIVERY -> \a -> a @~ out_for_delivery
    PICKUP_IN_PROGRESS -> \a -> a @~ pickup_in_progress
    ARRIVED_AT_DROP -> \a -> a @~ arrived_at_drop
    OUT_FOR_PICKUP -> \a -> a @~ out_for_pickup
    CONFIRM_PICKUP -> \a -> a @~ confirm_pickup
    RATE_YOUR_DELIVERY_WITH -> \a -> a @~ rate_your_delivery_with
    DELIVERY_COMPLETED -> \a -> a @~ delivery_completed
    ESTIMATED_ARRIVAL_BY -> \a -> a @~ estimated_arrival_by
    BACK -> \a -> a @~ back
    PHOTO_NOT_TAKEN_YET -> \a -> a @~ photo_not_taken_yet
    PLEASE_REFRESH_ONCE_DRIVER_TAKES_PHOTO -> \a -> a @~ please_refresh_once_driver_takes_photo
    PACKAGE_PHOTO_DESC -> \a -> a @~ package_photo_desc
    START_OTP -> \a -> a @~ start_otp
    PACKAGE_PHOTO -> \a -> a @~ package_photo
    REFRESH -> \a -> a @~ refresh
    DROP_INSTRUCTION -> \a -> a @~ drop_instruction
    QUICK_DELIVERY_WITH arg1 -> \a -> (a @~ quick_delivery_with) arg1
    STEP -> \a -> a @~ step
    BOOKING_CANNOT_PROCEED_ONE_PARTY_HAS_ACTIVE_BOOKING -> \a -> a @~ booking_cannot_proceed_one_party_has_active_booking
    PLEASE_ENTER_A_VALID_MOBILE_NUMBER -> \a -> a @~ please_enter_a_valid_mobile_number
    PLEASE_ENTER_A_VALID_ADDRESS -> \a -> a @~ please_enter_a_valid_address
    PLEASE_ENTER_A_VALID_NAME -> \a -> a @~ please_enter_a_valid_name
    ENTER_A_NAME -> \a -> a @~ enter_a_name
    ENTER_A_ADDRESS -> \a -> a @~ enter_a_address
    EXPLORE -> \a -> a @~ explore
    DELIVERED_IN_JUST arg1 -> \a -> (a @~ delivered_in_just) arg1
    DIFFERENT_BIKE -> \a -> a @~ different_bike
    ODISHA -> \a -> a @~ odisha
    BHUBANESWAR -> \a -> a @~ bhubaneswar
    LIMIT_REACHED -> \a -> a @~ limit_reached
    CONFIRM_CONTACTS -> \a -> a @~ confirm_contacts
    TICKETS_FOR_DELHI_METRO -> \a -> a @~ tickets_for_delhi_metro
    MAX_PARCEL_SIZE arg1 arg2 arg3 -> \a -> (a @~ max_parcel_size) arg1 arg2 arg3
    TICKETS_FOR_CHENNAI_BUS -> \a -> a @~ tickets_for_chennai_bus
    BUS__ -> \a -> a @~ bus__
    BUY_BUS_TICKETS -> \a -> a @~ buy_bus_tickets
    BOOK_AND_PAY -> \a -> a @~ book_and_pay
    BUS_TICKET -> \a -> a @~ bus_ticket
    CHECK_SPELLING_AND_TRY_AGAIN -> \a -> a @~ check_spelling_and_try_again
    BOOK_BUS_TICKET -> \a -> a @~ book_bus_ticket
    BOOK_A_ONE_WAY_INSTANT_BUS_TICKET -> \a -> a @~ book_a_one_way_instant_bus_ticket
    RECENT_TICKETS -> \a -> a @~ recent_ticket
    EXPERIENCE_HASSLE_FREE_BUS_BOOKINGS_WITH -> \a -> a @~ experience_hassle_free_bus_bookings_with
    ENTER_BUS_NUMBER_OR_DESTINATION -> \a -> a @~ enter_bus_number_or_destination
    DESTINATION_STOPS -> \a -> a @~ destination_stop
    ROUTE_BUS_NO -> \a -> a @~ route_bus_number
    PICKUP_STOP -> \a -> a @~ pickup_stop
    TICKETS_FOR_KOLKATA_BUS -> \a -> a @~ tickets_for_kolkata_bus
    TICKET_VALIDITY_30_MINUTES -> \a -> a @~ ticket_validity_30_minutes
    FARE_COMMISSION_FREE_WBTC -> \a -> a @~ fare_commission_free_wbtc
    SELECT_ROUTE_NUMBER -> \a -> a @~ select_route_number
    PICKUP_AND_DESTINATION_STOP -> \a -> a @~ pickup_and_destination_stop
    BUS__ -> \a -> a @~ bus__
    BUS_BOARDED_CONFIRMATION -> \a -> a @~ bus_boarded_confirmation
    TOWARDS_STATION arg1 -> \a -> (a @~ towards_station) arg1
    BUS_NO arg1 -> \a -> (a @~ bus_no) arg1
    VERIFIED -> \a -> a @~ verified
    EXPERIENCE_OUR_PILOT_LAUNCH_FOR_BUS_TICKETING_IN_PRIME_ROUTES arg1 -> \a -> (a @~ experience_our_pilot_launch_for_bus_ticketing_in_prime_routes) arg1
    NOTE_YOUR_TICKET_IS_ONLY_VALID_FOR arg1 -> \a -> (a @~ note_your_ticket_is_only_valid_for) arg1
    HERE_IS_BUS_TICKET -> \a -> a @~ here_is_bus_ticket
    AMBULANCE_ -> \a -> a @~ ambulance_
    FREE_LOADING_UNLOADING_TIME arg1 arg2 -> \a -> (a @~ free_loading_unloading_time) arg1 arg2
    SELECT_ROUTE -> \a -> a @~ select_route


