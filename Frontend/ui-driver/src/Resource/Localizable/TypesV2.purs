module Resource.Localizable.TypesV2 where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))

newtype Languages = Languages
  { hindi :: Keymap
  , english :: Keymap
  }

newtype Keymap = Keymap
  { str1 :: String
  , str2 :: String
  , str3 :: String
  , str4 :: (String -> String)
  }

derive instance ntL :: Newtype Languages _
derive instance ntK :: Newtype Keymap _
a :: forall a. Proxy a
a = Proxy

hindi :: Proxy "hindi"
hindi = a

english :: Proxy "english"
english = a

malayalam :: Proxy "malayalam"
malayalam = a

str1 :: Proxy "str1"
str1 = a

str2 :: Proxy "str2"
str2 = a

str3 :: Proxy "str3"
str3 = a

str4 :: Proxy "str4"
str4 = a


language_updated :: Proxy "language_updated"
language_updated = a
your_application_has_been_submitted_successfully_and_is_under_verification :: Proxy "your_application_has_been_submitted_successfully_and_is_under_verification"
your_application_has_been_submitted_successfully_and_is_under_verification = a
view_status :: Proxy "view_status"
view_status = a
go_home :: Proxy "go_home"
go_home = a
select_language :: Proxy "select_language"
select_language = a
which_language_do_you_prefer :: Proxy "which_language_do_you_prefer"
which_language_do_you_prefer = a
t_c :: Proxy "t_c"
t_c = a
enter_mobile_number :: Proxy "enter_mobile_number"
enter_mobile_number = a
by_clicking_continue_you_will_be_agreeing_to_our :: Proxy "by_clicking_continue_you_will_be_agreeing_to_our"
by_clicking_continue_you_will_be_agreeing_to_our = a
enter_otp :: Proxy "enter_otp"
enter_otp = a
didnt_recieve_otp :: Proxy "didnt_recieve_otp"
didnt_recieve_otp = a
resend_otp :: Proxy "resend_otp"
resend_otp = a
please_enter_valid_otp :: Proxy "please_enter_valid_otp"
please_enter_valid_otp = a
invalid_mobile_number :: Proxy "invalid_mobile_number"
invalid_mobile_number = a
register :: Proxy "register"
register = a
mobile_number :: Proxy "mobile_number"
mobile_number = a
auto_reading_otp :: Proxy "auto_reading_otp"
auto_reading_otp = a
upload_driving_license :: Proxy "upload_driving_license"
upload_driving_license = a
upload_back_side :: Proxy "upload_back_side"
upload_back_side = a
upload_front_side :: Proxy "upload_front_side"
upload_front_side = a
back_side :: Proxy "back_side"
back_side = a
front_side :: Proxy "front_side"
front_side = a
next :: Proxy "next"
next = a
license_instruction_picture :: Proxy "license_instruction_picture"
license_instruction_picture = a
license_instruction_clarity :: Proxy "license_instruction_clarity"
license_instruction_clarity = a
registration_steps :: Proxy "registration_steps"
registration_steps = a
progress_saved :: Proxy "progress_saved"
progress_saved = a
driving_license :: Proxy "driving_license"
driving_license = a
aadhar_card :: Proxy "aadhar_card"
aadhar_card = a
bank_details :: Proxy "bank_details"
bank_details = a
vehicle_details :: Proxy "vehicle_details"
vehicle_details = a
upload_front_back :: Proxy "upload_front_back"
upload_front_back = a
earnings_will_be_credited :: Proxy "earnings_will_be_credited"
earnings_will_be_credited = a
fill_vehicle_details :: Proxy "fill_vehicle_details"
fill_vehicle_details = a
follow_steps :: Proxy "follow_steps"
follow_steps = a
registration :: Proxy "registration"
registration = a
upload_adhaar_card :: Proxy "upload_adhaar_card"
upload_adhaar_card = a
adhaar_intruction_picture :: Proxy "adhaar_intruction_picture"
adhaar_intruction_picture = a
add_vehicle_details :: Proxy "add_vehicle_details"
add_vehicle_details = a
vehicle_registration_number :: Proxy "vehicle_registration_number"
vehicle_registration_number = a
enter_vehicle_no :: Proxy "enter_vehicle_no"
enter_vehicle_no = a
vehicle_type :: Proxy "vehicle_type"
vehicle_type = a
vehicle_model_name :: Proxy "vehicle_model_name"
vehicle_model_name = a
enter_model_name :: Proxy "enter_model_name"
enter_model_name = a
vehicle_colour :: Proxy "vehicle_colour"
vehicle_colour = a
enter_vehicle_colour :: Proxy "enter_vehicle_colour"
enter_vehicle_colour = a
upload_registration_certificate :: Proxy "upload_registration_certificate"
upload_registration_certificate = a
upload_rc :: Proxy "upload_rc"
upload_rc = a
preview :: Proxy "preview"
preview = a
choose_vehicle_type :: Proxy "choose_vehicle_type"
choose_vehicle_type = a
max_images :: Proxy "max_images"
max_images = a
re_enter_benificiary_number :: Proxy "re_enter_benificiary_number"
re_enter_benificiary_number = a
ifsc_code :: Proxy "ifsc_code"
ifsc_code = a
benificiary_number :: Proxy "benificiary_number"
benificiary_number = a
sending_otp :: Proxy "sending_otp"
sending_otp = a
loading :: Proxy "loading"
loading = a
please_wait_while_in_progress :: Proxy "please_wait_while_in_progress"
please_wait_while_in_progress = a
your_request_has_timeout_try_again :: Proxy "your_request_has_timeout_try_again"
your_request_has_timeout_try_again = a
error_occured_please_try_again_later :: Proxy "error_occured_please_try_again_later"
error_occured_please_try_again_later = a
country_code_india :: Proxy "country_code_india"
country_code_india = a
enter_otp_sent_to :: Proxy "enter_otp_sent_to"
enter_otp_sent_to = a
otp_sent_to :: Proxy "otp_sent_to"
otp_sent_to = a
enter_account_number :: Proxy "enter_account_number"
enter_account_number = a
add_bank_details :: Proxy "add_bank_details"
add_bank_details = a
enter_ifsc_code :: Proxy "enter_ifsc_code"
enter_ifsc_code = a
submit :: Proxy "submit"
submit = a
personal_details :: Proxy "personal_details"
personal_details = a
languages :: Proxy "languages"
languages = a
help_and_faq :: Proxy "help_and_faq"
help_and_faq = a
about :: Proxy "about"
about = a
logout :: Proxy "logout"
logout = a
update :: Proxy "update"
update = a
edit :: Proxy "edit"
edit = a
delete :: Proxy "delete"
delete = a
view :: Proxy "view"
view = a
issue_no :: Proxy "issue_no"
issue_no = a
add_voice_note :: Proxy "add_voice_note"
add_voice_note = a
voice_note_added :: Proxy "voice_note_added"
voice_note_added = a
added_images :: Proxy "added_images"
added_images = a
no_images_added :: Proxy "no_images_added"
no_images_added = a
ask_details_message :: Proxy "ask_details_message"
ask_details_message = a
ask_details_message_reversed :: Proxy "ask_details_message_reversed"
ask_details_message_reversed = a
select_option :: Proxy "select_option"
select_option = a
select_option_reversed :: Proxy "select_option_reversed"
select_option_reversed = a
issue_submitted_message :: Proxy "issue_submitted_message"
issue_submitted_message = a
submit_issue_details :: Proxy "submit_issue_details"
submit_issue_details = a
image_preview :: Proxy "image_preview"
image_preview = a
ride_report_issue :: Proxy "ride_report_issue"
ride_report_issue = a
i_dont_know_which_ride :: Proxy "i_dont_know_which_ride"
i_dont_know_which_ride = a
report_issue_chat_placeholder :: Proxy "report_issue_chat_placeholder"
report_issue_chat_placeholder = a
added_voice_note :: Proxy "added_voice_note"
added_voice_note = a
no_voice_note_added :: Proxy "no_voice_note_added"
no_voice_note_added = a
call_customer_title :: Proxy "call_customer_title"
call_customer_title = a
call_customer_description :: Proxy "call_customer_description"
call_customer_description = a
place_call :: Proxy "place_call"
place_call = a
place_call_request :: Proxy "place_call_request"
place_call_request = a
add_image :: Proxy "add_image"
add_image = a
add_another :: Proxy "add_another"
add_another = a
images_added :: Proxy "images_added"
images_added = a
issue_submitted_text :: Proxy "issue_submitted_text"
issue_submitted_text = a
choose_an_option :: Proxy "choose_an_option"
choose_an_option = a
image_added :: Proxy "image_added"
image_added = a
done :: Proxy "done"
done = a
record_voice_note :: Proxy "record_voice_note"
record_voice_note = a
auto :: Proxy "auto"
auto = a
name :: Proxy "name"
name = a
privacy_policy :: Proxy "privacy_policy"
privacy_policy = a
logo :: Proxy "logo"
logo = a
about_app_description :: Proxy "about_app_description"
about_app_description = a
terms_and_conditions :: Proxy "terms_and_conditions"
terms_and_conditions = a
update_vehicle_details :: Proxy "update_vehicle_details"
update_vehicle_details = a
help_and_support :: Proxy "help_and_support"
help_and_support = a
note :: Proxy "note"
note = a
visit_my_rides_screen_for_specific_complaints :: Proxy "visit_my_rides_screen_for_specific_complaints"
visit_my_rides_screen_for_specific_complaints = a
thank_you_for_wrtitting_us :: Proxy "thank_you_for_wrtitting_us"
thank_you_for_wrtitting_us = a
go_to_home :: Proxy "go_to_home"
go_to_home = a
your_recent_ride :: Proxy "your_recent_ride"
your_recent_ride = a
your_recent_trip :: Proxy "your_recent_trip"
your_recent_trip = a
all_topics :: Proxy "all_topics"
all_topics = a
report_an_issue_with_this_trip :: Proxy "report_an_issue_with_this_trip"
report_an_issue_with_this_trip = a
you_rated :: Proxy "you_rated"
you_rated = a
view_all_rides :: Proxy "view_all_rides"
view_all_rides = a
write_to_us :: Proxy "write_to_us"
write_to_us = a
subject :: Proxy "subject"
subject = a
your_email_id :: Proxy "your_email_id"
your_email_id = a
more_options :: Proxy "more_options"
more_options = a
describe_your_issue :: Proxy "describe_your_issue"
describe_your_issue = a
getting_started_and_faq :: Proxy "getting_started_and_faq"
getting_started_and_faq = a
ongoing_issues :: Proxy "ongoing_issues"
ongoing_issues = a
resolved_issues :: Proxy "resolved_issues"
resolved_issues = a
for_other_issues_write_to_us :: Proxy "for_other_issues_write_to_us"
for_other_issues_write_to_us = a
call_support_center :: Proxy "call_support_center"
call_support_center = a
you_can_describe_issue_that_you_faced_here :: Proxy "you_can_describe_issue_that_you_faced_here"
you_can_describe_issue_that_you_faced_here = a
registration_certificate_image :: Proxy "registration_certificate_image"
registration_certificate_image = a
home :: Proxy "home"
home = a
rides :: Proxy "rides"
rides = a
my_rides :: Proxy "my_rides"
my_rides = a
profile :: Proxy "profile"
profile = a
enter_driving_license_number :: Proxy "enter_driving_license_number"
enter_driving_license_number = a
trip_details :: Proxy "trip_details"
trip_details = a
by_cash :: Proxy "by_cash"
by_cash = a
online_ :: Proxy "online_"
online_ = a
go_online_popup :: Proxy "go_online_popup"
go_online_popup = a
distance :: Proxy "distance"
distance = a
coin_balance :: Proxy "coin_balance"
coin_balance = a
coins_balance :: Proxy "coins_balance"
coins_balance = a
total_earned :: Proxy "total_earned"
total_earned = a
ride_history :: Proxy "ride_history"
ride_history = a
transaction_history :: Proxy "transaction_history"
transaction_history = a
coins_earned :: Proxy "coins_earned"
coins_earned = a
no_rides :: Proxy "no_rides"
no_rides = a
coins_used :: Proxy "coins_used"
coins_used = a
use_coins :: Proxy "use_coins"
use_coins = a
insights :: Proxy "insights"
insights = a
usage_history :: Proxy "usage_history"
usage_history = a
no_coins_earned :: Proxy "no_coins_earned"
no_coins_earned = a
no_coins_used :: Proxy "no_coins_used"
no_coins_used = a
earn_coins_by_taking_rides_and_referring_the_app_to_others :: Proxy "earn_coins_by_taking_rides_and_referring_the_app_to_others"
earn_coins_by_taking_rides_and_referring_the_app_to_others = a
use_them_before_they_expire :: Proxy "use_them_before_they_expire"
use_them_before_they_expire = a
no_ride_history_available :: Proxy "no_ride_history_available"
no_ride_history_available = a
you_have_not_completed_a_ride_yet :: Proxy "you_have_not_completed_a_ride_yet"
you_have_not_completed_a_ride_yet = a
complete_first_ride_to_unlock_coins :: Proxy "complete_first_ride_to_unlock_coins"
complete_first_ride_to_unlock_coins = a
destination :: Proxy "destination"
destination = a
you_did_not_take_any_rides_on_prefix :: Proxy "you_did_not_take_any_rides_on_prefix"
you_did_not_take_any_rides_on_prefix = a
you_did_not_take_any_rides_on_suffix :: Proxy "you_did_not_take_any_rides_on_suffix"
you_did_not_take_any_rides_on_suffix = a
convert :: Proxy "convert"
convert = a
cash_converted :: Proxy "cash_converted"
cash_converted = a
will_be_adjusted_in_your_future_subscription_dues :: Proxy "will_be_adjusted_in_your_future_subscription_dues"
will_be_adjusted_in_your_future_subscription_dues = a
has_been_adjusted_in_your_subscription_dues :: Proxy "has_been_adjusted_in_your_subscription_dues"
has_been_adjusted_in_your_subscription_dues = a
using_coins_requires_an_active_plan :: Proxy "using_coins_requires_an_active_plan"
using_coins_requires_an_active_plan = a
to_get_started :: Proxy "to_get_started"
to_get_started = a
converted_from_coins :: Proxy "converted_from_coins"
converted_from_coins = a
report_an_issue :: Proxy "report_an_issue"
report_an_issue = a
time_taken :: Proxy "time_taken"
time_taken = a
maps :: Proxy "maps"
maps = a
call :: Proxy "call"
call = a
start_ride :: Proxy "start_ride"
start_ride = a
cancel_ride :: Proxy "cancel_ride"
cancel_ride = a
please_tell_us_why_you_want_to_cancel :: Proxy "please_tell_us_why_you_want_to_cancel"
please_tell_us_why_you_want_to_cancel = a
mandatory :: Proxy "mandatory"
mandatory = a
end_ride :: Proxy "end_ride"
end_ride = a
ride_completed_with :: Proxy "ride_completed_with"
ride_completed_with = a
collect_amount_in_cash :: Proxy "collect_amount_in_cash"
collect_amount_in_cash = a
cash_collected :: Proxy "cash_collected"
cash_collected = a
offline :: Proxy "offline"
offline = a
accept_for :: Proxy "accept_for"
accept_for = a
decline :: Proxy "decline"
decline = a
request :: Proxy "request"
request = a
you_are_offline :: Proxy "you_are_offline"
you_are_offline = a
you_are_currently_busy_go_online_to_recieve_trip_requests :: Proxy "you_are_currently_busy_go_online_to_recieve_trip_requests"
you_are_currently_busy_go_online_to_recieve_trip_requests = a
going_offline_will_not_get_you_any_ride :: Proxy "going_offline_will_not_get_you_any_ride"
going_offline_will_not_get_you_any_ride = a
cancel :: Proxy "cancel"
cancel = a
go_offline :: Proxy "go_offline"
go_offline = a
is_waiting_for_you :: Proxy "is_waiting_for_you"
is_waiting_for_you = a
you_are_on_a_ride :: Proxy "you_are_on_a_ride"
you_are_on_a_ride = a
please_ask_the_customer_for_the_otp :: Proxy "please_ask_the_customer_for_the_otp"
please_ask_the_customer_for_the_otp = a
enter_current_odometer_reading :: Proxy "enter_current_odometer_reading"
enter_current_odometer_reading = a
enter_final_odo_reading :: Proxy "enter_final_odo_reading"
enter_final_odo_reading = a
enter_the_last_4_digits_of_odometer :: Proxy "enter_the_last_4_digits_of_odometer"
enter_the_last_4_digits_of_odometer = a
enter_the_digits_of_odometer :: Proxy "enter_the_digits_of_odometer"
enter_the_digits_of_odometer = a
odometer_reading_validation_failed :: Proxy "odometer_reading_validation_failed"
odometer_reading_validation_failed = a
completed_ :: Proxy "completed_"
completed_ = a
cancelled_ :: Proxy "cancelled_"
cancelled_ = a
where_is_my_license_number :: Proxy "where_is_my_license_number"
where_is_my_license_number = a
we_need_some_access :: Proxy "we_need_some_access"
we_need_some_access = a
allow_access :: Proxy "allow_access"
allow_access = a
enter_rc_number :: Proxy "enter_rc_number"
enter_rc_number = a
where_is_my_rc_number :: Proxy "where_is_my_rc_number"
where_is_my_rc_number = a
we_have_recieved_your_issue :: Proxy "we_have_recieved_your_issue"
we_have_recieved_your_issue = a
thank_you_for_writing_to_us :: Proxy "thank_you_for_writing_to_us"
thank_you_for_writing_to_us = a
rider :: Proxy "rider"
rider = a
trip_id :: Proxy "trip_id"
trip_id = a
need_it_to_show_you_incoming_ride_request :: Proxy "need_it_to_show_you_incoming_ride_request"
need_it_to_show_you_incoming_ride_request = a
need_it_to_disable_battery_optimization_for_the_app :: Proxy "need_it_to_disable_battery_optimization_for_the_app"
need_it_to_disable_battery_optimization_for_the_app = a
need_it_to_autostart_your_app :: Proxy "need_it_to_autostart_your_app"
need_it_to_autostart_your_app = a
need_it_to_enable_location :: Proxy "need_it_to_enable_location"
need_it_to_enable_location = a
overlay_to_draw_over_applications :: Proxy "overlay_to_draw_over_applications"
overlay_to_draw_over_applications = a
battery_optimizations :: Proxy "battery_optimizations"
battery_optimizations = a
auto_start_application_in_background :: Proxy "auto_start_application_in_background"
auto_start_application_in_background = a
location_access :: Proxy "location_access"
location_access = a
step :: Proxy "step"
step = a
paid :: Proxy "paid"
paid = a
entered_wrong_otp :: Proxy "entered_wrong_otp"
entered_wrong_otp = a
otp_invalid_for_this_vehicle_variant :: Proxy "otp_invalid_for_this_vehicle_variant"
otp_invalid_for_this_vehicle_variant = a
copied :: Proxy "copied"
copied = a
bank_name :: Proxy "bank_name"
bank_name = a
aadhar_details :: Proxy "aadhar_details"
aadhar_details = a
aadhar_number :: Proxy "aadhar_number"
aadhar_number = a
front_side_image :: Proxy "front_side_image"
front_side_image = a
back_side_image :: Proxy "back_side_image"
back_side_image = a
still_not_resolved :: Proxy "still_not_resolved"
still_not_resolved = a
case_two :: Proxy "case_two"
case_two = a
non_disclouser_agreement :: Proxy "non_disclouser_agreement"
non_disclouser_agreement = a
data_collection_authority :: Proxy "data_collection_authority"
data_collection_authority = a
software_license :: Proxy "software_license"
software_license = a
load_more :: Proxy "load_more"
load_more = a
are_you_sure_you_want_to_logout :: Proxy "are_you_sure_you_want_to_logout"
are_you_sure_you_want_to_logout = a
go_back :: Proxy "go_back"
go_back = a
thank_you_for_registering_us :: Proxy "thank_you_for_registering_us"
thank_you_for_registering_us = a
unfortanutely_we_are_not_available__yet_for_you :: Proxy "unfortanutely_we_are_not_available__yet_for_you"
unfortanutely_we_are_not_available__yet_for_you = a
are_you_sure_you_want_to_end_the_ride :: Proxy "are_you_sure_you_want_to_end_the_ride"
are_you_sure_you_want_to_end_the_ride = a
empty_rides :: Proxy "empty_rides"
empty_rides = a
you_have_not_taken_a_trip_yet :: Proxy "you_have_not_taken_a_trip_yet"
you_have_not_taken_a_trip_yet = a
book_now :: Proxy "book_now"
book_now = a
resend_otp_in :: Proxy "resend_otp_in"
resend_otp_in = a
we_need_access_to_your_location :: Proxy "we_need_access_to_your_location"
we_need_access_to_your_location = a
your_location_helps_our_system :: Proxy "your_location_helps_our_system"
your_location_helps_our_system = a
no_internet_connection :: Proxy "no_internet_connection"
no_internet_connection = a
please_check_your_internet_connection_and_try_again :: Proxy "please_check_your_internet_connection_and_try_again"
please_check_your_internet_connection_and_try_again = a
try_again :: Proxy "try_again"
try_again = a
grant_access :: Proxy "grant_access"
grant_access = a
enter_referral_mobile_number :: Proxy "enter_referral_mobile_number"
enter_referral_mobile_number = a
apply :: Proxy "apply"
apply = a
have_a_referral :: Proxy "have_a_referral"
have_a_referral = a
add_here :: Proxy "add_here"
add_here = a
referral_applied :: Proxy "referral_applied"
referral_applied = a
smalledit :: Proxy "smalledit"
smalledit = a
add_driving_license :: Proxy "add_driving_license"
add_driving_license = a
help :: Proxy "help"
help = a
invalid_dl_number :: Proxy "invalid_dl_number"
invalid_dl_number = a
driving_license_number :: Proxy "driving_license_number"
driving_license_number = a
enter_dl_number :: Proxy "enter_dl_number"
enter_dl_number = a
select_date_of_birth :: Proxy "select_date_of_birth"
select_date_of_birth = a
date_of_birth :: Proxy "date_of_birth"
date_of_birth = a
watch_a_tutorial_for_easy_registration :: Proxy "watch_a_tutorial_for_easy_registration"
watch_a_tutorial_for_easy_registration = a
enter_minimum_fifteen_characters :: Proxy "enter_minimum_fifteen_characters"
enter_minimum_fifteen_characters = a
add_your_friend :: Proxy "add_your_friend"
add_your_friend = a
please_wait_while_validating_the_image :: Proxy "please_wait_while_validating_the_image"
please_wait_while_validating_the_image = a
validating :: Proxy "validating"
validating = a
verification_pending :: Proxy "verification_pending"
verification_pending = a
verification_failed :: Proxy "verification_failed"
verification_failed = a
no_doc_available :: Proxy "no_doc_available"
no_doc_available = a
issue_with_dl_image :: Proxy "issue_with_dl_image"
issue_with_dl_image = a
still_have_some_doubt :: Proxy "still_have_some_doubt"
still_have_some_doubt = a
issue_with_rc_image :: Proxy "issue_with_rc_image"
issue_with_rc_image = a
please_check_for_image_if_valid_document_image_or_not :: Proxy "please_check_for_image_if_valid_document_image_or_not"
please_check_for_image_if_valid_document_image_or_not = a
oops_your_application_has_been_rejected :: Proxy "oops_your_application_has_been_rejected"
oops_your_application_has_been_rejected = a
invalid_driving_license :: Proxy "invalid_driving_license"
invalid_driving_license = a
limit_exceeded_for_dl_upload :: Proxy "limit_exceeded_for_dl_upload"
limit_exceeded_for_dl_upload = a
invalid_vehicle_registration_certificate :: Proxy "invalid_vehicle_registration_certificate"
invalid_vehicle_registration_certificate = a
limit_exceeded_for_rc_upload :: Proxy "limit_exceeded_for_rc_upload"
limit_exceeded_for_rc_upload = a
your_documents_are_approved :: Proxy "your_documents_are_approved"
your_documents_are_approved = a
application_status :: Proxy "application_status"
application_status = a
for_support :: Proxy "for_support"
for_support = a
contact_us :: Proxy "contact_us"
contact_us = a
image_validation_failed :: Proxy "image_validation_failed"
image_validation_failed = a
image_not_readable :: Proxy "image_not_readable"
image_not_readable = a
image_low_quality :: Proxy "image_low_quality"
image_low_quality = a
image_invalid_type :: Proxy "image_invalid_type"
image_invalid_type = a
image_document_number_mismatch :: Proxy "image_document_number_mismatch"
image_document_number_mismatch = a
image_extraction_failed :: Proxy "image_extraction_failed"
image_extraction_failed = a
image_not_found :: Proxy "image_not_found"
image_not_found = a
image_not_valid :: Proxy "image_not_valid"
image_not_valid = a
driver_already_linked :: Proxy "driver_already_linked"
driver_already_linked = a
dl_already_updated :: Proxy "dl_already_updated"
dl_already_updated = a
rc_already_linked :: Proxy "rc_already_linked"
rc_already_linked = a
rc_already_updated :: Proxy "rc_already_updated"
rc_already_updated = a
dl_already_linked :: Proxy "dl_already_linked"
dl_already_linked = a
something_went_wrong :: Proxy "something_went_wrong"
something_went_wrong = a
pickup :: Proxy "pickup"
pickup = a
trip :: Proxy "trip"
trip = a
currently_we_allow_only_karnataka_registered_number :: Proxy "currently_we_allow_only_karnataka_registered_number"
currently_we_allow_only_karnataka_registered_number = a
re_enter_vehicle_registration_number :: Proxy "re_enter_vehicle_registration_number"
re_enter_vehicle_registration_number = a
re_enter_driving_license_number :: Proxy "re_enter_driving_license_number"
re_enter_driving_license_number = a
updated_at :: Proxy "updated_at"
updated_at = a
trip_count :: Proxy "trip_count"
trip_count = a
todays_earnings :: Proxy "todays_earnings"
todays_earnings = a
todays_earnings_str :: Proxy "todays_earnings_str"
todays_earnings_str = a
bonus_earned :: Proxy "bonus_earned"
bonus_earned = a
what_is_namma_yatri_bonus :: Proxy "what_is_namma_yatri_bonus"
what_is_namma_yatri_bonus = a
bonus_primary_text :: Proxy "bonus_primary_text"
bonus_primary_text = a
bonus_secondary_text :: Proxy "bonus_secondary_text"
bonus_secondary_text = a
date_of_registration :: Proxy "date_of_registration"
date_of_registration = a
select_date_of_registration :: Proxy "select_date_of_registration"
select_date_of_registration = a
date_of_issue :: Proxy "date_of_issue"
date_of_issue = a
provide_date_of_issue_text :: Proxy "provide_date_of_issue_text"
provide_date_of_issue_text = a
provide_date_of_registration_text :: Proxy "provide_date_of_registration_text"
provide_date_of_registration_text = a
select_date_of_issue :: Proxy "select_date_of_issue"
select_date_of_issue = a
same_reentered_rc_message :: Proxy "same_reentered_rc_message"
same_reentered_rc_message = a
same_reentered_dl_message :: Proxy "same_reentered_dl_message"
same_reentered_dl_message = a
where_is_my_issue_date :: Proxy "where_is_my_issue_date"
where_is_my_issue_date = a
where_is_my_registration_date :: Proxy "where_is_my_registration_date"
where_is_my_registration_date = a
earnings_credited_in_account :: Proxy "earnings_credited_in_account"
earnings_credited_in_account = a
invalid_parameters :: Proxy "invalid_parameters"
invalid_parameters = a
unauthorized :: Proxy "unauthorized"
unauthorized = a
invalid_token :: Proxy "invalid_token"
invalid_token = a
some_error_occured_in_offerride :: Proxy "some_error_occured_in_offerride"
some_error_occured_in_offerride = a
select_vehicle_type :: Proxy "select_vehicle_type"
select_vehicle_type = a
ride :: Proxy "ride"
ride = a
no_location_update :: Proxy "no_location_update"
no_location_update = a
got_it_tell_us_more :: Proxy "got_it_tell_us_more"
got_it_tell_us_more = a
write_a_comment :: Proxy "write_a_comment"
write_a_comment = a
how_was_your_ride_with :: Proxy "how_was_your_ride_with"
how_was_your_ride_with = a
rude_behaviour :: Proxy "rude_behaviour"
rude_behaviour = a
long_waiting_time :: Proxy "long_waiting_time"
long_waiting_time = a
didnt_come_to_picup_location :: Proxy "didnt_come_to_picup_location"
didnt_come_to_picup_location = a
help_us_with_your_reason :: Proxy "help_us_with_your_reason"
help_us_with_your_reason = a
max_char_limit_reached :: Proxy "max_char_limit_reached"
max_char_limit_reached = a
show_all_options :: Proxy "show_all_options"
show_all_options = a
update_required :: Proxy "update_required"
update_required = a
please_update_app_to_continue_service :: Proxy "please_update_app_to_continue_service"
please_update_app_to_continue_service = a
not_now :: Proxy "not_now"
not_now = a
of_ :: Proxy "of"
of_= a
drop :: Proxy "drop"
drop = a
please_wait :: Proxy "please_wait"
please_wait = a
setting_you_offline :: Proxy "setting_you_offline"
setting_you_offline = a
setting_you_online :: Proxy "setting_you_online"
setting_you_online = a
setting_you_silent :: Proxy "setting_you_silent"
setting_you_silent = a
view_breakdown :: Proxy "view_breakdown"
view_breakdown = a
app_info :: Proxy "app_info"
app_info = a
other :: Proxy "other"
other = a
vehicle_issue :: Proxy "vehicle_issue"
vehicle_issue = a
fare_updated :: Proxy "fare_updated"
fare_updated = a
frequent_cancellations_will_lead_to_less_rides :: Proxy "frequent_cancellations_will_lead_to_less_rides"
frequent_cancellations_will_lead_to_less_rides = a
continue :: Proxy "continue"
continue = a
confirm_password :: Proxy "confirm_password"
confirm_password = a
demo_mode :: Proxy "demo_mode"
demo_mode = a
password :: Proxy "password"
password = a
enter_demo_mode_password :: Proxy "enter_demo_mode_password"
enter_demo_mode_password = a
demo_mode_disabled :: Proxy "demo_mode_disabled"
demo_mode_disabled = a
online_via_demo_mode :: Proxy "online_via_demo_mode"
online_via_demo_mode = a
more :: Proxy "more"
more = a
less :: Proxy "less"
less = a
you_are_at_pickup :: Proxy "you_are_at_pickup"
you_are_at_pickup = a
waiting_for_customer :: Proxy "waiting_for_customer"
waiting_for_customer = a
customer_notified :: Proxy "customer_notified"
customer_notified = a
pickup_too_far :: Proxy "pickup_too_far"
pickup_too_far = a
customer_not_picking_call :: Proxy "customer_not_picking_call"
customer_not_picking_call = a
traffic_jam :: Proxy "traffic_jam"
traffic_jam = a
customer_was_rude :: Proxy "customer_was_rude"
customer_was_rude = a
all_messages :: Proxy "all_messages"
all_messages = a
messages :: Proxy "messages"
messages = a
add_a_comment :: Proxy "add_a_comment"
add_a_comment = a
post_comment :: Proxy "post_comment"
post_comment = a
enter_your_comment :: Proxy "enter_your_comment"
enter_your_comment = a
no_notifications_right_now :: Proxy "no_notifications_right_now"
no_notifications_right_now = a
no_notifications_right_now_desc :: Proxy "no_notifications_right_now_desc"
no_notifications_right_now_desc = a
alerts :: Proxy "alerts"
alerts = a
your_comment :: Proxy "your_comment"
your_comment = a
show_more :: Proxy "show_more"
show_more = a
load_older_alerts :: Proxy "load_older_alerts"
load_older_alerts = a
contest :: Proxy "contest"
contest = a
your_referral_code_is_linked :: Proxy "your_referral_code_is_linked"
your_referral_code_is_linked = a
you_can_now_earn_rewards :: Proxy "you_can_now_earn_rewards"
you_can_now_earn_rewards = a
coming_soon :: Proxy "coming_soon"
coming_soon = a
coming_soon_description :: Proxy "coming_soon_description"
coming_soon_description = a
referral_code_number :: Proxy "referral_code_number"
referral_code_number = a
referral_code_hint :: Proxy "referral_code_hint"
referral_code_hint = a
confirm_referral_code :: Proxy "confirm_referral_code"
confirm_referral_code = a
confirm_referral_code_hint :: Proxy "confirm_referral_code_hint"
confirm_referral_code_hint = a
your_referral_code :: Proxy "your_referral_code"
your_referral_code = a
first_referral_successful :: Proxy "first_referral_successful"
first_referral_successful = a
awaiting_referral_ride :: Proxy "awaiting_referral_ride"
awaiting_referral_ride = a
check_this_space_when_you_get_referral_alert :: Proxy "check_this_space_when_you_get_referral_alert"
check_this_space_when_you_get_referral_alert = a
referred_customers :: Proxy "referred_customers"
referred_customers = a
activated_customers :: Proxy "activated_customers"
activated_customers = a
referral_code_linking :: Proxy "referral_code_linking"
referral_code_linking = a
contact_support :: Proxy "contact_support"
contact_support = a
call_support :: Proxy "call_support"
call_support = a
you_are_about_to_call_namma_yatri_support :: Proxy "you_are_about_to_call_namma_yatri_support"
you_are_about_to_call_namma_yatri_support = a
referral_enrolment :: Proxy "referral_enrolment"
referral_enrolment = a
referrals :: Proxy "referrals"
referrals = a
link_referral_code :: Proxy "link_referral_code"
link_referral_code = a
driver_details :: Proxy "driver_details"
driver_details = a
for_updates_see_alerts :: Proxy "for_updates_see_alerts"
for_updates_see_alerts = a
share_options :: Proxy "share_options"
share_options = a
enter_password :: Proxy "enter_password"
enter_password = a
welcome_text :: Proxy "welcome_text"
welcome_text = a
about_text :: Proxy "about_text"
about_text = a
your_vehicle :: Proxy "your_vehicle"
your_vehicle = a
booking_options :: Proxy "booking_options"
booking_options = a
confirm_and_change :: Proxy "confirm_and_change"
confirm_and_change = a
make_yourself_available_for :: Proxy "make_yourself_available_for"
make_yourself_available_for = a
otp_ :: Proxy "otp_"
otp_ = a
choose_language :: Proxy "choose_language"
choose_language = a
ride_fare :: Proxy "ride_fare"
ride_fare = a
ride_distance :: Proxy "ride_distance"
ride_distance = a
message :: Proxy "message"
message = a
start_your_chat_using_these_quick_chat_suggestions :: Proxy "start_your_chat_using_these_quick_chat_suggestions"
start_your_chat_using_these_quick_chat_suggestions = a
start_your_chat_with_the_driver :: Proxy "start_your_chat_with_the_driver"
start_your_chat_with_the_driver = a
i_am_on_my_way :: Proxy "i_am_on_my_way"
i_am_on_my_way = a
getting_delayed_please_wait :: Proxy "getting_delayed_please_wait"
getting_delayed_please_wait = a
unreachable_please_call_back :: Proxy "unreachable_please_call_back"
unreachable_please_call_back = a
are_you_staring :: Proxy "are_you_staring"
are_you_staring = a
please_come_soon :: Proxy "please_come_soon"
please_come_soon = a
ok_i_will_wait :: Proxy "ok_i_will_wait"
ok_i_will_wait = a
i_have_arrived :: Proxy "i_have_arrived"
i_have_arrived = a
please_come_fast_i_am_waiting :: Proxy "please_come_fast_i_am_waiting"
please_come_fast_i_am_waiting = a
please_wait_i_will_be_there :: Proxy "please_wait_i_will_be_there"
please_wait_i_will_be_there = a
looking_for_you_at_pickup :: Proxy "looking_for_you_at_pickup"
looking_for_you_at_pickup = a
silent :: Proxy "silent"
silent = a
try_silent_mode :: Proxy "try_silent_mode"
try_silent_mode = a
silent_mode_prompt :: Proxy "silent_mode_prompt"
silent_mode_prompt = a
go_silent :: Proxy "go_silent"
go_silent = a
go_online :: Proxy "go_online"
go_online = a
go_online_prompt :: Proxy "go_online_prompt"
go_online_prompt = a
live_dashboard :: Proxy "live_dashboard"
live_dashboard = a
click_to_access_your_account :: Proxy "click_to_access_your_account"
click_to_access_your_account = a
add_alternate_number :: Proxy "add_alternate_number"
add_alternate_number = a
enter_alternate_mobile_number :: Proxy "enter_alternate_mobile_number"
enter_alternate_mobile_number = a
please_enter_a_valid_10_digit_number :: Proxy "please_enter_a_valid_10_digit_number"
please_enter_a_valid_10_digit_number = a
alternate_mobile_number :: Proxy "alternate_mobile_number"
alternate_mobile_number = a
remove :: Proxy "remove"
remove = a
remove_alternate_number :: Proxy "remove_alternate_number"
remove_alternate_number = a
are_you_sure_you_want_to_remove_your_alternate_mobile_number :: Proxy "are_you_sure_you_want_to_remove_your_alternate_mobile_number"
are_you_sure_you_want_to_remove_your_alternate_mobile_number = a
yes_remove_it :: Proxy "yes_remove_it"
yes_remove_it = a
number_removed_successfully :: Proxy "number_removed_successfully"
number_removed_successfully = a
edit_alternate_mobile_number :: Proxy "edit_alternate_mobile_number"
edit_alternate_mobile_number = a
number_added_successfully :: Proxy "number_added_successfully"
number_added_successfully = a
number_edited_successfully :: Proxy "number_edited_successfully"
number_edited_successfully = a
alternate_mobile_otp_limit_exceed :: Proxy "alternate_mobile_otp_limit_exceed"
alternate_mobile_otp_limit_exceed = a
attempts_left :: Proxy "attempts_left"
attempts_left = a
wrong_otp :: Proxy "wrong_otp"
wrong_otp = a
otp_limit_exceeded :: Proxy "otp_limit_exceeded"
otp_limit_exceeded = a
otp_limit_exceeded_message :: Proxy "otp_limit_exceeded_message"
otp_limit_exceeded_message = a
try_again_later :: Proxy "try_again_later"
try_again_later = a
attempt_left :: Proxy "attempt_left"
attempt_left = a
number_already_exist_error :: Proxy "number_already_exist_error"
number_already_exist_error = a
please_ask_rider_for_the_otp :: Proxy "please_ask_rider_for_the_otp"
please_ask_rider_for_the_otp = a
your_limit_exceeded_try_again_after_10_min :: Proxy "your_limit_exceeded_try_again_after_10_min"
your_limit_exceeded_try_again_after_10_min = a
i_arrived :: Proxy "i_arrived"
i_arrived = a
estimated_ride_fare :: Proxy "estimated_ride_fare"
estimated_ride_fare = a
complete_onboarding :: Proxy "complete_onboarding"
complete_onboarding = a
person_with_this_number_already_exists :: Proxy "person_with_this_number_already_exists"
person_with_this_number_already_exists = a
resolved_issue :: Proxy "resolved_issue"
resolved_issue = a
ongoing_issue :: Proxy "ongoing_issue"
ongoing_issue = a
lost_item :: Proxy "lost_item"
lost_item = a
ride_related_issue :: Proxy "ride_related_issue"
ride_related_issue = a
app_related_issue :: Proxy "app_related_issue"
app_related_issue = a
fare_related_issue :: Proxy "fare_related_issue"
fare_related_issue = a
issue_number :: Proxy "issue_number"
issue_number = a
remove_issue :: Proxy "remove_issue"
remove_issue = a
call_support_number :: Proxy "call_support_number"
call_support_number = a
years_ago :: Proxy "years_ago"
years_ago = a
months_ago :: Proxy "months_ago"
months_ago = a
days_ago :: Proxy "days_ago"
days_ago = a
hours_ago :: Proxy "hours_ago"
hours_ago = a
min_ago :: Proxy "min_ago"
min_ago = a
sec_ago :: Proxy "sec_ago"
sec_ago = a
verification_is_taking_a_bit_longer :: Proxy "verification_is_taking_a_bit_longer"
verification_is_taking_a_bit_longer = a
demo :: Proxy "demo"
demo = a
ride_related :: Proxy "ride_related"
ride_related = a
fare :: Proxy "fare"
fare = a
app_related :: Proxy "app_related"
app_related = a