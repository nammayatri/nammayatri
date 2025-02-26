module Resource.Localizable.TypesV2 where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))

newtype Languages
  = Languages
  { english :: Keymap
  , hindi :: Keymap
  , malayalam :: Keymap
  , bengali :: Keymap
  , tamil :: Keymap
  , kannada :: Keymap
  , telugu :: Keymap
  , odiya :: Keymap
  }

newtype Keymap
  = Keymap
  { lets_get_started :: String
  , language_updated :: String
  , your_application_has_been_submitted_successfully_and_is_under_verification :: String
  , view_status :: String
  , go_home :: String
  , select_language :: String
  , which_language_do_you_prefer :: String
  , t_c :: String
  , enter_mobile_number :: String
  , by_clicking_continue_you_will_be_agreeing_to_our :: String
  , enter_otp :: String
  , didnt_recieve_otp :: String
  , resend_otp :: String
  , please_enter_valid_otp :: String
  , invalid_mobile_number :: String
  , register :: String
  , mobile_number :: String
  , auto_reading_otp :: String
  , upload_driving_license :: String
  , upload_back_side :: String
  , upload_front_side :: String
  , back_side :: String
  , front_side :: String
  , next :: String
  , license_instruction_picture :: String
  , license_instruction_clarity :: String
  , registration_steps :: String
  , progress_saved :: String
  , driving_license :: String
  , aadhar_card :: String
  , bank_details :: String
  , vehicle_details :: String
  , upload_front_back :: String
  , earnings_will_be_credited :: String
  , fill_vehicle_details :: String
  , follow_steps :: String
  , registration :: String
  , upload_adhaar_card :: String
  , adhaar_intruction_picture :: String
  , add_vehicle_details :: String
  , vehicle_registration_number :: String
  , enter_vehicle_no :: String
  , vehicle_type :: String
  , vehicle_model_name :: String
  , enter_model_name :: String
  , vehicle_colour :: String
  , enter_vehicle_colour :: String
  , upload_registration_certificate :: String
  , upload_rc :: String
  , preview :: String
  , choose_vehicle_type :: String
  , max_images :: String
  , re_enter_benificiary_number :: String
  , ifsc_code :: String
  , benificiary_number :: String
  , sending_otp :: String
  , loading :: String
  , please_wait_while_in_progress :: String
  , your_request_has_timeout_try_again :: String
  , error_occured_please_try_again_later :: String
  , country_code_india :: String
  , enter_otp_sent_to :: String
  , otp_sent_to :: String
  , enter_account_number :: String
  , add_bank_details :: String
  , enter_ifsc_code :: String
  , submit :: String
  , personal_details :: String
  , languages :: String
  , help_and_faq :: String
  , about :: String
  , logout :: String
  , update :: String
  , edit :: String
  , delete :: String
  , view :: String
  , issue_no :: String
  , add_voice_note :: String
  , voice_note_added :: String
  , added_images :: String
  , no_images_added :: String
  , ask_details_message :: String
  , ask_details_message_reversed :: String
  , select_option :: String
  , select_option_reversed :: String
  , issue_submitted_message :: String
  , submit_issue_details :: String
  , image_preview :: String
  , ride_report_issue :: String
  , i_dont_know_which_ride :: String
  , report_issue_chat_placeholder :: String -> String
  , added_voice_note :: String
  , no_voice_note_added :: String
  , call_customer_title :: String
  , call_customer_description :: String
  , place_call :: String
  , place_call_request :: String
  , add_image :: String
  , add_another :: String
  , images_added :: String
  , issue_submitted_text :: String
  , choose_an_option :: String
  , image_added :: String
  , done :: String
  , record_voice_note :: String
  , auto :: String
  , name :: String
  , privacy_policy :: String
  , logo :: String
  , about_app_description :: String
  , terms_and_conditions :: String
  , update_vehicle_details :: String
  , help_and_support :: String
  , note :: String
  , visit_my_rides_screen_for_specific_complaints :: String
  , thank_you_for_wrtitting_us :: String
  , go_to_home :: String
  , your_recent_ride :: String
  , your_recent_trip :: String
  , all_topics :: String
  , report_an_issue_with_this_trip :: String
  , you_rated :: String
  , view_all_rides :: String
  , write_to_us :: String
  , subject :: String
  , your_email_id :: String
  , more_options :: String
  , describe_your_issue :: String
  , getting_started_and_faq :: String
  , ongoing_issues :: String
  , resolved_issues :: String
  , for_other_issues_write_to_us :: String
  , call_support_center :: String
  , you_can_describe_issue_that_you_faced_here :: String
  , registration_certificate_image :: String
  , home :: String
  , rides :: String
  , my_rides :: String
  , profile :: String
  , enter_driving_license_number :: String
  , trip_details :: String
  , by_cash :: String
  , online_ :: String
  , go_online_popup :: String
  , distance :: String
  , coin_balance :: String
  , points_balance :: String
  , total_earned :: String
  , ride_history :: String
  , transaction_history :: String
  , points_earned :: String
  , no_rides :: String
  , points_used :: String
  , use_points :: String
  , insights :: String
  , usage_history :: String
  , no_points_earned :: String
  , no_points_used :: String
  , earn_points_by_taking_rides_and_referring_the_app_to_others :: String -> String
  , use_them_before_they_expire :: String
  , no_ride_history_available :: String
  , you_have_not_completed_a_ride_yet :: String
  , complete_first_ride_to_unlock_points :: String
  , destination :: String
  , you_did_not_take_any_rides_on_prefix :: String
  , you_did_not_take_any_rides_on_suffix :: String
  , convert :: String
  , cash_converted :: String
  , will_be_adjusted_in_your_future_subscription_dues :: String
  , has_been_adjusted_in_your_subscription_dues :: String
  , using_points_requires_an_active_plan :: String
  , to_get_started :: String
  , converted_from_points :: String
  , report_an_issue :: String
  , time_taken :: String
  , maps :: String
  , call :: String
  , start_ride :: String
  , cancel_ride :: String
  , please_tell_us_why_you_want_to_cancel :: String
  , mandatory :: String
  , end_ride :: String
  , ride_completed_with :: String
  , collect_amount_in_cash :: String
  , cash_collected :: String
  , offline :: String
  , accept_for :: String
  , decline :: String
  , request :: String
  , you_are_offline :: String
  , you_are_currently_busy_go_online_to_recieve_trip_requests :: String
  , going_offline_will_not_get_you_any_ride :: String
  , cancel :: String
  , go_offline :: String
  , is_waiting_for_you :: String
  , you_are_on_a_ride :: String
  , please_ask_the_customer_for_the_otp :: String
  , enter_current_odometer_reading :: String
  , enter_final_odo_reading :: String
  , enter_the_last_4_digits_of_odometer :: String
  , enter_the_digits_of_odometer :: String
  , odometer_reading_validation_failed :: String
  , completed_ :: String
  , cancelled_ :: String
  , where_is_my_license_number :: String
  , we_need_some_access :: String
  , allow_access :: String
  , enter_rc_number :: String
  , where_is_my_rc_number :: String
  , we_have_recieved_your_issue :: String
  , thank_you_for_writing_to_us :: String
  , rider :: String
  , trip_id :: String
  , need_it_to_show_you_incoming_ride_request :: String
  , need_it_to_disable_battery_optimization_for_the_app :: String
  , need_it_to_autostart_your_app :: String
  , need_it_to_enable_location :: String -> String
  , overlay_to_draw_over_applications :: String
  , battery_optimizations :: String
  , auto_start_application_in_background :: String
  , location_access :: String
  , step :: String
  , paid :: String
  , entered_wrong_otp :: String
  , otp_invalid_for_this_vehicle_variant :: String
  , copied :: String
  , bank_name :: String
  , aadhar_details :: String
  , aadhar_number :: String
  , front_side_image :: String
  , back_side_image :: String
  , still_not_resolved :: String
  , case_two :: String
  , non_disclouser_agreement :: String
  , data_collection_authority :: String
  , software_license :: String
  , load_more :: String
  , are_you_sure_you_want_to_logout :: String
  , go_back :: String
  , thank_you_for_registering_us :: String
  , unfortanutely_we_are_not_available__yet_for_you :: String
  , are_you_sure_you_want_to_end_the_ride :: String
  , empty_rides :: String
  , you_have_not_taken_a_trip_yet :: String
  , book_now :: String
  , resend_otp_in :: String
  , we_need_access_to_your_location :: String
  , your_location_helps_our_system :: String -> String
  , no_internet_connection :: String
  , please_check_your_internet_connection_and_try_again :: String
  , try_again :: String
  , grant_access :: String
  , enter_referral_mobile_number :: String
  , apply :: String
  , have_a_referral :: String
  , add_here :: String
  , referral_applied :: String
  , smalledit :: String
  , add_driving_license :: String
  , help :: String
  , invalid_dl_number :: String
  , driving_license_number :: String
  , enter_dl_number :: String
  , select_date_of_birth :: String
  , date_of_birth :: String
  , watch_a_tutorial_for_easy_registration :: String
  , enter_minimum_fifteen_characters :: String
  , add_your_friend :: String
  , please_wait_while_validating_the_image :: String
  , validating :: String
  , verification_pending :: String
  , verification_failed :: String
  , no_doc_available :: String
  , issue_with_dl_image :: String
  , still_have_some_doubt :: String
  , issue_with_rc_image :: String
  , please_check_for_image_if_valid_document_image_or_not :: String
  , oops_your_application_has_been_rejected :: String
  , invalid_driving_license :: String
  , limit_exceeded_for_dl_upload :: String
  , invalid_vehicle_registration_certificate :: String
  , limit_exceeded_for_rc_upload :: String
  , your_documents_are_approved :: String
  , application_status :: String
  , for_support :: String
  , contact_us :: String
  , image_validation_failed :: String
  , image_not_readable :: String
  , image_low_quality :: String
  , image_invalid_type :: String
  , image_document_number_mismatch :: String
  , image_extraction_failed :: String
  , image_not_found :: String
  , image_not_valid :: String
  , driver_already_linked :: String
  , dl_already_updated :: String
  , rc_already_linked :: String
  , rc_already_updated :: String
  , dl_already_linked :: String
  , something_went_wrong :: String
  , pickup :: String
  , trip :: String
  , currently_we_allow_only_karnataka_registered_number :: String -> String
  , re_enter_vehicle_registration_number :: String
  , re_enter_driving_license_number :: String
  , updated_at :: String
  , trip_count :: String
  , todays_earnings :: String
  , todays_earnings_str :: String
  , bonus_earned :: String
  , what_is_namma_yatri_bonus :: String -> String
  , bonus_primary_text :: String -> String
  , bonus_secondary_text :: String -> String
  , date_of_registration :: String
  , select_date_of_registration :: String
  , date_of_issue :: String
  , provide_date_of_issue_text :: String
  , provide_date_of_registration_text :: String
  , select_date_of_issue :: String
  , same_reentered_rc_message :: String
  , same_reentered_dl_message :: String
  , where_is_my_issue_date :: String
  , where_is_my_registration_date :: String
  , earnings_credited_in_account :: String
  , invalid_parameters :: String
  , unauthorized :: String
  , invalid_token :: String
  , some_error_occured_in_offerride :: String
  , select_vehicle_type :: String
  , ride :: String
  , no_location_update :: String
  , got_it_tell_us_more :: String
  , write_a_comment :: String
  , how_was_your_ride_with :: String
  , rude_behaviour :: String
  , long_waiting_time :: String
  , didnt_come_to_picup_location :: String
  , help_us_with_your_reason :: String
  , max_char_limit_reached :: String
  , show_all_options :: String
  , update_required :: String
  , please_update_app_to_continue_service :: String
  , not_now :: String
  , of_ :: String
  , drop :: String
  , please_wait :: String
  , setting_you_offline :: String
  , setting_you_online :: String
  , setting_you_silent :: String
  , view_breakdown :: String
  , app_info :: String
  , other :: String
  , vehicle_issue :: String
  , fare_updated :: String
  , frequent_cancellations_will_lead_to_less_rides :: String
  , frequent_cancellations_will_lead_to_blocking :: String
  , continue :: String
  , confirm_password :: String
  , demo_mode :: String
  , password :: String
  , enter_demo_mode_password :: String
  , demo_mode_disabled :: String
  , online_via_demo_mode :: String
  , more :: String
  , less :: String
  , you_are_at_pickup :: String
  , waiting_for_customer :: String
  , customer_notified :: String
  , pickup_too_far :: String
  , customer_not_picking_call :: String
  , traffic_jam :: String
  , customer_was_rude :: String
  , all_messages :: String
  , messages :: String
  , add_a_comment :: String
  , post_comment :: String
  , enter_your_comment :: String
  , no_notifications_right_now :: String
  , no_notifications_right_now_desc :: String
  , alerts :: String
  , your_comment :: String
  , show_more :: String
  , load_older_alerts :: String
  , contest :: String
  , your_referral_code_is_linked :: String
  , you_can_now_earn_rewards :: String
  , coming_soon :: String
  , coming_soon_description :: String
  , referral_code_number :: String
  , referral_code_hint :: String
  , confirm_referral_code :: String
  , confirm_referral_code_hint :: String
  , your_referral_code :: String
  , first_referral_successful :: String
  , awaiting_referral_ride :: String
  , check_this_space_when_you_get_referral_alert :: String
  , referred_customers :: String
  , activated_customers :: String
  , referral_code_linking :: String
  , contact_support :: String
  , ac_check_titile :: String
  , call_support :: String
  , you_are_about_to_call_namma_yatri_support :: String -> String
  , referral_enrolment :: String
  , referrals :: String
  , link_referral_code :: String
  , driver_details :: String
  , for_updates_see_alerts :: String
  , share_options :: String
  , enter_password :: String
  , welcome_text :: String
  , about_text :: String
  , your_vehicle :: String
  , booking_options :: String
  , confirm_and_change :: String
  , make_yourself_available_for :: String
  , otp_ :: String
  , choose_language :: String
  , ride_fare :: String
  , ride_distance :: String
  , message :: String
  , start_your_chat_using_these_quick_chat_suggestions :: String
  , start_your_chat_with_the_driver :: String
  , i_am_on_my_way :: String
  , getting_delayed_please_wait :: String
  , unreachable_please_call_back :: String
  , are_you_staring :: String
  , please_come_soon :: String
  , ok_i_will_wait :: String
  , i_have_arrived :: String
  , please_come_fast_i_am_waiting :: String
  , please_wait_i_will_be_there :: String
  , looking_for_you_at_pickup :: String
  , silent :: String
  , try_silent_mode :: String
  , silent_mode_prompt :: String
  , go_silent :: String
  , go_online :: String
  , go_online_prompt :: String
  , live_dashboard :: String
  , click_to_access_your_account :: String
  , add_alternate_number :: String
  , enter_alternate_mobile_number :: String
  , please_enter_a_valid_10_digit_number :: String
  , alternate_mobile_number :: String
  , remove :: String
  , remove_alternate_number :: String
  , are_you_sure_you_want_to_remove_your_alternate_mobile_number :: String
  , yes_remove_it :: String
  , number_removed_successfully :: String
  , edit_alternate_mobile_number :: String
  , number_added_successfully :: String
  , number_edited_successfully :: String
  , alternate_mobile_otp_limit_exceed :: String
  , attempts_left :: String
  , wrong_otp :: String
  , otp_limit_exceeded :: String
  , otp_limit_exceeded_message :: String
  , try_again_later :: String
  , attempt_left :: String
  , number_already_exist_error :: String
  , please_ask_rider_for_the_otp :: String
  , your_limit_exceeded_try_again_after_10_min :: String
  , i_arrived :: String
  , estimated_ride_fare :: String
  , complete_onboarding :: String
  , person_with_this_number_already_exists :: String
  , resolved_issue :: String
  , ongoing_issue :: String
  , lost_item :: String
  , ride_related_issue :: String
  , app_related_issue :: String
  , fare_related_issue :: String
  , issue_number :: String
  , remove_issue :: String
  , call_support_number :: String
  , years_ago :: String
  , months_ago :: String
  , days_ago :: String
  , hours_ago :: String
  , min_ago :: String
  , sec_ago :: String
  , verification_is_taking_a_bit_longer :: String
  , demo :: String
  , ride_related :: String
  , fare :: String
  , app_related :: String
  , lost_and_found :: String
  , report_lost_item :: String
  , corporate_address :: String -> String
  , corporate_address_description :: String -> String
  , corporate_address_description_additional :: String -> String
  , registered_address :: String -> String
  , registered_address_description :: String -> String
  , registered_address_description_additional :: String -> String
  , select_the_languages_you_can_speak :: String
  , gender :: String
  , select_your_gender :: String
  , male :: String
  , female :: String
  , prefer_not_to_say :: String
  , set_now :: String
  , complete_your_profile_and_find_more_rides :: String
  , update_now :: String
  , confirm :: String
  , gender_updated :: String
  , zone_cancel_text_drop :: String
  , zone_cancel_text_pickup :: String
  , rankings :: String
  , getting_the_leaderboard_ready :: String
  , please_wait_while_we_update_the_details :: String
  , last_updated :: String
  , congratulations_you_are_rank :: String
  , you :: String
  , daily :: String
  , inaccurate_date_and_time :: String
  , adjust_your_device_date_and_time_and_try_again :: String
  , the_current_date_and_time_is :: String
  , go_to_setting :: String
  , accept_rides_to_enter_rankings :: String
  , otp_has_been_resent :: String
  , otp_entering_limit_exhausted_please_try_resending_otp :: String
  , otp_resent_limit_exhausted_please_try_again_later :: String
  , otp_page_has_been_expired_please_request_otp_again :: String
  , something_went_wrong_please_try_again :: String
  , invalid_referral_code :: String
  , issue_removed_successfully :: String
  , otp_entering_limit_exhausted_please_try_again_later :: String
  , too_many_attempts_please_try_again_later :: String
  , invalid_referral_number :: String
  , something_went_wrong_try_again_later :: String
  , wait_time :: String
  , wait_timer :: String
  , how_long_waited_for_pickup :: String
  , customer_will_pay_for_every_minute :: String -> String -> String
  , others :: String
  , enter_second_sim_number :: String
  , alternate_number :: String
  , limit_exceeded_for_alternate_number :: String
  , add_alternate_number_in_meantime :: String
  , otp_resend_limit_exceeded :: String
  , alternate_number_cannot_be_added :: String
  , otp_resent :: String
  , sedan :: String
  , suv :: String
  , hatchback :: String
  , auto_rickshaw :: String
  , taxi :: String
  , taxi_plus :: String
  , my_profile :: String
  , settings :: String
  , reg_number :: String
  , type_ :: String
  , model_name :: String
  , colour :: String
  , badges :: String
  , edit_rc :: String
  , deactivate_rc :: String
  , activate_rc :: String
  , delete_rc :: String
  , call_driver :: String
  , call_customer_support :: String
  , active_rc_on_another_driver :: String
  , call_driver_or_contact_support :: String
  , skip :: String
  , active_str :: String
  , inactive_rc :: String
  , confirmation_for_deleting_rc :: String
  , yes_delete :: String
  , add_new_rc :: String
  , connect_call_anonymously :: String
  , yes_activate :: String
  , yes_deactivate :: String
  , confirmation_for_deactivating_rc :: String
  , confirmation_for_activating_rc :: String
  , this_will_deactivate_currently_active_rc :: String
  , removed :: String
  , deactivated :: String
  , ride_type_select :: String
  , deactivate :: String
  , vehicles_pending :: String
  , is_active_now :: String
  , single_rc_cannot_be_deleted :: String
  , cancellation_rate :: String
  , rides_cancelled :: String
  , earnings_missed :: String
  , summary :: String
  , namma_bonus :: String -> String
  , trips_completed :: String
  , late_night_trips :: String
  , about_me :: String
  , about_vehicle :: String
  , add :: String
  , years_old :: String
  , from_where :: String
  , missed_opportunity :: String
  , earned_on_app :: String -> String
  , travelled_on_app :: String -> String
  , how_old_is_your_vehicle :: String
  , enter_name_of_vehicle :: String
  , new_ :: String
  , with :: String
  , total_money_collected :: String
  , fare_earned_of_the_day :: String
  , gst_plus_payable :: String
  , to_continue_using_yatri_sathi :: String
  , pay :: String
  , later :: String
  , great_job :: String
  , fee_breakup :: String
  , yatri_sathi_fee_payable_for_date :: String -> String
  , fee_corresponding_to_the_distance :: String
  , platform_fee :: String
  , gst :: String
  , total_payable :: String
  , got_it :: String
  , view_details :: String
  , payment_successful :: String
  , payment_pending :: String
  , payment_failed :: String
  , payment_pending_desc :: String
  , payment_failed_desc :: String -> String
  , we_will_notify_when_payment_success :: String
  , continue_taking_rides :: String
  , your_previous_payment_is_pending :: String
  , goverment_charges :: String
  , today :: String
  , okay :: String
  , no_payment_history_available :: String
  , you_dont_have_any_payments :: String
  , enter_aadhaar_number :: String
  , enter_aadhaar_details :: String
  , enter_aadhaar_otp_ :: String
  , aadhaar_linking_required :: String
  , customer_added_a_stop :: String
  , aadhaar_linking_required_description :: String -> String
  , by_clicking_this_you_will_be_agreeing_to_our_tc :: String
  , terms_and_conditions_short :: String
  , otp_sent_to_aadhaar_number :: String
  , enter_six_digit_otp :: String
  , tc_tail :: String
  , link_aadhaar_id :: String
  , navigate_to_location :: String
  , no_mobile_number_registered :: String
  , exceed_otp_generation_limit :: String
  , aadhaar_number_not_exist :: String
  , invalid_otp :: String
  , no_share_code :: String
  , wrong_share_code :: String
  , invalid_share_code :: String
  , session_expired :: String
  , otp_attempt_exceeded :: String
  , upstream_internal_server_error :: String
  , transaction_already_completed :: String
  , goto_your_nearest_booth :: String
  , aadhaar_already_linked :: String
  , optional :: String
  , download_statement :: String
  , select_a_date_range :: String
  , fee_payment_history :: String
  , languages_spoken :: String
  , view_payment_history :: String
  , ride_type :: String
  , rc_status :: String
  , rated_by_users1 :: String
  , rated_by_users2 :: String
  , months :: String
  , rc_added_successfully :: String
  , call_request_has_been_placed :: String
  , trip_date :: String
  , offer_applied :: String
  , your_earnings :: String
  , number_of_rides :: String
  , fare_breakup :: String
  , my_plan :: String
  , your_dues :: String
  , your_dues_description :: String
  , your_dues_description_manual :: String
  , current_dues :: String
  , your_limit :: String
  , due_details :: String
  , amount :: String
  , view_due_details :: String
  , setup_autopay :: String
  , current_plan :: String
  , alternate_plan :: String
  , autopay_details :: String
  , cancel_autopay_str :: String
  , we_might_be_lost :: String
  , exeperiencing_error :: String
  , enjoy_these_benefits :: String
  , choose_your_plan :: String -> String
  , skip_for_now :: String
  , n_day_free_trial_activated :: String -> String
  , take_n_rides_for_the_next_n_days :: String -> String -> String
  , every_ride_at_zero_commission :: String
  , earn_upto_per_day :: String
  , how_this_works :: String
  , sign_up_for_autopay_by_paying_just :: String
  , get_reminded_about_your_plan_setup :: String
  , free_trial_reminder_n_days_m_rides :: String -> String -> String
  , plan_starts_n_days_m_rides :: String -> String -> String
  , easy_automatic_payments_start :: String
  , free_until :: String
  , per_ride :: String
  , per_day :: String
  , offer :: String
  , offers :: String
  , you_are_on_the_free_trial :: String
  , setup_autopay_before_the_trail_period_expires :: String
  , get_free_trail_until :: String
  , clear_dues :: String
  , payment_pending_alert :: String
  , payment_pending_alert_desc :: String -> String
  , low_account_balance :: String
  , low_account_balance_desc :: String
  , okay_got_it :: String
  , limited_time_offer :: String
  , join_now :: String
  , automatic_payments_will_appear_here :: String
  , manual_payments_will_appear_here :: String
  , manual_payments :: String
  , no_automatic_payments_desc :: String
  , no_manual_payments_desc :: String
  , payment_history :: String
  , plan :: String
  , day :: String
  , tap_a_plan_to_view_details :: String
  , plans :: String
  , how_it_works :: String -> String
  , zero_commision :: String
  , earn_today_pay_tomorrow :: String
  , pay_only_if_you_take_rides :: String
  , manage_plan :: String
  , view_autopay_details :: String
  , switch_and_save :: String
  , switch_and_save_desc :: String
  , switch_now :: String
  , payment_mode_changed_to_manual :: String
  , payment_mode_changed_to_manual_desc :: String
  , autopay_payments :: String
  , success :: String
  , transaction_on :: String
  , debited_on :: String
  , rides_taken_on :: String
  , join_plan :: String
  , join_nammaa_yatri :: String
  , cancel_autopay_and_pay_manually :: String
  , plan_activated_successfully :: String
  , dues_cleared_successfully :: String
  , not_planning_to_take_rides :: String
  , retry_payment_str :: String
  , pause_autopay_str :: String
  , setup_autopay_str :: String
  , view_ride_details :: String
  , account :: String
  , autopay_is_not_enabled_yet :: String
  , enable_autopay_desc :: String
  , enable_autopay_now :: String
  , autopay_setup_pending_str :: String
  , autopay_pending_desc_str :: String
  , refresh_str :: String
  , transaction_details :: String
  , ride_details :: String
  , my_plan_title :: String
  , switch_to :: String
  , your_rental_ride_starts_in :: String
  , your_intercity_ride_starts_in :: String
  , please_try_again :: String
  , plan_not_found :: String
  , mandate_not_found :: String
  , active_mandate_exists :: String
  , no_active_mandate_exist :: String
  , no_plan_for_driver :: String
  , invalid_payment_mode :: String
  , invalid_auto_pay_status :: String
  , max_amount :: String
  , frequency :: String
  , statred_on :: String
  , expires_on :: String
  , switched_plan :: String
  , resumed_autopay :: String
  , onetime :: String
  , weekly :: String
  , fortnightly :: String
  , monthly :: String
  , bimonthly :: String
  , quarterly :: String
  , halfyearly :: String
  , yearly :: String
  , aspresented :: String
  , first_free_ride :: String
  , daily_per_ride_desc :: String
  , join_the_unlimited_plan :: String
  , maybe_later :: String
  , do_you_want_to_cancel :: String
  , do_you_want_to_cancel_desc :: String
  , your_payment_was_unsuccessful :: String
  , payment_cancelled :: String
  , manual_payment_str :: String
  , upi_autopay_s :: String
  , daily_unlimited :: String
  , daily_per_ride :: String
  , daily_unlimited_plan_desc :: String
  , daily_per_ride_plan_desc :: String -> String
  , autopay_cancelled :: String
  , no :: String
  , yes_cancel :: String
  , pay_to_join_this_plan :: String
  , offers_not_applicable :: String
  , paused_str :: String
  , pending_str :: String
  , switch_plan_str :: String
  , offers_applicable_on_daily_unlimited :: String
  , daily_unlimited_offer_not_available :: String
  , plan_switched_to :: String
  , no_rides_no_charge :: String
  , get_special_offers :: String -> String
  , valid_only_if_payment :: String
  , help_str :: String
  , refresh_string :: String
  , chat_for_help :: String
  , view_faqs :: String
  , find_help_centre :: String -> String
  , contact :: String
  , go_to_location :: String
  , no_help_center_is_active_now :: String
  , help_centers_location_will_appear_here_once_they_are_active :: String
  , support :: String
  , need_help_joining_the_plan :: String
  , need_help :: String
  , setup_autopay_now_to_get_special_discounts :: String
  , setup_now :: String
  , go_to_vehicle_details :: String
  , close :: String
  , rc_deactivated :: String
  , rc_deactivated_details :: String
  , customer_has_low_mobility :: String
  , customer_has_disability :: String
  , customer_has_low_vision :: String
  , customer_has_hearing_impairment :: String
  , help_with_their_mobility_aid :: String
  , please_assist_them_if_needed :: String
  , message_them_at_pickup :: String
  , sound_horn_once_at_pickup :: String
  , please_call_and_avoid_chats :: String
  , please_chat_and_avoid_calls :: String
  , please_go_to_exact_pickup :: String
  , customer_has_poor_vision_sound_horn_at_pickup :: String
  , customer_has_poor_hearing_message_them_at_pickup :: String
  , customer_has_low_mobility_store_their_support_at_pickup :: String
  , customer_has_disability_please_assist_them :: String
  , customer_may_need_assistance :: String
  , learn_more :: String
  , customer_has_low_mobility_go_to_exact_loc :: String
  , customer_has_poor_hearing_chat_with_them_instead_of_calling :: String
  , customer_has_low_vision_call_them_instead_of_chatting :: String
  , please_help_them_as_you_can :: String
  , learn_how_you_can_help_customers_requiring_special_assistance :: String
  , assistance_required :: String
  , saved_due_to_zero_commission :: String
  , tip_earned_from_customer :: String
  , collect_via_case_upi :: String
  , fare_collected :: String
  , rate_your_ride_with1 :: String
  , rate_your_ride_with2 :: String
  , help_us_with_your_feedback :: String
  , collect_cash :: String
  , online_payment :: String
  , ride_completed :: String
  , submit_feedback :: String
  , badge_earned :: String
  , purple_ride_champion :: String
  , purple_ride :: String
  , proceed_to_chat :: String
  , please_consider_calling_them :: String
  , join_a_plan_to_start_earning :: String
  , go_online_prompt_subscribe :: String
  , go_online_prompt_payment_pending :: String
  , complete_payment_to_continue :: String -> String
  , downgrade_available_only_for_ac_vehicles :: String
  , downgrading_vehicle_will_allow_you_to_take_both_1 :: String
  , downgrading_vehicle_will_allow_you_to_take_both_2 :: String
  , downgrading_vehicle_will_allow_you_to_take_both_3 :: String
  , ac_cab :: String
  , ac_suv :: String
  , downgrade_vehicle :: String
  , rental_bookings :: String
  , rental_bookings_description :: String
  , pending_caps :: String
  , failure :: String
  , payment_mode :: String
  , txn_id :: String
  , amount_paid :: String
  , notification_scheduled :: String
  , manual_dues :: String
  , autopay_in_progress :: String
  , manual_due_overview :: String
  , autopay_due_overview :: String
  , manual_due_as_autopay_execution_failed :: String
  , clear_manual_dues :: String
  , due_overview :: String
  , manual_due_details :: String
  , autopay_due_details :: String
  , switched_to_manual :: String
  , split_payment :: String
  , gst_include :: String
  , scheduled_at :: String
  , payment_status :: String
  , notification_attempting :: String
  , execution_scheduled :: String
  , execution_attempting :: String
  , execution_success :: String
  , scheduled :: String
  , one_time_settlement :: String
  , payment_scheduled :: String
  , retry_autopay :: String
  , retry_str :: String
  , ongoing_payment_execution :: String
  , offer_card_banner_title :: String -> String -> String -> String
  , offer_card_banner_desc :: String
  , offer_card_banner_alert :: String
  , or :: String
  , collect_cash_directly :: String
  , or_collect_cash_directly :: String
  , setup_autopay_to_accept_payment :: String
  , download_qr :: String
  , use_this_qr_to_collect_payment :: String
  , amount_will_deposited_to_bank_account :: String
  , get_directly_to_your_bank_account :: String
  , payment :: String
  , qr_code :: String
  , get_qr_code :: String
  , execution_failed :: String
  , notification_failed :: String
  , clear_dues_banner_title :: String
  , pay_now :: String
  , collect_via_upi_qr_or_cash :: String
  , transaction_debited_on :: String
  , transaction_attempted_on :: String
  , autopay_setup_and_payment_successful :: String
  , autopay_setup_successful :: String
  , autopay_setup_and_payment_pending :: String
  , autopay_setup_pending :: String
  , autopay_setup_and_payment_failed :: String
  , autopay_setup_failed :: String
  , one_time_registeration :: String
  , clearance_and_registeration :: String
  , upi_autopay_setup :: String
  , watch_video_for_help :: String
  , payment_pending_soft_nudge :: String
  , clear_your_dues_early :: String
  , due_limit_warning_banner_title :: String
  , scheduled_on :: String
  , attempted_on :: String
  , free_trial_ending_tomorrow :: String
  , free_trial_ends_tonight :: String
  , join_a_plan_to_continue_taking_rides :: String
  , setup_autopay_for_easy_payments :: String
  , low_dues_clear_popup_desc :: String
  , dues_pending :: String
  , days :: String
  , active_plan :: String
  , what_are_purple_rides :: String
  , economical :: String
  , spacious :: String
  , comfy :: String
  , people :: String
  , go_to :: String
  , select_on_map :: String
  , confirm_location_str :: String
  , save_location_str :: String
  , remove_pref_loc :: String
  , conf_remove_pref_loc :: String
  , yes_remove :: String
  , add_location :: String
  , add_another_location :: String
  , add_a_goto_loc :: String
  , goto_loc_left :: String
  , current_location :: String
  , conf_goto_loc :: String
  , goto_locs :: String
  , location_str :: String
  , add_tag :: String
  , only_one_loc_can_added :: String
  , save_as :: String
  , no_goto_loc_added :: String
  , goto_loc_helps_you :: String
  , you_are_very_close :: String
  , goto_is_applicable_for :: String
  , cancel_anyway :: String
  , goto_maybe_reduced :: String
  , cancel_of_goto :: String
  , more_goto_ride_coming :: String
  , more_goto_ride_coming_desc :: String
  , goto_reduced_to_zero :: String
  , due_to_multiple_cancellations :: String
  , ok_got_it :: String
  , goto_reduced_to :: String
  , validity_expired_str :: String
  , validity_expired_desc :: String
  , know_more :: String
  , this_feature_will_be_applicable :: String
  , goto_loc_added :: String
  , goto_loc_removed :: String
  , goto_loc_updated :: String
  , goto_loc_is_enabled :: String
  , goto_loc_is_disabled :: String
  , goto_locations :: String
  , choose_a_goto_loc :: String
  , you_have_only_left_for_today :: String
  , yes_enable :: String
  , no_goto_locs_added_yet :: String
  , no_goto_locs_added_yet_desc :: String
  , enable_goto :: String
  , go_to_cancellation_title :: String
  , go_to_cancellation_desc :: String
  , disable_goto_str :: String
  , you_still_have_time_left :: String
  , yes_disable :: String
  , goto_loc_reached :: String
  , you_are_almost_at_location :: String
  , driver_home_location_not_found :: String
  , driver_home_location_does_not_exist :: String
  , driver_home_location_limit_reached :: String
  , driver_go_home_request_not_found :: String
  , driver_go_home_request_does_not_exist :: String
  , driver_go_home_request_daily_usage_limit_reached :: String
  , driver_go_home_request_already_active :: String
  , report_issue :: String
  , driver_home_location_outside_service_area :: String
  , new_location_too_close_to_previous_home_location :: String
  , driver_home_location_does_not_belong_to_driver :: String
  , driver_home_location_delete_while_active_error :: String
  , drag_to_adjust :: String
  , location_already_exists :: String
  , min_left :: String
  , get_ready_for_ys_subscription :: String -> String
  , signup_early_for_special_offers :: String
  , guaranteed_fixed_price :: String -> String
  , introductory_offer_to_be_announced_soon :: String
  , no_charges_till :: String
  , driver_go_home_request_not_present :: String
  , and :: String
  , direct_payment_no_commissions :: String
  , customer_pays_directly :: String
  , hundred_percent_fare_goes_to_you :: String
  , fare_shown_is_fare_you_get :: String
  , be_a_part_of_open_mobility_revolution :: String
  , our_data_and_product_are_transparent :: String
  , your_detected_location_is :: String
  , language_detected :: String
  , change_language_str :: String
  , select_location :: String
  , select_location_desc :: String
  , select_language_desc :: String
  , confirm_language :: String
  , get_started :: String
  , enable_location_permission :: String
  , please_enable_location_permission_for :: String
  , enable_location :: String
  , by_clicking_next_you_will_be_agreeing_to_our :: String
  , enter_your_mobile_number :: String
  , notification_access :: String
  , notification_access_desc :: String
  , watch_video :: String
  , dl_verification_failed :: String
  , rc_verification_failed :: String
  , dl_upload_failed :: String
  , rc_upload_failed :: String
  , please_retry_the_upload_again :: String
  , rc_and_dl_upload_failed :: String
  , rc_upload_limit_reached :: String
  , dl_upload_limit_reached :: String
  , retry_upload :: String
  , vehicle_registeraton_certificate :: String
  , grant_permissions :: String
  , subscription_plan_str :: String -> String
  , complete_autopay_later :: String
  , start_earning_in_four_steps :: String
  , complete :: String
  , how_to_upload :: String
  , take_clear_picture_dl :: String
  , ensure_adequate_light :: String
  , fit_dl_correctly :: String
  , take_photo :: String
  , fit_rc_correctly :: String
  , take_clear_picture_rc :: String
  , dl_uploaded :: String
  , rc_uploaded :: String
  , dl_uploading :: String
  , rc_uploading :: String
  , retake_rc :: String
  , retake_dl :: String
  , confirm_and_upload :: String
  , retake_photo :: String
  , change_city :: String
  , lets_get_you_trip_ready :: String
  , got_an_otp :: String
  , driving_license_details :: String
  , vehicle_registration_details :: String
  , upload_registration_certificate_str :: String
  , upload_photo :: String
  , clear_image :: String
  , blurry_image :: String
  , cropped_correctly :: String
  , wrong_cropping :: String
  , change_location :: String
  , rc_verification_in_progress :: String
  , rc_verification_failed_status :: String
  , rc_verification_success :: String
  , rc_in_progress_desc :: String
  , rc_failed_desc :: String
  , take_a_photo :: String
  , gallery :: String
  , unable_to_detect_your_location :: String
  , detecting_location :: String
  , get_full_payment :: String
  , select_city_str :: String
  , we_are_not_live_in_your_area :: String
  , location_unserviceable :: String
  , unable_to_get_your_location :: String
  , turn_off_any_mock_location_app_and_restart :: String
  , this_extra_amount_the_customer_will_pay :: String
  , ten_digit_mobile_number :: String
  , booth_charges :: String
  , booth_charges_included :: String
  , total_amount :: String
  , please_add_rc :: String
  , location_cannot_be_added_while_on_ride :: String
  , location_cannot_be_added_while_goto_active :: String
  , add_goto :: String
  , no_open_market_rides :: String -> String
  , account_blocked :: String
  , you_have_been_blocked_from_taking_rides :: String
  , dismiss :: String
  , earnings :: String
  , yatri_points :: String
  , discount_points :: String
  , yatri_points_str :: String
  , introducing_yatri_points :: String
  , now_earn_points_for_every_ride_and_referral_and_use_them_to_get_rewards :: String -> String
  , paid_by_yatri_points :: String
  , discount_points_small :: String
  , yatri_points_usage_popup :: String -> String
  , yatri_points_usage_secondary :: String -> String -> String
  , use_points_now :: String
  , buy_now :: String
  , select_date :: String
  , what_will_my_points_be_converted_to :: String
  , points_expiring_in_the_next :: String
  , days_use_them_before_they_expire :: String
  , points_expiring :: String
  , no_points_available :: String
  , failed_to_use_points_please_try_again_later :: String
  , bad_rating_by_customer :: String
  , good_rating_by_customer :: String
  , ride_cancellation :: String
  , customer_referral :: String
  , customer_should_complete_a_valid_ride :: String
  , driver_referral :: String
  , purple_ride_completed :: String
  , training_complted :: String
  , rides_in_a_day :: String
  , top :: String
  , in_weekly_leaderboard :: String
  , trip_earnings :: String
  , extra_earnings :: String
  , trips :: String
  , view_more :: String
  , minimum :: String
  , points_is_required_for_conversion :: String
  , discount :: String
  , check_now :: String
  , yatri_points_faqs :: String
  , learn_about_yatri_points :: String
  , yatri_points_faqs_ques1 :: String -> String
  , yatri_points_faqs_ques1_ans1 :: String -> String
  , yatri_points_faqs_ques1_ans2 :: String -> String
  , yatri_points_faqs_ques1_ans3 :: String -> String
  , yatri_points_faqs_ques2 :: String -> String
  , yatri_points_faqs_ques2_ans1 :: String -> String
  , yatri_points_faqs_ques2_ans2 :: String -> String
  , yatri_points_faqs_ques3 :: String -> String
  , yatri_points_faqs_ques3_ans1 :: String -> String
  , yatri_points_faqs_ques3_ans2 :: String -> String
  , yatri_points_faqs_ques4 :: String -> String
  , yatri_points_faqs_ques4_ans1 :: String -> String
  , yatri_points_faqs_ques4_ans2 :: String -> String
  , yatri_points_faqs_ques4_ans3 :: String -> String
  , yatri_points_faqs_ques5 :: String -> String
  , yatri_points_faqs_ques5_ans1 :: String -> String
  , yatri_points_faqs_ques5_ans2 :: String -> String
  , yatri_points_faqs_ques6 :: String -> String
  , yatri_points_faqs_ques6_ans1 :: String -> String
  , task_completed :: String
  , rides_in_a_day_prefix :: String
  , rides_in_a_day_suffix :: String
  , star_rating_for_the_trip :: String
  , one_two_start_rating :: String
  , booking_cancellation :: String
  , paid_by :: String
  , driver_referral_code :: String
  , app_qr_code :: String
  , start_taking_rides_and_refer :: String -> String
  , referred_drivers :: String
  , ride_leaderboard :: String
  , your_rank :: String
  , not_available_yet :: String
  , enter_referral_code :: String
  , have_a_referral_code :: String
  , complete_steps_to_apply_referral :: String
  , download_namma_yatri :: String -> String
  , enter_code :: String
  , complete_registration :: String
  , cant_find_option :: String
  , convert_points :: String
  , help_faq :: String
  , bonus_points :: String
  , max :: String
  , coins :: String
  , points_added :: String
  , watch_now :: String
  , choose_a_plan :: String
  , referral :: String
  , benefits :: String
  , your_daily_rank :: String
  , click_to_expand :: String
  , referred :: String
  , activated :: String
  , refer_driver :: String
  , refer_customer :: String
  , referred_drivers_info :: String -> String
  , referred_customers_info :: String -> String
  , activated_customers_info :: String
  , customer_referral_code :: String
  , accept_ride_to_enter_leaderboard :: String
  , contact_support_via :: String
  , you_can_share_screenshot :: String
  , place_a_call :: String
  , terms_and_conditions_updated :: String
  , safety_is_our_responsibility :: String
  , customer_safety_first :: String
  , lets_ensure_safe_ride :: String
  , customer_safety_our_resp_happy_ride :: String
  , our_safety_partner :: String
  , quiz :: String
  , hindi :: String
  , kannada :: String
  , tamil :: String
  , telugu :: String
  , french :: String
  , malayalam :: String
  , bengali :: String
  , english :: String
  , you_have_successfully_completed :: String -> String
  , all_answers_should_be_correct_to_complete :: String -> String
  , questions_should_be_correct_to_complete :: String
  , correct :: String
  , retake_quiz :: String
  , take_a_quiz :: String
  , play_again :: String
  , play_now :: String
  , watch_all_videos_to_learn :: String
  , play_quiz_to_complete_your_training :: String
  , training_completed :: String
  , watched :: String
  , uh_oh_something_went_wrong :: String
  , watch_all_videos_to_unlock_quiz :: String
  , incomplete :: String
  , new_c :: String
  , pending_str_c :: String
  , completed_str :: String
  , videos :: String
  , learn_and_earn :: String
  , unable_to_change_language_please_try_again :: String
  , unable_to_load_quiz_please_try_again :: String
  , we_guarantee_you :: String
  , lowest_fees_from :: String
  , zero_fee_till :: String
  , zero_commision_unlimited_rides :: String
  , we_are_currently_live_with_vehicle :: String
  , we_are_currently_live_with_vehicle_desc :: String
  , exit_the_quiz :: String
  , exit_and_start_again_later :: String
  , select_the_language_you_can_read :: String
  , check_app :: String
  , check_your_app_by_test_ride_request :: String
  , please_try_the_following_steps :: String
  , seems_like_there_is_a_problem :: String
  , did_you_receive_test_ride :: String
  , everything_is_ok :: String
  , call_our_support_team :: String
  , move_to_high_demand_area :: String
  , yes :: String
  , get_support_on_whatsapp :: String
  , know_about_points :: String
  , not_enough_points_description :: String
  , share :: String
  , share_namma_yatri :: String -> String
  , share_namma_yatri_message :: String
  , be_open_choose_open :: String
  , now :: String
  , add_vehicle :: String
  , select_your_vehicle_type :: String
  , car :: String
  , special_pickup_zone_nearby :: String
  , zone_pickup :: String
  , special_pickup_zone_ride :: String
  , special_pickup_zone :: String
  , special_pickup_zone_popup_info :: String
  , inside_special_pickup_zone_popup_info :: String
  , select_a_green_area_for_priority_rides :: String
  , priority_ride_expierence :: String
  , duration :: String
  , rental_fare :: String
  , start_time :: String
  , start_odo_reading :: String
  , ride_start :: String
  , ride_end :: String
  , ride_started_at :: String
  , ride_ended_at :: String
  , odometer_reading :: String
  , picked_up_at :: String
  , upcoming_stop :: String
  , last_stop :: String
  , previous_stop :: String
  , ride_time :: String
  , you_are_on_a_rental_ride :: String
  , you_are_on_a_intercity_ride :: String
  , enter_end_ride_otp :: String
  , you_are_not_at_stop_location :: String
  , arrived_at_stop :: String
  , enable_loc_permission_to_get_rides :: String
  , enable_loc_per_from_settings :: String
  , enable_permission_str :: String
  , capture_doc_desc_1 :: String
  , capture_doc_desc_2 :: String
  , capture_doc_desc_3 :: String
  , upload_doc :: String
  , register_your_car :: String
  , register_your_auto :: String
  , register_your_ambulance :: String
  , do_you_want_to_change_vt :: String
  , yes_change_vehicle :: String
  , change_vehicle :: String
  , vehicle_type_mismatch :: String
  , uploaded_doc_doesnt_match :: String
  , change_vehicle_type :: String
  , upload_different_rc :: String
  , profile_photo_str :: String
  , aadhaar_card_str :: String
  , pan_card_str :: String
  , vehicle_permit_str :: String
  , fitness_certificate_str :: String
  , vehicle_insurance_str :: String
  , vehicle_puc_str :: String
  , rc_mandatory :: String
  , document_uploaded_successfully :: String
  , toll_charges_including :: String -> String
  , toll_road_changed :: String
  , ride_toll_fare_includes :: String -> String
  , toll_included :: String
  , trip_time :: String
  , earnings_per_km :: String
  , optional_document :: String
  , earnings_per_km_desc_1 :: String
  , earnings_per_km_desc_2 :: String
  , earnings_p_km :: String
  , is_your_car_ac_working :: String
  , how_does_ac_condition_affect :: String
  , we_will_use_this_info :: String
  , you_can_always_change_this_from_profile :: String
  , is_your_car_ac_turned_on_and_working :: String
  , set_the_ac_on_to_enable :: String -> String
  , variants_are_switched :: String
  , non_ac_are_switched :: String
  , network_error :: String
  , unknown_error :: String
  , connection_refused :: String
  , timeout :: String
  , server_error :: String
  , all_eligible_variants_are_chosen_please_check :: String
  , ride_more_and_earn_points :: String
  , ride_more_earn_more :: String
  , take_more_rides_to_earn_more_points_and_convert_it_to_subscription_discounts :: String
  , check_yatri_points :: String
  , two_more_rides_to_go :: String
  , one_more_ride_to_go :: String
  , take_one_more_ride_to_earn_points :: String -> String
  , take_two_more_rides_to_earn_points :: String -> String
  , congratulations :: String
  , you_have_earned_points_for_completing_eight_rides :: String -> String
  , refer_namma_yatri_app_to_customers_and_earn_points :: String -> String
  , refer_now :: String
  , convert_your_points_to_discount :: String
  , convert_your_points_to_get_discount_on_your_subscription :: String
  , convert_now :: String
  , more_rides :: String
  , sort_by :: String
  , accept :: String
  , pass :: String
  , term_1a :: String
  , term_2a :: String
  , term_3a :: String
  , term_1b :: String -> String
  , term_2b :: String -> String
  , term_3b :: String -> String -> String
  , excluded_charges :: String
  , tolls :: String
  , state_permit :: String
  , excluded_footer :: String
  , included_charges :: String
  , inc_1 :: String
  , inc_2a :: String -> String -> String -> String
  , inc_2b :: String -> String -> String
  , pickup_drop :: String
  , toll_charges_included :: String
  , please_do_not_demand_extra :: String
  , final_fare_excludes_toll :: String
  , please_collect_separately :: String
  , toll_charges_maybe_applicable :: String
  , you_are_all_set_to_take_rides :: String -> String
  , top_ac_driver :: String -> String
  , go_to_advanced_ride :: String
  , get_advanced_ride :: String
  , advanced_ride_popup_title :: String
  , advance :: String
  , current_button_text :: String
  , advance_booking :: String
  , feature_update :: String
  , third_party_booking :: String
  , some_feature_are_not_available_with_this_provider :: String -> String
  , guaranteed_ride :: String
  , customer_calling_and_messaging :: String
  , waiting_charges :: String
  , customer_tips :: String
  , cancellation_charges :: String
  , merchant_points :: String -> String
  , merchant_name :: String -> String
  , third_party_rides :: String
  , third_party_rides_are_requested_with_by_a_users_from_another_app :: String
  , some_features_may_not_be_available :: String
  , why :: String
  , some_features_are_not_available_for_third_party_rides :: String
  , booking_from :: String -> String
  , pick_up :: String
  , rate_card :: String
  , toll_charges :: String
  , toll_charges_desc :: String
  , parking_charge :: String
  , fare_for :: String -> String
  , waiting_charge_limit :: String -> String
  , parking_charges_desc :: String
  , tip_can_be_added :: String -> String
  , day_time_charges :: String -> String -> String
  , congestion_charges_desc :: String -> String
  , toll_or_parking_charges :: String
  , toll_charges_estimated :: String
  , congestion_charges :: String
  , pickup_charge :: String
  , night_time_charges :: String -> String -> String
  , min_fare_upto :: String -> String
  , more_than :: String
  , rate_above_min_fare :: String
  , driver_pickup_charges :: String -> String
  , daytime_charges_applicable_at_night :: String -> String -> String
  , daytime_charges_applied_at_night :: String -> String -> String -> String
  , total_fare_may_change_due_to_change_in_route :: String
  , driver_additions :: String
  , fare_update_policy :: String
  , driver_additions_optional :: String
  , the_driver_may_quote_extra_to_cover_for_traffic :: String
  , driver_may_not_charge_this_additional_fare :: String
  , highest_earning_peak_time :: String
  , choose_ride_dist :: String
  , rates_change_as_the_dist :: String
  , view_booking_pref :: String
  , limited_time_offer_until :: String -> String
  , register_your_bike :: String
  , bike_taxi :: String
  , select_facilities :: String
  , first_aid_kit :: String
  , driver_acknowledge :: String
  , booking_preference :: String
  , inspection :: String
  , a_f :: String
  , by_proceeding_you_accept_full_responsibility :: String
  , a_c :: String
  , ambulance :: String
  , non_ac :: String
  , ac :: String
  , no_oxygen :: String
  , oxygen :: String
  , ventilator :: String
  , select_one :: String
  , first_ride_free :: String -> String
  , first_rides_free :: String -> String
  , additional_charges_will_be_applicable :: String
  , referral_first_ride_description :: String -> String
  , steps :: String
  , customer_completed_first_ride :: String
  , verifying :: String
  , processing :: String
  , payment_credited :: String
  , referral_bonus :: String
  , referral_bonus_tracker :: String
  , upi_details :: String
  , customer_referral_tracker :: String
  , pay_to_add_upi :: String -> String
  , add_upi_to_receive_reward :: String
  , earn_for_each_referral :: String -> String
  , start_referring_now :: String
  , will_get_referral_to_upi_id :: String
  , delete_upi_id :: String
  , confirm_delete_upi_id :: String -> String
  , payout_history :: String
  , how_to_earn :: String
  , received :: String -> String
  , credited_on :: String -> String
  , referral_bonus_earned :: String
  , no_activated_referral :: String
  , no_active_referral_on_date :: String
  , payment_in_progress :: String
  , refresh_payment :: String
  , by :: String
  , customers :: String
  , customer :: String
  , rating :: String
  , cancellation :: String
  , i_speak :: String
  , with_nammayatri_for :: String -> String
  , years :: String
  , vehicle_number :: String
  , what_people_say :: String
  , star_rating :: String
  , card_texts :: String
  , trainings_i_completed :: String
  , i_pledge :: String
  , only_5_more_rides_for_n_points :: String -> String
  , only_3_more_rides_for_n_points :: String -> String
  , only_4_more_rides_for_n_points :: String -> String
  , you_got_n_points :: String -> String
  , discounted :: String
  , yatri_points_faqs_ques1_ans4 :: String -> String
  , yatri_points_tnc :: String
  , yatri_points_faqs_ques2_ans3 :: String
  , hotspots :: String
  , very_high :: String
  , high :: String
  , very_high_demand_area :: String
  , high_demand_area :: String
  , moderate :: String
  , average_demand_area :: String
  , this_area_is_experiencing_average_searches :: String
  , this_area_is_experiencing_very_high_searches :: String
  , this_area_is_experiencing_high_searches :: String
  , navigate :: String
  , hotspots_not_available_currently :: String
  , gst_with_percentage :: String -> String
  , discount_points_upto :: String -> String
  , cannot_detect_pan_card :: String
  , cannot_detect_aadhaar :: String
  , document_already_validated :: String
  , document_under_manual_review :: String
  , document_already_linked_to_another_driver :: String
  , pan_already_linked :: String
  , exited_by_user :: String
  , app_update :: String
  , app_update_message :: String
  , aadhaar_front_not_detected :: String
  , aadhaar_back_not_detected :: String
  , unable_to_extract_name :: String
  , unable_to_extract_dob :: String
  , unable_to_extract_id :: String
  , image_b_w :: String
  , partial_doc_detected :: String
  , doc_is_blurred :: String
  , face_match_failed :: String
  , pan_not_detected :: String
  , unable_to_verify_selfie :: String
  , blurred_selfie :: String
  , eyes_closed_selfie :: String
  , multiple_faces_in_selfie :: String
  , face_blocked :: String
  , remove_eyewere :: String
  , image_validation_exceed_limit :: String
  , parking_charges_included :: String -> String
  , invoice_generated_from_driver_to_rider :: String
  , included :: String
  , db_check_and_name_match_failed :: String
  , complete_your_profile :: String
  , add_photos :: String
  , add_upto_four :: String
  , card_text :: String
  , pledge :: String
  , safe_journey :: String
  , clean_car :: String
  , on_time_pick_up :: String
  , maintenance :: String
  , vehicle_offer :: String
  , gas :: String
  , radio :: String
  , eco_friendly :: String
  , device_charging :: String
  , boot_space :: String
  , pet_friendly :: String
  , hometown :: String
  , why_ny :: String -> String
  , cab :: String 
  , new_home :: String
  , kid_education :: String
  , new_vehicle :: String
  , add_your_photos :: String
  , add_photo_caption :: String
  , complete_profile :: String
  , complete_profile_msg :: String
  , edit_profile :: String
  , save :: String
  , manage_vehicle :: String
  , is_not_supported_yet :: String
  , we_will_nofity_you_when_it_is_available :: String -> String
  , add_upi_to_receive_referral_reward :: String
  , do_you_want_to_receive_amount_here :: String
  , yes_pay_to_this_account :: String
  , i_will_add_different_account :: String
  , add_now :: String
  , recording_audio :: String
  , recorded_audio :: String
  , share_with_safety_team :: String
  , record_audio :: String
  , cannot_enable_go_home_for_different_city :: String
  , ride_cancellation_rate :: String
  , cancellation_rate_trivia :: String
  , high_cancellation_rate :: String
  , last_n_days :: String -> String
  , cancellation_rate_trivia_2 :: String
  , lifetime_stats :: String
  , total_rides_cancelled :: String
  , rental_ride :: String
  , total_earnings_missed :: String
  , more_about_me :: String
  , driving_since :: String
  , error_occured_try_again :: String
  , there_might_be_multiple_stops_in_this_rental_ride :: String -> String
  , rental_ride_accepted :: String
  , my_referral_bonus :: String
  , add_upi_id :: String
  , linked_upi_id :: String
  , to_get_money :: String
  , till :: String -> String
  , referral_bonus_will_be_credited_to_bank :: String
  , expert_driving :: String
  , clean_vehicle :: String
  , skilled_navigator :: String
  , safe_ride :: String
  , polite_driver :: String
  , on_time :: String
  , ac_not_turned_on :: String
  , late_pick_up_arrival :: String
  , asked_for_more_fare :: String
  , unhygienic_vehicle :: String
  , rash_driving :: String
  , rude_driver :: String
  , training :: String
  , financial :: String
  , safety :: String
  , kids_education :: String
  , buy_new_vehicle :: String
  , not_available :: String
  , please_write_something :: String
  , buy_new_home :: String
  , favourites :: String
  , points_earned_ :: String -> String
  , for_metro_pickup_ride :: String
  , for_metro_drop_ride :: String
  , continue_with :: String -> String
  , contact_support_for_help :: String
  , you_have_switched_city_or_vehicle :: String
  , xl_plus :: String
  , ride_requests :: String
  , scheduled_ride_accepted :: String
  , you_can_access_scheduled_rides :: String
  , from_your_homescreen :: String
  , currently_there_are_no_rides_available :: String
  , due_to_higher_cancellation_rate_you_are_blocked :: String
  , blocked_till :: String -> String -> String
  , cancel_booking :: String
  , go_to_pickup :: String
  , ride_scheduled :: String
  , please_be_online :: String -> String
  , before_the_ride_starts :: String
  , trip_will_be_assigned_to_another_driver :: String
  , ride_summary :: String
  , ride_assigned_to_another_driver :: String
  , you_can_see_other_available_ride_in_more_rides_section :: String
  , round_trip :: String
  , upcoming :: String
  , follow_instructions_to_avoid_reassignment_of_ride :: String
  , be_within_10km_of_pickup :: String
  , please_collect_parking_charges :: String
  , incurred_during_trip :: String
  , back :: String
  , your_ride_starts_in :: String
  , away :: String
  , intercity :: String
  , local :: String
  , intercity_return :: String
  , rental :: String
  , regular :: String
  , upcoming_ride :: String
  , all :: String
  , tomorrow :: String
  , we_are_not_able_to_fetch_your_current_location :: String
  , you_have_an_upcoming :: String
  , booking :: String
  , intercity_ride_accepted :: String
  , intercity_ride :: String
  , please_ensure_that_your_vehicle_is_ready_for_intercity_trip :: String
  , per_km_charge :: String
  , extra_time_charge :: String
  , added_at_end_of_trip :: String
  , driver_allowance :: String
  , add_on_km_charge :: String
  , extra_distance_charges :: String
  , base_charge :: String -> String
  , the_customer_will_pay_post_scheduled_ride_start_time :: String -> String
  , clean_auto :: String
  , clean_cab :: String
  , metro_ride_completed :: String
  , or_ride_is_cancelled_by_customer :: String
  , the_ride_starts :: String
  , good_services :: String
  , smooth_driving :: String
  , no_cancellation :: String
  , free_trial_ending_in_n_days :: String -> String
  , n_free_rides_completed :: String -> String
  , n_more_free_rides_left :: String -> String
  , collect_cash_at_drop :: String
  , more_details :: String
  , take_photo_of_parcel :: String
  , sender_will_verify_parcel :: String
  , call_customer_text :: String
  , call_sender :: String
  , call_receiver :: String
  , start :: String
  , end :: String
  , delivery_bike_service_tier_desc :: String
  , rate_your_delivery_with :: String
  , delivery_details :: String
  , take_clear_picture_parcel :: String
  , ensure_adequate_light_parcel_desc :: String
  , fit_parcel_correctly :: String
  , correct_positioning ::  String
  , incorrect_positioning :: String
  , upload_parcel_image :: String
  , pickup_instruction :: String
  , drop_instruction :: String
  , parcel_is_inappropriate :: String
  , sender_asking_different_location :: String
  , sender_unavailable_unreachable :: String
  , truck :: String
  , register_your_truck :: String
  , no_plan_selected :: String
  , a_new_way_to_earn_parcel :: String
  , seamless_earning_experience_click_below :: String
  , metro_warrior_mode :: String
  , choose_metro_station :: String
  , primary_metro_station :: String
  , primary_station_info :: String
  , nearby_stations :: String
  , nearby_station_info :: String
  , change :: String
  , disable_metro_warriors_info :: String
  , choose_preferred_metro :: String
  , metro_warriors :: String
  , search :: String
  , bus__ :: String 
  , driver_unsubscribed :: String
  , canceling_this_booking_may_affect_the_emergency_medical :: String
  , drivers_are_permitted_to_cancel_ambulance_bookings :: String
  , payment_under_maintenance :: String
  , payments_temporarily_unavailable :: String
  , resume_ride :: String
  , end_ride_with_stops :: String
  , stop :: String -> String 
  }

derive instance ntL :: Newtype Languages _

derive instance ntK :: Newtype Keymap _

a :: forall a. Proxy a
a = Proxy

odiya :: Proxy "odiya"
odiya = a

hindi :: Proxy "hindi"
hindi = a

english :: Proxy "english"
english = a

malayalam :: Proxy "malayalam"
malayalam = a

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

points_balance :: Proxy "points_balance"
points_balance = a

total_earned :: Proxy "total_earned"
total_earned = a

ride_history :: Proxy "ride_history"
ride_history = a

transaction_history :: Proxy "transaction_history"
transaction_history = a

points_earned :: Proxy "points_earned"
points_earned = a

no_rides :: Proxy "no_rides"
no_rides = a

points_used :: Proxy "points_used"
points_used = a

use_points :: Proxy "use_points"
use_points = a

insights :: Proxy "insights"
insights = a

usage_history :: Proxy "usage_history"
usage_history = a

no_points_earned :: Proxy "no_points_earned"
no_points_earned = a

no_points_used :: Proxy "no_points_used"
no_points_used = a

earn_points_by_taking_rides_and_referring_the_app_to_others :: Proxy "earn_points_by_taking_rides_and_referring_the_app_to_others"
earn_points_by_taking_rides_and_referring_the_app_to_others = a

use_them_before_they_expire :: Proxy "use_them_before_they_expire"
use_them_before_they_expire = a

no_ride_history_available :: Proxy "no_ride_history_available"
no_ride_history_available = a

you_have_not_completed_a_ride_yet :: Proxy "you_have_not_completed_a_ride_yet"
you_have_not_completed_a_ride_yet = a

complete_first_ride_to_unlock_points :: Proxy "complete_first_ride_to_unlock_points"
complete_first_ride_to_unlock_points = a

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

using_points_requires_an_active_plan :: Proxy "using_points_requires_an_active_plan"
using_points_requires_an_active_plan = a

to_get_started :: Proxy "to_get_started"
to_get_started = a

converted_from_points :: Proxy "converted_from_points"
converted_from_points = a

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

of_ :: Proxy "of_"
of_ = a

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

frequent_cancellations_will_lead_to_blocking :: Proxy "frequent_cancellations_will_lead_to_blocking"
frequent_cancellations_will_lead_to_blocking = a

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

ac_check_titile :: Proxy "ac_check_titile"
ac_check_titile = a

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

lost_and_found :: Proxy "lost_and_found"
lost_and_found = a

report_lost_item :: Proxy "report_lost_item"
report_lost_item = a

corporate_address :: Proxy "corporate_address"
corporate_address = a

corporate_address_description :: Proxy "corporate_address_description"
corporate_address_description = a

corporate_address_description_additional :: Proxy "corporate_address_description_additional"
corporate_address_description_additional = a

registered_address :: Proxy "registered_address"
registered_address = a

registered_address_description :: Proxy "registered_address_description"
registered_address_description = a

registered_address_description_additional :: Proxy "registered_address_description_additional"
registered_address_description_additional = a

select_the_languages_you_can_speak :: Proxy "select_the_languages_you_can_speak"
select_the_languages_you_can_speak = a

gender :: Proxy "gender"
gender = a

select_your_gender :: Proxy "select_your_gender"
select_your_gender = a

male :: Proxy "male"
male = a

female :: Proxy "female"
female = a

prefer_not_to_say :: Proxy "prefer_not_to_say"
prefer_not_to_say = a

set_now :: Proxy "set_now"
set_now = a

complete_your_profile_and_find_more_rides :: Proxy "complete_your_profile_and_find_more_rides"
complete_your_profile_and_find_more_rides = a

update_now :: Proxy "update_now"
update_now = a

confirm :: Proxy "confirm"
confirm = a

gender_updated :: Proxy "gender_updated"
gender_updated = a

zone_cancel_text_drop :: Proxy "zone_cancel_text_drop"
zone_cancel_text_drop = a

zone_cancel_text_pickup :: Proxy "zone_cancel_text_pickup"
zone_cancel_text_pickup = a

rankings :: Proxy "rankings"
rankings = a

getting_the_leaderboard_ready :: Proxy "getting_the_leaderboard_ready"
getting_the_leaderboard_ready = a

please_wait_while_we_update_the_details :: Proxy "please_wait_while_we_update_the_details"
please_wait_while_we_update_the_details = a

last_updated :: Proxy "last_updated"
last_updated = a

congratulations_you_are_rank :: Proxy "congratulations_you_are_rank"
congratulations_you_are_rank = a

you :: Proxy "you"
you = a

daily :: Proxy "daily"
daily = a

inaccurate_date_and_time :: Proxy "inaccurate_date_and_time"
inaccurate_date_and_time = a

adjust_your_device_date_and_time_and_try_again :: Proxy "adjust_your_device_date_and_time_and_try_again"
adjust_your_device_date_and_time_and_try_again = a

the_current_date_and_time_is :: Proxy "the_current_date_and_time_is"
the_current_date_and_time_is = a

go_to_setting :: Proxy "go_to_setting"
go_to_setting = a

accept_rides_to_enter_rankings :: Proxy "accept_rides_to_enter_rankings"
accept_rides_to_enter_rankings = a

otp_has_been_resent :: Proxy "otp_has_been_resent"
otp_has_been_resent = a

otp_entering_limit_exhausted_please_try_resending_otp :: Proxy "otp_entering_limit_exhausted_please_try_resending_otp"
otp_entering_limit_exhausted_please_try_resending_otp = a

otp_resent_limit_exhausted_please_try_again_later :: Proxy "otp_resent_limit_exhausted_please_try_again_later"
otp_resent_limit_exhausted_please_try_again_later = a

otp_page_has_been_expired_please_request_otp_again :: Proxy "otp_page_has_been_expired_please_request_otp_again"
otp_page_has_been_expired_please_request_otp_again = a

something_went_wrong_please_try_again :: Proxy "something_went_wrong_please_try_again"
something_went_wrong_please_try_again = a

invalid_referral_code :: Proxy "invalid_referral_code"
invalid_referral_code = a

issue_removed_successfully :: Proxy "issue_removed_successfully"
issue_removed_successfully = a

otp_entering_limit_exhausted_please_try_again_later :: Proxy "otp_entering_limit_exhausted_please_try_again_later"
otp_entering_limit_exhausted_please_try_again_later = a

too_many_attempts_please_try_again_later :: Proxy "too_many_attempts_please_try_again_later"
too_many_attempts_please_try_again_later = a

invalid_referral_number :: Proxy "invalid_referral_number"
invalid_referral_number = a

something_went_wrong_try_again_later :: Proxy "something_went_wrong_try_again_later"
something_went_wrong_try_again_later = a

wait_time :: Proxy "wait_time"
wait_time = a

wait_timer :: Proxy "wait_timer"
wait_timer = a

how_long_waited_for_pickup :: Proxy "how_long_waited_for_pickup"
how_long_waited_for_pickup = a

customer_will_pay_for_every_minute :: Proxy "customer_will_pay_for_every_minute"
customer_will_pay_for_every_minute = a

others :: Proxy "others"
others = a

enter_second_sim_number :: Proxy "enter_second_sim_number"
enter_second_sim_number = a

alternate_number :: Proxy "alternate_number"
alternate_number = a

limit_exceeded_for_alternate_number :: Proxy "limit_exceeded_for_alternate_number"
limit_exceeded_for_alternate_number = a

add_alternate_number_in_meantime :: Proxy "add_alternate_number_in_meantime"
add_alternate_number_in_meantime = a

otp_resend_limit_exceeded :: Proxy "otp_resend_limit_exceeded"
otp_resend_limit_exceeded = a

alternate_number_cannot_be_added :: Proxy "alternate_number_cannot_be_added"
alternate_number_cannot_be_added = a

otp_resent :: Proxy "otp_resent"
otp_resent = a

sedan :: Proxy "sedan"
sedan = a

suv :: Proxy "suv"
suv = a

hatchback :: Proxy "hatchback"
hatchback = a

auto_rickshaw :: Proxy "auto_rickshaw"
auto_rickshaw = a

taxi :: Proxy "taxi"
taxi = a

taxi_plus :: Proxy "taxi_plus"
taxi_plus = a

my_profile :: Proxy "my_profile"
my_profile = a

settings :: Proxy "settings"
settings = a

reg_number :: Proxy "reg_number"
reg_number = a

type_ :: Proxy "type_"
type_ = a

model_name :: Proxy "model_name"
model_name = a

colour :: Proxy "colour"
colour = a

badges :: Proxy "badges"
badges = a

edit_rc :: Proxy "edit_rc"
edit_rc = a

deactivate_rc :: Proxy "deactivate_rc"
deactivate_rc = a

activate_rc :: Proxy "activate_rc"
activate_rc = a

delete_rc :: Proxy "delete_rc"
delete_rc = a

call_driver :: Proxy "call_driver"
call_driver = a

call_customer_support :: Proxy "call_customer_support"
call_customer_support = a

active_rc_on_another_driver :: Proxy "active_rc_on_another_driver"
active_rc_on_another_driver = a

call_driver_or_contact_support :: Proxy "call_driver_or_contact_support"
call_driver_or_contact_support = a

skip :: Proxy "skip"
skip = a

active_str :: Proxy "active_str"
active_str = a

inactive_rc :: Proxy "inactive_rc"
inactive_rc = a

confirmation_for_deleting_rc :: Proxy "confirmation_for_deleting_rc"
confirmation_for_deleting_rc = a

yes_delete :: Proxy "yes_delete"
yes_delete = a

add_new_rc :: Proxy "add_new_rc"
add_new_rc = a

connect_call_anonymously :: Proxy "connect_call_anonymously"
connect_call_anonymously = a

yes_activate :: Proxy "yes_activate"
yes_activate = a

yes_deactivate :: Proxy "yes_deactivate"
yes_deactivate = a

confirmation_for_deactivating_rc :: Proxy "confirmation_for_deactivating_rc"
confirmation_for_deactivating_rc = a

confirmation_for_activating_rc :: Proxy "confirmation_for_activating_rc"
confirmation_for_activating_rc = a

this_will_deactivate_currently_active_rc :: Proxy "this_will_deactivate_currently_active_rc"
this_will_deactivate_currently_active_rc = a

removed :: Proxy "removed"
removed = a

deactivated :: Proxy "deactivated"
deactivated = a

ride_type_select :: Proxy "ride_type_select"
ride_type_select = a

deactivate :: Proxy "deactivate"
deactivate = a

vehicles_pending :: Proxy "vehicles_pending"
vehicles_pending = a

is_active_now :: Proxy "is_active_now"
is_active_now = a

single_rc_cannot_be_deleted :: Proxy "single_rc_cannot_be_deleted"
single_rc_cannot_be_deleted = a

cancellation_rate :: Proxy "cancellation_rate"
cancellation_rate = a

rides_cancelled :: Proxy "rides_cancelled"
rides_cancelled = a

earnings_missed :: Proxy "earnings_missed"
earnings_missed = a

summary :: Proxy "summary"
summary = a

namma_bonus :: Proxy "namma_bonus"
namma_bonus = a

trips_completed :: Proxy "trips_completed"
trips_completed = a

late_night_trips :: Proxy "late_night_trips"
late_night_trips = a

about_me :: Proxy "about_me"
about_me = a

about_vehicle :: Proxy "about_vehicle"
about_vehicle = a

add :: Proxy "add"
add = a

years_old :: Proxy "years_old"
years_old = a

from_where :: Proxy "from_where"
from_where = a

missed_opportunity :: Proxy "missed_opportunity"
missed_opportunity = a

earned_on_app :: Proxy "earned_on_app"
earned_on_app = a

travelled_on_app :: Proxy "travelled_on_app"
travelled_on_app = a

how_old_is_your_vehicle :: Proxy "how_old_is_your_vehicle"
how_old_is_your_vehicle = a

enter_name_of_vehicle :: Proxy "enter_name_of_vehicle"
enter_name_of_vehicle = a

new_ :: Proxy "new_"
new_ = a

with :: Proxy "with"
with = a

total_money_collected :: Proxy "total_money_collected"
total_money_collected = a

fare_earned_of_the_day :: Proxy "fare_earned_of_the_day"
fare_earned_of_the_day = a

gst_plus_payable :: Proxy "gst_plus_payable"
gst_plus_payable = a

to_continue_using_yatri_sathi :: Proxy "to_continue_using_yatri_sathi"
to_continue_using_yatri_sathi = a

pay :: Proxy "pay"
pay = a

later :: Proxy "later"
later = a

great_job :: Proxy "great_job"
great_job = a

fee_breakup :: Proxy "fee_breakup"
fee_breakup = a

yatri_sathi_fee_payable_for_date :: Proxy "yatri_sathi_fee_payable_for_date"
yatri_sathi_fee_payable_for_date = a

fee_corresponding_to_the_distance :: Proxy "fee_corresponding_to_the_distance"
fee_corresponding_to_the_distance = a

platform_fee :: Proxy "platform_fee"
platform_fee = a

gst :: Proxy "gst"
gst = a

total_payable :: Proxy "total_payable"
total_payable = a

got_it :: Proxy "got_it"
got_it = a

view_details :: Proxy "view_details"
view_details = a

payment_successful :: Proxy "payment_successful"
payment_successful = a

payment_pending :: Proxy "payment_pending"
payment_pending = a

payment_failed :: Proxy "payment_failed"
payment_failed = a

payment_pending_desc :: Proxy "payment_pending_desc"
payment_pending_desc = a

payment_failed_desc :: Proxy "payment_failed_desc"
payment_failed_desc = a

we_will_notify_when_payment_success :: Proxy "we_will_notify_when_payment_success"
we_will_notify_when_payment_success = a

continue_taking_rides :: Proxy "continue_taking_rides"
continue_taking_rides = a

your_previous_payment_is_pending :: Proxy "your_previous_payment_is_pending"
your_previous_payment_is_pending = a

goverment_charges :: Proxy "goverment_charges"
goverment_charges = a

today :: Proxy "today"
today = a

okay :: Proxy "okay"
okay = a

no_payment_history_available :: Proxy "no_payment_history_available"
no_payment_history_available = a

you_dont_have_any_payments :: Proxy "you_dont_have_any_payments"
you_dont_have_any_payments = a

enter_aadhaar_number :: Proxy "enter_aadhaar_number"
enter_aadhaar_number = a

enter_aadhaar_details :: Proxy "enter_aadhaar_details"
enter_aadhaar_details = a

enter_aadhaar_otp_ :: Proxy "enter_aadhaar_otp_"
enter_aadhaar_otp_ = a

aadhaar_linking_required :: Proxy "aadhaar_linking_required"
aadhaar_linking_required = a

customer_added_a_stop :: Proxy "customer_added_a_stop"
customer_added_a_stop = a

aadhaar_linking_required_description :: Proxy "aadhaar_linking_required_description"
aadhaar_linking_required_description = a

by_clicking_this_you_will_be_agreeing_to_our_tc :: Proxy "by_clicking_this_you_will_be_agreeing_to_our_tc"
by_clicking_this_you_will_be_agreeing_to_our_tc = a

terms_and_conditions_short :: Proxy "terms_and_conditions_short"
terms_and_conditions_short = a

otp_sent_to_aadhaar_number :: Proxy "otp_sent_to_aadhaar_number"
otp_sent_to_aadhaar_number = a

enter_six_digit_otp :: Proxy "enter_six_digit_otp"
enter_six_digit_otp = a

tc_tail :: Proxy "tc_tail"
tc_tail = a

link_aadhaar_id :: Proxy "link_aadhaar_id"
link_aadhaar_id = a

navigate_to_location :: Proxy "navigate_to_location"
navigate_to_location = a

no_mobile_number_registered :: Proxy "no_mobile_number_registered"
no_mobile_number_registered = a

exceed_otp_generation_limit :: Proxy "exceed_otp_generation_limit"
exceed_otp_generation_limit = a

aadhaar_number_not_exist :: Proxy "aadhaar_number_not_exist"
aadhaar_number_not_exist = a

invalid_otp :: Proxy "invalid_otp"
invalid_otp = a

no_share_code :: Proxy "no_share_code"
no_share_code = a

wrong_share_code :: Proxy "wrong_share_code"
wrong_share_code = a

invalid_share_code :: Proxy "invalid_share_code"
invalid_share_code = a

session_expired :: Proxy "session_expired"
session_expired = a

otp_attempt_exceeded :: Proxy "otp_attempt_exceeded"
otp_attempt_exceeded = a

upstream_internal_server_error :: Proxy "upstream_internal_server_error"
upstream_internal_server_error = a

transaction_already_completed :: Proxy "transaction_already_completed"
transaction_already_completed = a

goto_your_nearest_booth :: Proxy "goto_your_nearest_booth"
goto_your_nearest_booth = a

aadhaar_already_linked :: Proxy "aadhaar_already_linked"
aadhaar_already_linked = a

optional :: Proxy "optional"
optional = a

download_statement :: Proxy "download_statement"
download_statement = a

select_a_date_range :: Proxy "select_a_date_range"
select_a_date_range = a

fee_payment_history :: Proxy "fee_payment_history"
fee_payment_history = a

languages_spoken :: Proxy "languages_spoken"
languages_spoken = a

view_payment_history :: Proxy "view_payment_history"
view_payment_history = a

ride_type :: Proxy "ride_type"
ride_type = a

rc_status :: Proxy "rc_status"
rc_status = a

rated_by_users1 :: Proxy "rated_by_users1"
rated_by_users1 = a

rated_by_users2 :: Proxy "rated_by_users2"
rated_by_users2 = a

months :: Proxy "months"
months = a

rc_added_successfully :: Proxy "rc_added_successfully"
rc_added_successfully = a

call_request_has_been_placed :: Proxy "call_request_has_been_placed"
call_request_has_been_placed = a

trip_date :: Proxy "trip_date"
trip_date = a

offer_applied :: Proxy "offer_applied"
offer_applied = a

your_earnings :: Proxy "your_earnings"
your_earnings = a

number_of_rides :: Proxy "number_of_rides"
number_of_rides = a

fare_breakup :: Proxy "fare_breakup"
fare_breakup = a

my_plan :: Proxy "my_plan"
my_plan = a

your_dues :: Proxy "your_dues"
your_dues = a

your_dues_description :: Proxy "your_dues_description"
your_dues_description = a

your_dues_description_manual :: Proxy "your_dues_description_manual"
your_dues_description_manual = a

current_dues :: Proxy "current_dues"
current_dues = a

your_limit :: Proxy "your_limit"
your_limit = a

due_details :: Proxy "due_details"
due_details = a

amount :: Proxy "amount"
amount = a

view_due_details :: Proxy "view_due_details"
view_due_details = a

setup_autopay :: Proxy "setup_autopay"
setup_autopay = a

current_plan :: Proxy "current_plan"
current_plan = a

alternate_plan :: Proxy "alternate_plan"
alternate_plan = a

autopay_details :: Proxy "autopay_details"
autopay_details = a

cancel_autopay_str :: Proxy "cancel_autopay_str"
cancel_autopay_str = a

we_might_be_lost :: Proxy "we_might_be_lost"
we_might_be_lost = a

exeperiencing_error :: Proxy "exeperiencing_error"
exeperiencing_error = a

enjoy_these_benefits :: Proxy "enjoy_these_benefits"
enjoy_these_benefits = a

choose_your_plan :: Proxy "choose_your_plan"
choose_your_plan = a

skip_for_now :: Proxy "skip_for_now"
skip_for_now = a

n_day_free_trial_activated :: Proxy "n_day_free_trial_activated"
n_day_free_trial_activated = a

take_n_rides_for_the_next_n_days :: Proxy "take_n_rides_for_the_next_n_days"
take_n_rides_for_the_next_n_days = a

every_ride_at_zero_commission :: Proxy "every_ride_at_zero_commission"
every_ride_at_zero_commission = a

earn_upto_per_day :: Proxy "earn_upto_per_day"
earn_upto_per_day = a

how_this_works :: Proxy "how_this_works"
how_this_works = a

sign_up_for_autopay_by_paying_just :: Proxy "sign_up_for_autopay_by_paying_just"
sign_up_for_autopay_by_paying_just = a

get_reminded_about_your_plan_setup :: Proxy "get_reminded_about_your_plan_setup"
get_reminded_about_your_plan_setup = a

free_trial_reminder_n_days_m_rides :: Proxy "free_trial_reminder_n_days_m_rides"
free_trial_reminder_n_days_m_rides = a

plan_starts_n_days_m_rides :: Proxy "plan_starts_n_days_m_rides"
plan_starts_n_days_m_rides = a

easy_automatic_payments_start :: Proxy "easy_automatic_payments_start"
easy_automatic_payments_start = a

free_until :: Proxy "free_until"
free_until = a

per_ride :: Proxy "per_ride"
per_ride = a

per_day :: Proxy "per_day"
per_day = a

offer :: Proxy "offer"
offer = a

offers :: Proxy "offers"
offers = a

you_are_on_the_free_trial :: Proxy "you_are_on_the_free_trial"
you_are_on_the_free_trial = a

setup_autopay_before_the_trail_period_expires :: Proxy "setup_autopay_before_the_trail_period_expires"
setup_autopay_before_the_trail_period_expires = a

get_free_trail_until :: Proxy "get_free_trail_until"
get_free_trail_until = a

clear_dues :: Proxy "clear_dues"
clear_dues = a

payment_pending_alert :: Proxy "payment_pending_alert"
payment_pending_alert = a

payment_pending_alert_desc :: Proxy "payment_pending_alert_desc"
payment_pending_alert_desc = a

low_account_balance :: Proxy "low_account_balance"
low_account_balance = a

low_account_balance_desc :: Proxy "low_account_balance_desc"
low_account_balance_desc = a

okay_got_it :: Proxy "okay_got_it"
okay_got_it = a

limited_time_offer :: Proxy "limited_time_offer"
limited_time_offer = a

join_now :: Proxy "join_now"
join_now = a

automatic_payments_will_appear_here :: Proxy "automatic_payments_will_appear_here"
automatic_payments_will_appear_here = a

manual_payments_will_appear_here :: Proxy "manual_payments_will_appear_here"
manual_payments_will_appear_here = a

manual_payments :: Proxy "manual_payments"
manual_payments = a

no_automatic_payments_desc :: Proxy "no_automatic_payments_desc"
no_automatic_payments_desc = a

no_manual_payments_desc :: Proxy "no_manual_payments_desc"
no_manual_payments_desc = a

payment_history :: Proxy "payment_history"
payment_history = a

plan :: Proxy "plan"
plan = a

day :: Proxy "day"
day = a

tap_a_plan_to_view_details :: Proxy "tap_a_plan_to_view_details"
tap_a_plan_to_view_details = a

plans :: Proxy "plans"
plans = a

how_it_works :: Proxy "how_it_works"
how_it_works = a

zero_commision :: Proxy "zero_commision"
zero_commision = a

earn_today_pay_tomorrow :: Proxy "earn_today_pay_tomorrow"
earn_today_pay_tomorrow = a

pay_only_if_you_take_rides :: Proxy "pay_only_if_you_take_rides"
pay_only_if_you_take_rides = a

manage_plan :: Proxy "manage_plan"
manage_plan = a

view_autopay_details :: Proxy "view_autopay_details"
view_autopay_details = a

switch_and_save :: Proxy "switch_and_save"
switch_and_save = a

switch_and_save_desc :: Proxy "switch_and_save_desc"
switch_and_save_desc = a

switch_now :: Proxy "switch_now"
switch_now = a

payment_mode_changed_to_manual :: Proxy "payment_mode_changed_to_manual"
payment_mode_changed_to_manual = a

payment_mode_changed_to_manual_desc :: Proxy "payment_mode_changed_to_manual_desc"
payment_mode_changed_to_manual_desc = a

autopay_payments :: Proxy "autopay_payments"
autopay_payments = a

success :: Proxy "success"
success = a

transaction_on :: Proxy "transaction_on"
transaction_on = a

debited_on :: Proxy "debited_on"
debited_on = a

rides_taken_on :: Proxy "rides_taken_on"
rides_taken_on = a

join_plan :: Proxy "join_plan"
join_plan = a

join_nammaa_yatri :: Proxy "join_nammaa_yatri"
join_nammaa_yatri = a

cancel_autopay_and_pay_manually :: Proxy "cancel_autopay_and_pay_manually"
cancel_autopay_and_pay_manually = a

plan_activated_successfully :: Proxy "plan_activated_successfully"
plan_activated_successfully = a

dues_cleared_successfully :: Proxy "dues_cleared_successfully"
dues_cleared_successfully = a

not_planning_to_take_rides :: Proxy "not_planning_to_take_rides"
not_planning_to_take_rides = a

retry_payment_str :: Proxy "retry_payment_str"
retry_payment_str = a

pause_autopay_str :: Proxy "pause_autopay_str"
pause_autopay_str = a

setup_autopay_str :: Proxy "setup_autopay_str"
setup_autopay_str = a

view_ride_details :: Proxy "view_ride_details"
view_ride_details = a

account :: Proxy "account"
account = a

autopay_is_not_enabled_yet :: Proxy "autopay_is_not_enabled_yet"
autopay_is_not_enabled_yet = a

enable_autopay_desc :: Proxy "enable_autopay_desc"
enable_autopay_desc = a

enable_autopay_now :: Proxy "enable_autopay_now"
enable_autopay_now = a

autopay_setup_pending_str :: Proxy "autopay_setup_pending_str"
autopay_setup_pending_str = a

autopay_pending_desc_str :: Proxy "autopay_pending_desc_str"
autopay_pending_desc_str = a

refresh_str :: Proxy "refresh_str"
refresh_str = a

transaction_details :: Proxy "transaction_details"
transaction_details = a

ride_details :: Proxy "ride_details"
ride_details = a

my_plan_title :: Proxy "my_plan_title"
my_plan_title = a

switch_to :: Proxy "switch_to"
switch_to = a

your_rental_ride_starts_in :: Proxy "your_rental_ride_starts_in"
your_rental_ride_starts_in = a

your_intercity_ride_starts_in :: Proxy "your_intercity_ride_starts_in"
your_intercity_ride_starts_in = a

please_try_again :: Proxy "please_try_again"
please_try_again = a

plan_not_found :: Proxy "plan_not_found"
plan_not_found = a

mandate_not_found :: Proxy "mandate_not_found"
mandate_not_found = a

active_mandate_exists :: Proxy "active_mandate_exists"
active_mandate_exists = a

no_active_mandate_exist :: Proxy "no_active_mandate_exist"
no_active_mandate_exist = a

no_plan_for_driver :: Proxy "no_plan_for_driver"
no_plan_for_driver = a

invalid_payment_mode :: Proxy "invalid_payment_mode"
invalid_payment_mode = a

invalid_auto_pay_status :: Proxy "invalid_auto_pay_status"
invalid_auto_pay_status = a

max_amount :: Proxy "max_amount"
max_amount = a

frequency :: Proxy "frequency"
frequency = a

statred_on :: Proxy "statred_on"
statred_on = a

expires_on :: Proxy "expires_on"
expires_on = a

switched_plan :: Proxy "switched_plan"
switched_plan = a

resumed_autopay :: Proxy "resumed_autopay"
resumed_autopay = a

onetime :: Proxy "onetime"
onetime = a

weekly :: Proxy "weekly"
weekly = a

fortnightly :: Proxy "fortnightly"
fortnightly = a

monthly :: Proxy "monthly"
monthly = a

bimonthly :: Proxy "bimonthly"
bimonthly = a

quarterly :: Proxy "quarterly"
quarterly = a

halfyearly :: Proxy "halfyearly"
halfyearly = a

yearly :: Proxy "yearly"
yearly = a

aspresented :: Proxy "aspresented"
aspresented = a

first_free_ride :: Proxy "first_free_ride"
first_free_ride = a

daily_per_ride_desc :: Proxy "daily_per_ride_desc"
daily_per_ride_desc = a

join_the_unlimited_plan :: Proxy "join_the_unlimited_plan"
join_the_unlimited_plan = a

maybe_later :: Proxy "maybe_later"
maybe_later = a

do_you_want_to_cancel :: Proxy "do_you_want_to_cancel"
do_you_want_to_cancel = a

do_you_want_to_cancel_desc :: Proxy "do_you_want_to_cancel_desc"
do_you_want_to_cancel_desc = a

your_payment_was_unsuccessful :: Proxy "your_payment_was_unsuccessful"
your_payment_was_unsuccessful = a

payment_cancelled :: Proxy "payment_cancelled"
payment_cancelled = a

manual_payment_str :: Proxy "manual_payment_str"
manual_payment_str = a

upi_autopay_s :: Proxy "upi_autopay_s"
upi_autopay_s = a

daily_unlimited :: Proxy "daily_unlimited"
daily_unlimited = a

daily_per_ride :: Proxy "daily_per_ride"
daily_per_ride = a

daily_unlimited_plan_desc :: Proxy "daily_unlimited_plan_desc"
daily_unlimited_plan_desc = a

daily_per_ride_plan_desc :: Proxy "daily_per_ride_plan_desc"
daily_per_ride_plan_desc = a

autopay_cancelled :: Proxy "autopay_cancelled"
autopay_cancelled = a

no :: Proxy "no"
no = a

yes_cancel :: Proxy "yes_cancel"
yes_cancel = a

pay_to_join_this_plan :: Proxy "pay_to_join_this_plan"
pay_to_join_this_plan = a

offers_not_applicable :: Proxy "offers_not_applicable"
offers_not_applicable = a

paused_str :: Proxy "paused_str"
paused_str = a

pending_str :: Proxy "pending_str"
pending_str = a

switch_plan_str :: Proxy "switch_plan_str"
switch_plan_str = a

offers_applicable_on_daily_unlimited :: Proxy "offers_applicable_on_daily_unlimited"
offers_applicable_on_daily_unlimited = a

daily_unlimited_offer_not_available :: Proxy "daily_unlimited_offer_not_available"
daily_unlimited_offer_not_available = a

plan_switched_to :: Proxy "plan_switched_to"
plan_switched_to = a

no_rides_no_charge :: Proxy "no_rides_no_charge"
no_rides_no_charge = a

get_special_offers :: Proxy "get_special_offers"
get_special_offers = a

valid_only_if_payment :: Proxy "valid_only_if_payment"
valid_only_if_payment = a

help_str :: Proxy "help_str"
help_str = a

refresh_string :: Proxy "refresh_string"
refresh_string = a

chat_for_help :: Proxy "chat_for_help"
chat_for_help = a

view_faqs :: Proxy "view_faqs"
view_faqs = a

find_help_centre :: Proxy "find_help_centre"
find_help_centre = a

contact :: Proxy "contact"
contact = a

go_to_location :: Proxy "go_to_location"
go_to_location = a

no_help_center_is_active_now :: Proxy "no_help_center_is_active_now"
no_help_center_is_active_now = a

help_centers_location_will_appear_here_once_they_are_active :: Proxy "help_centers_location_will_appear_here_once_they_are_active"
help_centers_location_will_appear_here_once_they_are_active = a

support :: Proxy "support"
support = a

need_help_joining_the_plan :: Proxy "need_help_joining_the_plan"
need_help_joining_the_plan = a

need_help :: Proxy "need_help"
need_help = a

setup_autopay_now_to_get_special_discounts :: Proxy "setup_autopay_now_to_get_special_discounts"
setup_autopay_now_to_get_special_discounts = a

setup_now :: Proxy "setup_now"
setup_now = a

go_to_vehicle_details :: Proxy "go_to_vehicle_details"
go_to_vehicle_details = a

close :: Proxy "close"
close = a

rc_deactivated :: Proxy "rc_deactivated"
rc_deactivated = a

rc_deactivated_details :: Proxy "rc_deactivated_details"
rc_deactivated_details = a

customer_has_low_mobility :: Proxy "customer_has_low_mobility"
customer_has_low_mobility = a

customer_has_disability :: Proxy "customer_has_disability"
customer_has_disability = a

customer_has_low_vision :: Proxy "customer_has_low_vision"
customer_has_low_vision = a

customer_has_hearing_impairment :: Proxy "customer_has_hearing_impairment"
customer_has_hearing_impairment = a

help_with_their_mobility_aid :: Proxy "help_with_their_mobility_aid"
help_with_their_mobility_aid = a

please_assist_them_if_needed :: Proxy "please_assist_them_if_needed"
please_assist_them_if_needed = a

message_them_at_pickup :: Proxy "message_them_at_pickup"
message_them_at_pickup = a

sound_horn_once_at_pickup :: Proxy "sound_horn_once_at_pickup"
sound_horn_once_at_pickup = a

please_call_and_avoid_chats :: Proxy "please_call_and_avoid_chats"
please_call_and_avoid_chats = a

please_chat_and_avoid_calls :: Proxy "please_chat_and_avoid_calls"
please_chat_and_avoid_calls = a

please_go_to_exact_pickup :: Proxy "please_go_to_exact_pickup"
please_go_to_exact_pickup = a

customer_has_poor_vision_sound_horn_at_pickup :: Proxy "customer_has_poor_vision_sound_horn_at_pickup"
customer_has_poor_vision_sound_horn_at_pickup = a

customer_has_poor_hearing_message_them_at_pickup :: Proxy "customer_has_poor_hearing_message_them_at_pickup"
customer_has_poor_hearing_message_them_at_pickup = a

customer_has_low_mobility_store_their_support_at_pickup :: Proxy "customer_has_low_mobility_store_their_support_at_pickup"
customer_has_low_mobility_store_their_support_at_pickup = a

customer_has_disability_please_assist_them :: Proxy "customer_has_disability_please_assist_them"
customer_has_disability_please_assist_them = a

customer_may_need_assistance :: Proxy "customer_may_need_assistance"
customer_may_need_assistance = a

learn_more :: Proxy "learn_more"
learn_more = a

customer_has_low_mobility_go_to_exact_loc :: Proxy "customer_has_low_mobility_go_to_exact_loc"
customer_has_low_mobility_go_to_exact_loc = a

customer_has_poor_hearing_chat_with_them_instead_of_calling :: Proxy "customer_has_poor_hearing_chat_with_them_instead_of_calling"
customer_has_poor_hearing_chat_with_them_instead_of_calling = a

customer_has_low_vision_call_them_instead_of_chatting :: Proxy "customer_has_low_vision_call_them_instead_of_chatting"
customer_has_low_vision_call_them_instead_of_chatting = a

please_help_them_as_you_can :: Proxy "please_help_them_as_you_can"
please_help_them_as_you_can = a

learn_how_you_can_help_customers_requiring_special_assistance :: Proxy "learn_how_you_can_help_customers_requiring_special_assistance"
learn_how_you_can_help_customers_requiring_special_assistance = a

assistance_required :: Proxy "assistance_required"
assistance_required = a

saved_due_to_zero_commission :: Proxy "saved_due_to_zero_commission"
saved_due_to_zero_commission = a

tip_earned_from_customer :: Proxy "tip_earned_from_customer"
tip_earned_from_customer = a

collect_via_case_upi :: Proxy "collect_via_case_upi"
collect_via_case_upi = a

fare_collected :: Proxy "fare_collected"
fare_collected = a

rate_your_ride_with1 :: Proxy "rate_your_ride_with1"
rate_your_ride_with1 = a

rate_your_ride_with2 :: Proxy "rate_your_ride_with2"
rate_your_ride_with2 = a

help_us_with_your_feedback :: Proxy "help_us_with_your_feedback"
help_us_with_your_feedback = a

collect_cash :: Proxy "collect_cash"
collect_cash = a

online_payment :: Proxy "online_payment"
online_payment = a

ride_completed :: Proxy "ride_completed"
ride_completed = a

submit_feedback :: Proxy "submit_feedback"
submit_feedback = a

badge_earned :: Proxy "badge_earned"
badge_earned = a

purple_ride_champion :: Proxy "purple_ride_champion"
purple_ride_champion = a

purple_ride :: Proxy "purple_ride"
purple_ride = a

proceed_to_chat :: Proxy "proceed_to_chat"
proceed_to_chat = a

please_consider_calling_them :: Proxy "please_consider_calling_them"
please_consider_calling_them = a

join_a_plan_to_start_earning :: Proxy "join_a_plan_to_start_earning"
join_a_plan_to_start_earning = a

go_online_prompt_subscribe :: Proxy "go_online_prompt_subscribe"
go_online_prompt_subscribe = a

go_online_prompt_payment_pending :: Proxy "go_online_prompt_payment_pending"
go_online_prompt_payment_pending = a

complete_payment_to_continue :: Proxy "complete_payment_to_continue"
complete_payment_to_continue = a

downgrade_available_only_for_ac_vehicles :: Proxy "downgrade_available_only_for_ac_vehicles"
downgrade_available_only_for_ac_vehicles = a

downgrading_vehicle_will_allow_you_to_take_both_1 :: Proxy "downgrading_vehicle_will_allow_you_to_take_both_1"
downgrading_vehicle_will_allow_you_to_take_both_1 = a

downgrading_vehicle_will_allow_you_to_take_both_2 :: Proxy "downgrading_vehicle_will_allow_you_to_take_both_2"
downgrading_vehicle_will_allow_you_to_take_both_2 = a

downgrading_vehicle_will_allow_you_to_take_both_3 :: Proxy "downgrading_vehicle_will_allow_you_to_take_both_3"
downgrading_vehicle_will_allow_you_to_take_both_3 = a

ac_cab :: Proxy "ac_cab"
ac_cab = a

ac_suv :: Proxy "ac_suv"
ac_suv = a

downgrade_vehicle :: Proxy "downgrade_vehicle"
downgrade_vehicle = a

rental_bookings :: Proxy "rental_bookings"
rental_bookings = a

rental_bookings_description :: Proxy "rental_bookings_description"
rental_bookings_description = a

pending_caps :: Proxy "pending_caps"
pending_caps = a

failure :: Proxy "failure"
failure = a

payment_mode :: Proxy "payment_mode"
payment_mode = a

txn_id :: Proxy "txn_id"
txn_id = a

amount_paid :: Proxy "amount_paid"
amount_paid = a

notification_scheduled :: Proxy "notification_scheduled"
notification_scheduled = a

manual_dues :: Proxy "manual_dues"
manual_dues = a

autopay_in_progress :: Proxy "autopay_in_progress"
autopay_in_progress = a

manual_due_overview :: Proxy "manual_due_overview"
manual_due_overview = a

autopay_due_overview :: Proxy "autopay_due_overview"
autopay_due_overview = a

manual_due_as_autopay_execution_failed :: Proxy "manual_due_as_autopay_execution_failed"
manual_due_as_autopay_execution_failed = a

clear_manual_dues :: Proxy "clear_manual_dues"
clear_manual_dues = a

due_overview :: Proxy "due_overview"
due_overview = a

manual_due_details :: Proxy "manual_due_details"
manual_due_details = a

autopay_due_details :: Proxy "autopay_due_details"
autopay_due_details = a

switched_to_manual :: Proxy "switched_to_manual"
switched_to_manual = a

split_payment :: Proxy "split_payment"
split_payment = a

gst_include :: Proxy "gst_include"
gst_include = a

scheduled_at :: Proxy "scheduled_at"
scheduled_at = a

payment_status :: Proxy "payment_status"
payment_status = a

notification_attempting :: Proxy "notification_attempting"
notification_attempting = a

execution_scheduled :: Proxy "execution_scheduled"
execution_scheduled = a

execution_attempting :: Proxy "execution_attempting"
execution_attempting = a

execution_success :: Proxy "execution_success"
execution_success = a

scheduled :: Proxy "scheduled"
scheduled = a

one_time_settlement :: Proxy "one_time_settlement"
one_time_settlement = a

payment_scheduled :: Proxy "payment_scheduled"
payment_scheduled = a

retry_autopay :: Proxy "retry_autopay"
retry_autopay = a

retry_str :: Proxy "retry_str"
retry_str = a

ongoing_payment_execution :: Proxy "ongoing_payment_execution"
ongoing_payment_execution = a

offer_card_banner_title :: Proxy "offer_card_banner_title"
offer_card_banner_title = a

offer_card_banner_desc :: Proxy "offer_card_banner_desc"
offer_card_banner_desc = a

offer_card_banner_alert :: Proxy "offer_card_banner_alert"
offer_card_banner_alert = a

or :: Proxy "or"
or = a

collect_cash_directly :: Proxy "collect_cash_directly"
collect_cash_directly = a

or_collect_cash_directly :: Proxy "or_collect_cash_directly"
or_collect_cash_directly = a

setup_autopay_to_accept_payment :: Proxy "setup_autopay_to_accept_payment"
setup_autopay_to_accept_payment = a

download_qr :: Proxy "download_qr"
download_qr = a

use_this_qr_to_collect_payment :: Proxy "use_this_qr_to_collect_payment"
use_this_qr_to_collect_payment = a

amount_will_deposited_to_bank_account :: Proxy "amount_will_deposited_to_bank_account"
amount_will_deposited_to_bank_account = a

get_directly_to_your_bank_account :: Proxy "get_directly_to_your_bank_account"
get_directly_to_your_bank_account = a

payment :: Proxy "payment"
payment = a

qr_code :: Proxy "qr_code"
qr_code = a

get_qr_code :: Proxy "get_qr_code"
get_qr_code = a

execution_failed :: Proxy "execution_failed"
execution_failed = a

notification_failed :: Proxy "notification_failed"
notification_failed = a

clear_dues_banner_title :: Proxy "clear_dues_banner_title"
clear_dues_banner_title = a

pay_now :: Proxy "pay_now"
pay_now = a

collect_via_upi_qr_or_cash :: Proxy "collect_via_upi_qr_or_cash"
collect_via_upi_qr_or_cash = a

transaction_debited_on :: Proxy "transaction_debited_on"
transaction_debited_on = a

transaction_attempted_on :: Proxy "transaction_attempted_on"
transaction_attempted_on = a

autopay_setup_and_payment_successful :: Proxy "autopay_setup_and_payment_successful"
autopay_setup_and_payment_successful = a

autopay_setup_successful :: Proxy "autopay_setup_successful"
autopay_setup_successful = a

autopay_setup_and_payment_pending :: Proxy "autopay_setup_and_payment_pending"
autopay_setup_and_payment_pending = a

autopay_setup_pending :: Proxy "autopay_setup_pending"
autopay_setup_pending = a

autopay_setup_and_payment_failed :: Proxy "autopay_setup_and_payment_failed"
autopay_setup_and_payment_failed = a

autopay_setup_failed :: Proxy "autopay_setup_failed"
autopay_setup_failed = a

one_time_registeration :: Proxy "one_time_registeration"
one_time_registeration = a

clearance_and_registeration :: Proxy "clearance_and_registeration"
clearance_and_registeration = a

upi_autopay_setup :: Proxy "upi_autopay_setup"
upi_autopay_setup = a

watch_video_for_help :: Proxy "watch_video_for_help"
watch_video_for_help = a

payment_pending_soft_nudge :: Proxy "payment_pending_soft_nudge"
payment_pending_soft_nudge = a

clear_your_dues_early :: Proxy "clear_your_dues_early"
clear_your_dues_early = a

due_limit_warning_banner_title :: Proxy "due_limit_warning_banner_title"
due_limit_warning_banner_title = a

scheduled_on :: Proxy "scheduled_on"
scheduled_on = a

attempted_on :: Proxy "attempted_on"
attempted_on = a

free_trial_ending_tomorrow :: Proxy "free_trial_ending_tomorrow"
free_trial_ending_tomorrow = a

free_trial_ends_tonight :: Proxy "free_trial_ends_tonight"
free_trial_ends_tonight = a

join_a_plan_to_continue_taking_rides :: Proxy "join_a_plan_to_continue_taking_rides"
join_a_plan_to_continue_taking_rides = a

setup_autopay_for_easy_payments :: Proxy "setup_autopay_for_easy_payments"
setup_autopay_for_easy_payments = a

low_dues_clear_popup_desc :: Proxy "low_dues_clear_popup_desc"
low_dues_clear_popup_desc = a

dues_pending :: Proxy "dues_pending"
dues_pending = a

days :: Proxy "days"
days = a

active_plan :: Proxy "active_plan"
active_plan = a

what_are_purple_rides :: Proxy "what_are_purple_rides"
what_are_purple_rides = a

economical :: Proxy "economical"
economical = a

spacious :: Proxy "spacious"
spacious = a

comfy :: Proxy "comfy"
comfy = a

people :: Proxy "people"
people = a

go_to :: Proxy "go_to"
go_to = a

select_on_map :: Proxy "select_on_map"
select_on_map = a

confirm_location_str :: Proxy "confirm_location_str"
confirm_location_str = a

save_location_str :: Proxy "save_location_str"
save_location_str = a

remove_pref_loc :: Proxy "remove_pref_loc"
remove_pref_loc = a

conf_remove_pref_loc :: Proxy "conf_remove_pref_loc"
conf_remove_pref_loc = a

yes_remove :: Proxy "yes_remove"
yes_remove = a

add_location :: Proxy "add_location"
add_location = a

add_another_location :: Proxy "add_another_location"
add_another_location = a

add_a_goto_loc :: Proxy "add_a_goto_loc"
add_a_goto_loc = a

goto_loc_left :: Proxy "goto_loc_left"
goto_loc_left = a

current_location :: Proxy "current_location"
current_location = a

conf_goto_loc :: Proxy "conf_goto_loc"
conf_goto_loc = a

goto_locs :: Proxy "goto_locs"
goto_locs = a

location_str :: Proxy "location_str"
location_str = a

add_tag :: Proxy "add_tag"
add_tag = a

only_one_loc_can_added :: Proxy "only_one_loc_can_added"
only_one_loc_can_added = a

save_as :: Proxy "save_as"
save_as = a

no_goto_loc_added :: Proxy "no_goto_loc_added"
no_goto_loc_added = a

goto_loc_helps_you :: Proxy "goto_loc_helps_you"
goto_loc_helps_you = a

you_are_very_close :: Proxy "you_are_very_close"
you_are_very_close = a

goto_is_applicable_for :: Proxy "goto_is_applicable_for"
goto_is_applicable_for = a

cancel_anyway :: Proxy "cancel_anyway"
cancel_anyway = a

goto_maybe_reduced :: Proxy "goto_maybe_reduced"
goto_maybe_reduced = a

cancel_of_goto :: Proxy "cancel_of_goto"
cancel_of_goto = a

more_goto_ride_coming :: Proxy "more_goto_ride_coming"
more_goto_ride_coming = a

more_goto_ride_coming_desc :: Proxy "more_goto_ride_coming_desc"
more_goto_ride_coming_desc = a

goto_reduced_to_zero :: Proxy "goto_reduced_to_zero"
goto_reduced_to_zero = a

due_to_multiple_cancellations :: Proxy "due_to_multiple_cancellations"
due_to_multiple_cancellations = a

ok_got_it :: Proxy "ok_got_it"
ok_got_it = a

goto_reduced_to :: Proxy "goto_reduced_to"
goto_reduced_to = a

validity_expired_str :: Proxy "validity_expired_str"
validity_expired_str = a

validity_expired_desc :: Proxy "validity_expired_desc"
validity_expired_desc = a

know_more :: Proxy "know_more"
know_more = a

this_feature_will_be_applicable :: Proxy "this_feature_will_be_applicable"
this_feature_will_be_applicable = a

goto_loc_added :: Proxy "goto_loc_added"
goto_loc_added = a

goto_loc_removed :: Proxy "goto_loc_removed"
goto_loc_removed = a

goto_loc_updated :: Proxy "goto_loc_updated"
goto_loc_updated = a

goto_loc_is_enabled :: Proxy "goto_loc_is_enabled"
goto_loc_is_enabled = a

goto_loc_is_disabled :: Proxy "goto_loc_is_disabled"
goto_loc_is_disabled = a

goto_locations :: Proxy "goto_locations"
goto_locations = a

choose_a_goto_loc :: Proxy "choose_a_goto_loc"
choose_a_goto_loc = a

you_have_only_left_for_today :: Proxy "you_have_only_left_for_today"
you_have_only_left_for_today = a

yes_enable :: Proxy "yes_enable"
yes_enable = a

no_goto_locs_added_yet :: Proxy "no_goto_locs_added_yet"
no_goto_locs_added_yet = a

no_goto_locs_added_yet_desc :: Proxy "no_goto_locs_added_yet_desc"
no_goto_locs_added_yet_desc = a

enable_goto :: Proxy "enable_goto"
enable_goto = a

go_to_cancellation_title :: Proxy "go_to_cancellation_title"
go_to_cancellation_title = a

go_to_cancellation_desc :: Proxy "go_to_cancellation_desc"
go_to_cancellation_desc = a

disable_goto_str :: Proxy "disable_goto_str"
disable_goto_str = a

you_still_have_time_left :: Proxy "you_still_have_time_left"
you_still_have_time_left = a

yes_disable :: Proxy "yes_disable"
yes_disable = a

goto_loc_reached :: Proxy "goto_loc_reached"
goto_loc_reached = a

you_are_almost_at_location :: Proxy "you_are_almost_at_location"
you_are_almost_at_location = a

driver_home_location_not_found :: Proxy "driver_home_location_not_found"
driver_home_location_not_found = a

driver_home_location_does_not_exist :: Proxy "driver_home_location_does_not_exist"
driver_home_location_does_not_exist = a

driver_home_location_limit_reached :: Proxy "driver_home_location_limit_reached"
driver_home_location_limit_reached = a

driver_go_home_request_not_found :: Proxy "driver_go_home_request_not_found"
driver_go_home_request_not_found = a

driver_go_home_request_does_not_exist :: Proxy "driver_go_home_request_does_not_exist"
driver_go_home_request_does_not_exist = a

driver_go_home_request_daily_usage_limit_reached :: Proxy "driver_go_home_request_daily_usage_limit_reached"
driver_go_home_request_daily_usage_limit_reached = a

driver_go_home_request_already_active :: Proxy "driver_go_home_request_already_active"
driver_go_home_request_already_active = a

report_issue :: Proxy "report_issue"
report_issue = a

driver_home_location_outside_service_area :: Proxy "driver_home_location_outside_service_area"
driver_home_location_outside_service_area = a

new_location_too_close_to_previous_home_location :: Proxy "new_location_too_close_to_previous_home_location"
new_location_too_close_to_previous_home_location = a

driver_home_location_does_not_belong_to_driver :: Proxy "driver_home_location_does_not_belong_to_driver"
driver_home_location_does_not_belong_to_driver = a

driver_home_location_delete_while_active_error :: Proxy "driver_home_location_delete_while_active_error"
driver_home_location_delete_while_active_error = a

drag_to_adjust :: Proxy "drag_to_adjust"
drag_to_adjust = a

location_already_exists :: Proxy "location_already_exists"
location_already_exists = a

min_left :: Proxy "min_left"
min_left = a

get_ready_for_ys_subscription :: Proxy "get_ready_for_ys_subscription"
get_ready_for_ys_subscription = a

signup_early_for_special_offers :: Proxy "signup_early_for_special_offers"
signup_early_for_special_offers = a

guaranteed_fixed_price :: Proxy "guaranteed_fixed_price"
guaranteed_fixed_price = a

introductory_offer_to_be_announced_soon :: Proxy "introductory_offer_to_be_announced_soon"
introductory_offer_to_be_announced_soon = a

no_charges_till :: Proxy "no_charges_till"
no_charges_till = a

driver_go_home_request_not_present :: Proxy "driver_go_home_request_not_present"
driver_go_home_request_not_present = a

and :: Proxy "and"
and = a

direct_payment_no_commissions :: Proxy "direct_payment_no_commissions"
direct_payment_no_commissions = a

customer_pays_directly :: Proxy "customer_pays_directly"
customer_pays_directly = a

hundred_percent_fare_goes_to_you :: Proxy "hundred_percent_fare_goes_to_you"
hundred_percent_fare_goes_to_you = a

fare_shown_is_fare_you_get :: Proxy "fare_shown_is_fare_you_get"
fare_shown_is_fare_you_get = a

be_a_part_of_open_mobility_revolution :: Proxy "be_a_part_of_open_mobility_revolution"
be_a_part_of_open_mobility_revolution = a

our_data_and_product_are_transparent :: Proxy "our_data_and_product_are_transparent"
our_data_and_product_are_transparent = a

your_detected_location_is :: Proxy "your_detected_location_is"
your_detected_location_is = a

language_detected :: Proxy "language_detected"
language_detected = a

change_language_str :: Proxy "change_language_str"
change_language_str = a

select_location :: Proxy "select_location"
select_location = a

select_location_desc :: Proxy "select_location_desc"
select_location_desc = a

select_language_desc :: Proxy "select_language_desc"
select_language_desc = a

confirm_language :: Proxy "confirm_language"
confirm_language = a

get_started :: Proxy "get_started"
get_started = a

enable_location_permission :: Proxy "enable_location_permission"
enable_location_permission = a

please_enable_location_permission_for :: Proxy "please_enable_location_permission_for"
please_enable_location_permission_for = a

enable_location :: Proxy "enable_location"
enable_location = a

by_clicking_next_you_will_be_agreeing_to_our :: Proxy "by_clicking_next_you_will_be_agreeing_to_our"
by_clicking_next_you_will_be_agreeing_to_our = a

enter_your_mobile_number :: Proxy "enter_your_mobile_number"
enter_your_mobile_number = a

notification_access :: Proxy "notification_access"
notification_access = a

notification_access_desc :: Proxy "notification_access_desc"
notification_access_desc = a

watch_video :: Proxy "watch_video"
watch_video = a

dl_verification_failed :: Proxy "dl_verification_failed"
dl_verification_failed = a

rc_verification_failed :: Proxy "rc_verification_failed"
rc_verification_failed = a

dl_upload_failed :: Proxy "dl_upload_failed"
dl_upload_failed = a

rc_upload_failed :: Proxy "rc_upload_failed"
rc_upload_failed = a

please_retry_the_upload_again :: Proxy "please_retry_the_upload_again"
please_retry_the_upload_again = a

rc_and_dl_upload_failed :: Proxy "rc_and_dl_upload_failed"
rc_and_dl_upload_failed = a

rc_upload_limit_reached :: Proxy "rc_upload_limit_reached"
rc_upload_limit_reached = a

dl_upload_limit_reached :: Proxy "dl_upload_limit_reached"
dl_upload_limit_reached = a

retry_upload :: Proxy "retry_upload"
retry_upload = a

vehicle_registeraton_certificate :: Proxy "vehicle_registeraton_certificate"
vehicle_registeraton_certificate = a

grant_permissions :: Proxy "grant_permissions"
grant_permissions = a

subscription_plan_str :: Proxy "subscription_plan_str"
subscription_plan_str = a

complete_autopay_later :: Proxy "complete_autopay_later"
complete_autopay_later = a

start_earning_in_four_steps :: Proxy "start_earning_in_four_steps"
start_earning_in_four_steps = a

complete :: Proxy "complete"
complete = a

how_to_upload :: Proxy "how_to_upload"
how_to_upload = a

take_clear_picture_dl :: Proxy "take_clear_picture_dl"
take_clear_picture_dl = a

ensure_adequate_light :: Proxy "ensure_adequate_light"
ensure_adequate_light = a

fit_dl_correctly :: Proxy "fit_dl_correctly"
fit_dl_correctly = a

take_photo :: Proxy "take_photo"
take_photo = a

fit_rc_correctly :: Proxy "fit_rc_correctly"
fit_rc_correctly = a

take_clear_picture_rc :: Proxy "take_clear_picture_rc"
take_clear_picture_rc = a

dl_uploaded :: Proxy "dl_uploaded"
dl_uploaded = a

rc_uploaded :: Proxy "rc_uploaded"
rc_uploaded = a

dl_uploading :: Proxy "dl_uploading"
dl_uploading = a

rc_uploading :: Proxy "rc_uploading"
rc_uploading = a

retake_rc :: Proxy "retake_rc"
retake_rc = a

retake_dl :: Proxy "retake_dl"
retake_dl = a

confirm_and_upload :: Proxy "confirm_and_upload"
confirm_and_upload = a

retake_photo :: Proxy "retake_photo"
retake_photo = a

change_city :: Proxy "change_city"
change_city = a

lets_get_you_trip_ready :: Proxy "lets_get_you_trip_ready"
lets_get_you_trip_ready = a

got_an_otp :: Proxy "got_an_otp"
got_an_otp = a

driving_license_details :: Proxy "driving_license_details"
driving_license_details = a

vehicle_registration_details :: Proxy "vehicle_registration_details"
vehicle_registration_details = a

upload_registration_certificate_str :: Proxy "upload_registration_certificate_str"
upload_registration_certificate_str = a

upload_photo :: Proxy "upload_photo"
upload_photo = a

clear_image :: Proxy "clear_image"
clear_image = a

blurry_image :: Proxy "blurry_image"
blurry_image = a

cropped_correctly :: Proxy "cropped_correctly"
cropped_correctly = a

wrong_cropping :: Proxy "wrong_cropping"
wrong_cropping = a

change_location :: Proxy "change_location"
change_location = a

rc_verification_in_progress :: Proxy "rc_verification_in_progress"
rc_verification_in_progress = a

rc_verification_failed_status :: Proxy "rc_verification_failed_status"
rc_verification_failed_status = a

rc_verification_success :: Proxy "rc_verification_success"
rc_verification_success = a

rc_in_progress_desc :: Proxy "rc_in_progress_desc"
rc_in_progress_desc = a

rc_failed_desc :: Proxy "rc_failed_desc"
rc_failed_desc = a

take_a_photo :: Proxy "take_a_photo"
take_a_photo = a

gallery :: Proxy "gallery"
gallery = a

unable_to_detect_your_location :: Proxy "unable_to_detect_your_location"
unable_to_detect_your_location = a

detecting_location :: Proxy "detecting_location"
detecting_location = a

get_full_payment :: Proxy "get_full_payment"
get_full_payment = a

select_city_str :: Proxy "select_city_str"
select_city_str = a

we_are_not_live_in_your_area :: Proxy "we_are_not_live_in_your_area"
we_are_not_live_in_your_area = a

location_unserviceable :: Proxy "location_unserviceable"
location_unserviceable = a

unable_to_get_your_location :: Proxy "unable_to_get_your_location"
unable_to_get_your_location = a

turn_off_any_mock_location_app_and_restart :: Proxy "turn_off_any_mock_location_app_and_restart"
turn_off_any_mock_location_app_and_restart = a

this_extra_amount_the_customer_will_pay :: Proxy "this_extra_amount_the_customer_will_pay"
this_extra_amount_the_customer_will_pay = a

ten_digit_mobile_number :: Proxy "ten_digit_mobile_number"
ten_digit_mobile_number = a

booth_charges :: Proxy "booth_charges"
booth_charges = a

booth_charges_included :: Proxy "booth_charges_included"
booth_charges_included = a

total_amount :: Proxy "total_amount"
total_amount = a

please_add_rc :: Proxy "please_add_rc"
please_add_rc = a

location_cannot_be_added_while_on_ride :: Proxy "location_cannot_be_added_while_on_ride"
location_cannot_be_added_while_on_ride = a

location_cannot_be_added_while_goto_active :: Proxy "location_cannot_be_added_while_goto_active"
location_cannot_be_added_while_goto_active = a

add_goto :: Proxy "add_goto"
add_goto = a

no_open_market_rides :: Proxy "no_open_market_rides"
no_open_market_rides = a

account_blocked :: Proxy "account_blocked"
account_blocked = a

you_have_been_blocked_from_taking_rides :: Proxy "you_have_been_blocked_from_taking_rides"
you_have_been_blocked_from_taking_rides = a

dismiss :: Proxy "dismiss"
dismiss = a

earnings :: Proxy "earnings"
earnings = a

yatri_points :: Proxy "yatri_points"
yatri_points = a

discount_points :: Proxy "discount_points"
discount_points = a

yatri_points_str :: Proxy "yatri_points_str"
yatri_points_str = a

introducing_yatri_points :: Proxy "introducing_yatri_points"
introducing_yatri_points = a

now_earn_points_for_every_ride_and_referral_and_use_them_to_get_rewards :: Proxy "now_earn_points_for_every_ride_and_referral_and_use_them_to_get_rewards"
now_earn_points_for_every_ride_and_referral_and_use_them_to_get_rewards = a

paid_by_yatri_points :: Proxy "paid_by_yatri_points"
paid_by_yatri_points = a

discount_points_small :: Proxy "discount_points_small"
discount_points_small = a

yatri_points_usage_popup :: Proxy "yatri_points_usage_popup"
yatri_points_usage_popup = a

yatri_points_usage_secondary :: Proxy "yatri_points_usage_secondary"
yatri_points_usage_secondary = a

use_points_now :: Proxy "use_points_now"
use_points_now = a

buy_now :: Proxy "buy_now"
buy_now = a

select_date :: Proxy "select_date"
select_date = a

what_will_my_points_be_converted_to :: Proxy "what_will_my_points_be_converted_to"
what_will_my_points_be_converted_to = a

points_expiring_in_the_next :: Proxy "points_expiring_in_the_next"
points_expiring_in_the_next = a

days_use_them_before_they_expire :: Proxy "days_use_them_before_they_expire"
days_use_them_before_they_expire = a

points_expiring :: Proxy "points_expiring"
points_expiring = a

no_points_available :: Proxy "no_points_available"
no_points_available = a

failed_to_use_points_please_try_again_later :: Proxy "failed_to_use_points_please_try_again_later"
failed_to_use_points_please_try_again_later = a

bad_rating_by_customer :: Proxy "bad_rating_by_customer"
bad_rating_by_customer = a

good_rating_by_customer :: Proxy "good_rating_by_customer"
good_rating_by_customer = a

ride_cancellation :: Proxy "ride_cancellation"
ride_cancellation = a

customer_referral :: Proxy "customer_referral"
customer_referral = a

customer_should_complete_a_valid_ride :: Proxy "customer_should_complete_a_valid_ride"
customer_should_complete_a_valid_ride = a

driver_referral :: Proxy "driver_referral"
driver_referral = a

purple_ride_completed :: Proxy "purple_ride_completed"
purple_ride_completed = a

training_complted :: Proxy "training_complted"
training_complted = a

rides_in_a_day :: Proxy "rides_in_a_day"
rides_in_a_day = a

top :: Proxy "top"
top = a

in_weekly_leaderboard :: Proxy "in_weekly_leaderboard"
in_weekly_leaderboard = a

trip_earnings :: Proxy "trip_earnings"
trip_earnings = a

extra_earnings :: Proxy "extra_earnings"
extra_earnings = a

trips :: Proxy "trips"
trips = a

view_more :: Proxy "view_more"
view_more = a

minimum :: Proxy "minimum"
minimum = a

points_is_required_for_conversion :: Proxy "points_is_required_for_conversion"
points_is_required_for_conversion = a

discount :: Proxy "discount"
discount = a

check_now :: Proxy "check_now"
check_now = a

yatri_points_faqs :: Proxy "yatri_points_faqs"
yatri_points_faqs = a

learn_about_yatri_points :: Proxy "learn_about_yatri_points"
learn_about_yatri_points = a

yatri_points_faqs_ques1 :: Proxy "yatri_points_faqs_ques1"
yatri_points_faqs_ques1 = a

yatri_points_faqs_ques1_ans1 :: Proxy "yatri_points_faqs_ques1_ans1"
yatri_points_faqs_ques1_ans1 = a

yatri_points_faqs_ques1_ans2 :: Proxy "yatri_points_faqs_ques1_ans2"
yatri_points_faqs_ques1_ans2 = a

yatri_points_faqs_ques1_ans3 :: Proxy "yatri_points_faqs_ques1_ans3"
yatri_points_faqs_ques1_ans3 = a

yatri_points_faqs_ques2 :: Proxy "yatri_points_faqs_ques2"
yatri_points_faqs_ques2 = a

yatri_points_faqs_ques2_ans1 :: Proxy "yatri_points_faqs_ques2_ans1"
yatri_points_faqs_ques2_ans1 = a

yatri_points_faqs_ques2_ans2 :: Proxy "yatri_points_faqs_ques2_ans2"
yatri_points_faqs_ques2_ans2 = a

yatri_points_faqs_ques3 :: Proxy "yatri_points_faqs_ques3"
yatri_points_faqs_ques3 = a

yatri_points_faqs_ques3_ans1 :: Proxy "yatri_points_faqs_ques3_ans1"
yatri_points_faqs_ques3_ans1 = a

yatri_points_faqs_ques3_ans2 :: Proxy "yatri_points_faqs_ques3_ans2"
yatri_points_faqs_ques3_ans2 = a

yatri_points_faqs_ques4 :: Proxy "yatri_points_faqs_ques4"
yatri_points_faqs_ques4 = a

yatri_points_faqs_ques4_ans1 :: Proxy "yatri_points_faqs_ques4_ans1"
yatri_points_faqs_ques4_ans1 = a

yatri_points_faqs_ques4_ans2 :: Proxy "yatri_points_faqs_ques4_ans2"
yatri_points_faqs_ques4_ans2 = a

yatri_points_faqs_ques4_ans3 :: Proxy "yatri_points_faqs_ques4_ans3"
yatri_points_faqs_ques4_ans3 = a

yatri_points_faqs_ques5 :: Proxy "yatri_points_faqs_ques5"
yatri_points_faqs_ques5 = a

yatri_points_faqs_ques5_ans1 :: Proxy "yatri_points_faqs_ques5_ans1"
yatri_points_faqs_ques5_ans1 = a

yatri_points_faqs_ques5_ans2 :: Proxy "yatri_points_faqs_ques5_ans2"
yatri_points_faqs_ques5_ans2 = a

yatri_points_faqs_ques6 :: Proxy "yatri_points_faqs_ques6"
yatri_points_faqs_ques6 = a

yatri_points_faqs_ques6_ans1 :: Proxy "yatri_points_faqs_ques6_ans1"
yatri_points_faqs_ques6_ans1 = a

task_completed :: Proxy "task_completed"
task_completed = a

rides_in_a_day_prefix :: Proxy "rides_in_a_day_prefix"
rides_in_a_day_prefix = a

rides_in_a_day_suffix :: Proxy "rides_in_a_day_suffix"
rides_in_a_day_suffix = a

star_rating_for_the_trip :: Proxy "star_rating_for_the_trip"
star_rating_for_the_trip = a

one_two_start_rating :: Proxy "one_two_start_rating"
one_two_start_rating = a

booking_cancellation :: Proxy "booking_cancellation"
booking_cancellation = a

paid_by :: Proxy "paid_by"
paid_by = a

driver_referral_code :: Proxy "driver_referral_code"
driver_referral_code = a

app_qr_code :: Proxy "app_qr_code"
app_qr_code = a

start_taking_rides_and_refer :: Proxy "start_taking_rides_and_refer"
start_taking_rides_and_refer = a

referred_drivers :: Proxy "referred_drivers"
referred_drivers = a

ride_leaderboard :: Proxy "ride_leaderboard"
ride_leaderboard = a

your_rank :: Proxy "your_rank"
your_rank = a

not_available_yet :: Proxy "not_available_yet"
not_available_yet = a

enter_referral_code :: Proxy "enter_referral_code"
enter_referral_code = a

have_a_referral_code :: Proxy "have_a_referral_code"
have_a_referral_code = a

complete_steps_to_apply_referral :: Proxy "complete_steps_to_apply_referral"
complete_steps_to_apply_referral = a

download_namma_yatri :: Proxy "download_namma_yatri"
download_namma_yatri = a

enter_code :: Proxy "enter_code"
enter_code = a

complete_registration :: Proxy "complete_registration"
complete_registration = a

cant_find_option :: Proxy "cant_find_option"
cant_find_option = a

convert_points :: Proxy "convert_points"
convert_points = a

help_faq :: Proxy "help_faq"
help_faq = a

bonus_points :: Proxy "bonus_points"
bonus_points = a

max :: Proxy "max"
max = a

coins :: Proxy "coins"
coins = a

points_added :: Proxy "points_added"
points_added = a

watch_now :: Proxy "watch_now"
watch_now = a

choose_a_plan :: Proxy "choose_a_plan"
choose_a_plan = a

referral :: Proxy "referral"
referral = a

benefits :: Proxy "benefits"
benefits = a

your_daily_rank :: Proxy "your_daily_rank"
your_daily_rank = a

click_to_expand :: Proxy "click_to_expand"
click_to_expand = a

referred :: Proxy "referred"
referred = a

activated :: Proxy "activated"
activated = a

refer_driver :: Proxy "refer_driver"
refer_driver = a

refer_customer :: Proxy "refer_customer"
refer_customer = a

referred_drivers_info :: Proxy "referred_drivers_info"
referred_drivers_info = a

referred_customers_info :: Proxy "referred_customers_info"
referred_customers_info = a

activated_customers_info :: Proxy "activated_customers_info"
activated_customers_info = a

customer_referral_code :: Proxy "customer_referral_code"
customer_referral_code = a

accept_ride_to_enter_leaderboard :: Proxy "accept_ride_to_enter_leaderboard"
accept_ride_to_enter_leaderboard = a

contact_support_via :: Proxy "contact_support_via"
contact_support_via = a

you_can_share_screenshot :: Proxy "you_can_share_screenshot"
you_can_share_screenshot = a

place_a_call :: Proxy "place_a_call"
place_a_call = a

terms_and_conditions_updated :: Proxy "terms_and_conditions_updated"
terms_and_conditions_updated = a

safety_is_our_responsibility :: Proxy "safety_is_our_responsibility"
safety_is_our_responsibility = a

customer_safety_first :: Proxy "customer_safety_first"
customer_safety_first = a

lets_ensure_safe_ride :: Proxy "lets_ensure_safe_ride"
lets_ensure_safe_ride = a

customer_safety_our_resp_happy_ride :: Proxy "customer_safety_our_resp_happy_ride"
customer_safety_our_resp_happy_ride = a

our_safety_partner :: Proxy "our_safety_partner"
our_safety_partner = a

quiz :: Proxy "quiz"
quiz = a

kannada :: Proxy "kannada"
kannada = a

tamil :: Proxy "tamil"
tamil = a

telugu :: Proxy "telugu"
telugu = a

french :: Proxy "french"
french = a

bengali :: Proxy "bengali"
bengali = a

you_have_successfully_completed :: Proxy "you_have_successfully_completed"
you_have_successfully_completed = a

all_answers_should_be_correct_to_complete :: Proxy "all_answers_should_be_correct_to_complete"
all_answers_should_be_correct_to_complete = a

questions_should_be_correct_to_complete :: Proxy "questions_should_be_correct_to_complete"
questions_should_be_correct_to_complete = a

correct :: Proxy "correct"
correct = a

retake_quiz :: Proxy "retake_quiz"
retake_quiz = a

take_a_quiz :: Proxy "take_a_quiz"
take_a_quiz = a

play_again :: Proxy "play_again"
play_again = a

play_now :: Proxy "play_now"
play_now = a

watch_all_videos_to_learn :: Proxy "watch_all_videos_to_learn"
watch_all_videos_to_learn = a

play_quiz_to_complete_your_training :: Proxy "play_quiz_to_complete_your_training"
play_quiz_to_complete_your_training = a

training_completed :: Proxy "training_completed"
training_completed = a

watched :: Proxy "watched"
watched = a

uh_oh_something_went_wrong :: Proxy "uh_oh_something_went_wrong"
uh_oh_something_went_wrong = a

watch_all_videos_to_unlock_quiz :: Proxy "watch_all_videos_to_unlock_quiz"
watch_all_videos_to_unlock_quiz = a

incomplete :: Proxy "incomplete"
incomplete = a

new_c :: Proxy "new_c"
new_c = a

pending_str_c :: Proxy "pending_str_c"
pending_str_c = a

completed_str :: Proxy "completed_str"
completed_str = a

videos :: Proxy "videos"
videos = a

learn_and_earn :: Proxy "learn_and_earn"
learn_and_earn = a

unable_to_change_language_please_try_again :: Proxy "unable_to_change_language_please_try_again"
unable_to_change_language_please_try_again = a

unable_to_load_quiz_please_try_again :: Proxy "unable_to_load_quiz_please_try_again"
unable_to_load_quiz_please_try_again = a

we_guarantee_you :: Proxy "we_guarantee_you"
we_guarantee_you = a

lowest_fees_from :: Proxy "lowest_fees_from"
lowest_fees_from = a

zero_fee_till :: Proxy "zero_fee_till"
zero_fee_till = a

zero_commision_unlimited_rides :: Proxy "zero_commision_unlimited_rides"
zero_commision_unlimited_rides = a

we_are_currently_live_with_vehicle :: Proxy "we_are_currently_live_with_vehicle"
we_are_currently_live_with_vehicle = a

we_are_currently_live_with_vehicle_desc :: Proxy "we_are_currently_live_with_vehicle_desc"
we_are_currently_live_with_vehicle_desc = a

exit_the_quiz :: Proxy "exit_the_quiz"
exit_the_quiz = a

exit_and_start_again_later :: Proxy "exit_and_start_again_later"
exit_and_start_again_later = a

select_the_language_you_can_read :: Proxy "select_the_language_you_can_read"
select_the_language_you_can_read = a

check_app :: Proxy "check_app"
check_app = a

check_your_app_by_test_ride_request :: Proxy "check_your_app_by_test_ride_request"
check_your_app_by_test_ride_request = a

please_try_the_following_steps :: Proxy "please_try_the_following_steps"
please_try_the_following_steps = a

seems_like_there_is_a_problem :: Proxy "seems_like_there_is_a_problem"
seems_like_there_is_a_problem = a

did_you_receive_test_ride :: Proxy "did_you_receive_test_ride"
did_you_receive_test_ride = a

everything_is_ok :: Proxy "everything_is_ok"
everything_is_ok = a

call_our_support_team :: Proxy "call_our_support_team"
call_our_support_team = a

move_to_high_demand_area :: Proxy "move_to_high_demand_area"
move_to_high_demand_area = a

yes :: Proxy "yes"
yes = a

get_support_on_whatsapp :: Proxy "get_support_on_whatsapp"
get_support_on_whatsapp = a

know_about_points :: Proxy "know_about_points"
know_about_points = a

not_enough_points_description :: Proxy "not_enough_points_description"
not_enough_points_description = a

share :: Proxy "share"
share = a

share_namma_yatri :: Proxy "share_namma_yatri"
share_namma_yatri = a

share_namma_yatri_message :: Proxy "share_namma_yatri_message"
share_namma_yatri_message = a

be_open_choose_open :: Proxy "be_open_choose_open"
be_open_choose_open = a

now :: Proxy "now"
now = a

add_vehicle :: Proxy "add_vehicle"
add_vehicle = a

select_your_vehicle_type :: Proxy "select_your_vehicle_type"
select_your_vehicle_type = a

car :: Proxy "car"
car = a

special_pickup_zone_nearby :: Proxy "special_pickup_zone_nearby"
special_pickup_zone_nearby = a

zone_pickup :: Proxy "zone_pickup"
zone_pickup = a

special_pickup_zone_ride :: Proxy "special_pickup_zone_ride"
special_pickup_zone_ride = a

special_pickup_zone :: Proxy "special_pickup_zone"
special_pickup_zone = a

special_pickup_zone_popup_info :: Proxy "special_pickup_zone_popup_info"
special_pickup_zone_popup_info = a

inside_special_pickup_zone_popup_info :: Proxy "inside_special_pickup_zone_popup_info"
inside_special_pickup_zone_popup_info = a

select_a_green_area_for_priority_rides :: Proxy "select_a_green_area_for_priority_rides"
select_a_green_area_for_priority_rides = a

priority_ride_expierence :: Proxy "priority_ride_expierence"
priority_ride_expierence = a

duration :: Proxy "duration"
duration = a

rental_fare :: Proxy "rental_fare"
rental_fare = a

start_time :: Proxy "start_time"
start_time = a

start_odo_reading :: Proxy "start_odo_reading"
start_odo_reading = a

ride_start :: Proxy "ride_start"
ride_start = a

ride_end :: Proxy "ride_end"
ride_end = a

ride_started_at :: Proxy "ride_started_at"
ride_started_at = a

ride_ended_at :: Proxy "ride_ended_at"
ride_ended_at = a

odometer_reading :: Proxy "odometer_reading"
odometer_reading = a

picked_up_at :: Proxy "picked_up_at"
picked_up_at = a

upcoming_stop :: Proxy "upcoming_stop"
upcoming_stop = a

last_stop :: Proxy "last_stop"
last_stop = a

previous_stop :: Proxy "previous_stop"
previous_stop = a

ride_time :: Proxy "ride_time"
ride_time = a

you_are_on_a_rental_ride :: Proxy "you_are_on_a_rental_ride"
you_are_on_a_rental_ride = a

you_are_on_a_intercity_ride :: Proxy "you_are_on_a_intercity_ride"
you_are_on_a_intercity_ride = a

enter_end_ride_otp :: Proxy "enter_end_ride_otp"
enter_end_ride_otp = a

you_are_not_at_stop_location :: Proxy "you_are_not_at_stop_location"
you_are_not_at_stop_location = a

arrived_at_stop :: Proxy "arrived_at_stop"
arrived_at_stop = a

enable_loc_permission_to_get_rides :: Proxy "enable_loc_permission_to_get_rides"
enable_loc_permission_to_get_rides = a

enable_loc_per_from_settings :: Proxy "enable_loc_per_from_settings"
enable_loc_per_from_settings = a

enable_permission_str :: Proxy "enable_permission_str"
enable_permission_str = a

capture_doc_desc_1 :: Proxy "capture_doc_desc_1"
capture_doc_desc_1 = a

capture_doc_desc_2 :: Proxy "capture_doc_desc_2"
capture_doc_desc_2 = a

capture_doc_desc_3 :: Proxy "capture_doc_desc_3"
capture_doc_desc_3 = a

upload_doc :: Proxy "upload_doc"
upload_doc = a

register_your_car :: Proxy "register_your_car"
register_your_car = a

register_your_auto :: Proxy "register_your_auto"
register_your_auto = a

register_your_ambulance :: Proxy "register_your_ambulance"
register_your_ambulance = a

do_you_want_to_change_vt :: Proxy "do_you_want_to_change_vt"
do_you_want_to_change_vt = a

yes_change_vehicle :: Proxy "yes_change_vehicle"
yes_change_vehicle = a

change_vehicle :: Proxy "change_vehicle"
change_vehicle = a

vehicle_type_mismatch :: Proxy "vehicle_type_mismatch"
vehicle_type_mismatch = a

uploaded_doc_doesnt_match :: Proxy "uploaded_doc_doesnt_match"
uploaded_doc_doesnt_match = a

change_vehicle_type :: Proxy "change_vehicle_type"
change_vehicle_type = a

upload_different_rc :: Proxy "upload_different_rc"
upload_different_rc = a

profile_photo_str :: Proxy "profile_photo_str"
profile_photo_str = a

aadhaar_card_str :: Proxy "aadhaar_card_str"
aadhaar_card_str = a

pan_card_str :: Proxy "pan_card_str"
pan_card_str = a

vehicle_permit_str :: Proxy "vehicle_permit_str"
vehicle_permit_str = a

fitness_certificate_str :: Proxy "fitness_certificate_str"
fitness_certificate_str = a

vehicle_insurance_str :: Proxy "vehicle_insurance_str"
vehicle_insurance_str = a

vehicle_puc_str :: Proxy "vehicle_puc_str"
vehicle_puc_str = a

rc_mandatory :: Proxy "rc_mandatory"
rc_mandatory = a

document_uploaded_successfully :: Proxy "document_uploaded_successfully"
document_uploaded_successfully = a

toll_charges_including :: Proxy "toll_charges_including"
toll_charges_including = a

toll_road_changed :: Proxy "toll_road_changed"
toll_road_changed = a

ride_toll_fare_includes :: Proxy "ride_toll_fare_includes"
ride_toll_fare_includes = a

toll_included :: Proxy "toll_included"
toll_included = a

trip_time :: Proxy "trip_time"
trip_time = a

earnings_per_km :: Proxy "earnings_per_km"
earnings_per_km = a

optional_document :: Proxy "optional_document"
optional_document = a

earnings_per_km_desc_1 :: Proxy "earnings_per_km_desc_1"
earnings_per_km_desc_1 = a

earnings_per_km_desc_2 :: Proxy "earnings_per_km_desc_2"
earnings_per_km_desc_2 = a

earnings_p_km :: Proxy "earnings_p_km"
earnings_p_km = a

is_your_car_ac_working :: Proxy "is_your_car_ac_working"
is_your_car_ac_working = a

how_does_ac_condition_affect :: Proxy "how_does_ac_condition_affect"
how_does_ac_condition_affect = a

we_will_use_this_info :: Proxy "we_will_use_this_info"
we_will_use_this_info = a

you_can_always_change_this_from_profile :: Proxy "you_can_always_change_this_from_profile"
you_can_always_change_this_from_profile = a

is_your_car_ac_turned_on_and_working :: Proxy "is_your_car_ac_turned_on_and_working"
is_your_car_ac_turned_on_and_working = a

set_the_ac_on_to_enable :: Proxy "set_the_ac_on_to_enable"
set_the_ac_on_to_enable = a

variants_are_switched :: Proxy "variants_are_switched"
variants_are_switched = a

non_ac_are_switched :: Proxy "non_ac_are_switched"
non_ac_are_switched = a

network_error :: Proxy "network_error"
network_error = a

unknown_error :: Proxy "unknown_error"
unknown_error = a

connection_refused :: Proxy "connection_refused"
connection_refused = a

timeout :: Proxy "timeout"
timeout = a

server_error :: Proxy "server_error"
server_error = a

all_eligible_variants_are_chosen_please_check :: Proxy "all_eligible_variants_are_chosen_please_check"
all_eligible_variants_are_chosen_please_check = a

ride_more_and_earn_points :: Proxy "ride_more_and_earn_points"
ride_more_and_earn_points = a

ride_more_earn_more :: Proxy "ride_more_earn_more"
ride_more_earn_more = a

take_more_rides_to_earn_more_points_and_convert_it_to_subscription_discounts :: Proxy "take_more_rides_to_earn_more_points_and_convert_it_to_subscription_discounts"
take_more_rides_to_earn_more_points_and_convert_it_to_subscription_discounts = a

check_yatri_points :: Proxy "check_yatri_points"
check_yatri_points = a

two_more_rides_to_go :: Proxy "two_more_rides_to_go"
two_more_rides_to_go = a

one_more_ride_to_go :: Proxy "one_more_ride_to_go"
one_more_ride_to_go = a

take_one_more_ride_to_earn_points :: Proxy "take_one_more_ride_to_earn_points"
take_one_more_ride_to_earn_points = a

take_two_more_rides_to_earn_points :: Proxy "take_two_more_rides_to_earn_points"
take_two_more_rides_to_earn_points = a

congratulations :: Proxy "congratulations"
congratulations = a

you_have_earned_points_for_completing_eight_rides :: Proxy "you_have_earned_points_for_completing_eight_rides"
you_have_earned_points_for_completing_eight_rides = a

refer_namma_yatri_app_to_customers_and_earn_points :: Proxy "refer_namma_yatri_app_to_customers_and_earn_points"
refer_namma_yatri_app_to_customers_and_earn_points = a

refer_now :: Proxy "refer_now"
refer_now = a

convert_your_points_to_discount :: Proxy "convert_your_points_to_discount"
convert_your_points_to_discount = a

convert_your_points_to_get_discount_on_your_subscription :: Proxy "convert_your_points_to_get_discount_on_your_subscription"
convert_your_points_to_get_discount_on_your_subscription = a

convert_now :: Proxy "convert_now"
convert_now = a

more_rides :: Proxy "more_rides"
more_rides = a

sort_by :: Proxy "sort_by"
sort_by = a

accept :: Proxy "accept"
accept = a

pass :: Proxy "pass"
pass = a

term_1a :: Proxy "term_1a"
term_1a = a

term_2a :: Proxy "term_2a"
term_2a = a

term_3a :: Proxy "term_3a"
term_3a = a

term_1b :: Proxy "term_1b"
term_1b = a

term_2b :: Proxy "term_2b"
term_2b = a

term_3b :: Proxy "term_3b"
term_3b = a

excluded_charges :: Proxy "excluded_charges"
excluded_charges = a

tolls :: Proxy "tolls"
tolls = a

state_permit :: Proxy "state_permit"
state_permit = a

excluded_footer :: Proxy "excluded_footer"
excluded_footer = a

included_charges :: Proxy "included_charges"
included_charges = a

inc_1 :: Proxy "inc_1"
inc_1 = a

inc_2a :: Proxy "inc_2a"
inc_2a = a

inc_2b :: Proxy "inc_2b"
inc_2b = a

pickup_drop :: Proxy "pickup_drop"
pickup_drop = a

toll_charges_included :: Proxy "toll_charges_included"
toll_charges_included = a

please_do_not_demand_extra :: Proxy "please_do_not_demand_extra"
please_do_not_demand_extra = a

final_fare_excludes_toll :: Proxy "final_fare_excludes_toll"
final_fare_excludes_toll = a

please_collect_separately :: Proxy "please_collect_separately"
please_collect_separately = a

toll_charges_maybe_applicable :: Proxy "toll_charges_maybe_applicable"
toll_charges_maybe_applicable = a

you_are_all_set_to_take_rides :: Proxy "you_are_all_set_to_take_rides"
you_are_all_set_to_take_rides = a

top_ac_driver :: Proxy "top_ac_driver"
top_ac_driver = a

go_to_advanced_ride :: Proxy "go_to_advanced_ride"
go_to_advanced_ride = a

get_advanced_ride :: Proxy "get_advanced_ride"
get_advanced_ride = a

advanced_ride_popup_title :: Proxy "advanced_ride_popup_title"
advanced_ride_popup_title = a

advance :: Proxy "advance"
advance = a

current_button_text :: Proxy "current_button_text"
current_button_text = a

advance_booking :: Proxy "advance_booking"
advance_booking = a

feature_update :: Proxy "feature_update"
feature_update = a

third_party_booking :: Proxy "third_party_booking"
third_party_booking = a

some_feature_are_not_available_with_this_provider :: Proxy "some_feature_are_not_available_with_this_provider"
some_feature_are_not_available_with_this_provider = a

guaranteed_ride :: Proxy "guaranteed_ride"
guaranteed_ride = a

customer_calling_and_messaging :: Proxy "customer_calling_and_messaging"
customer_calling_and_messaging = a

waiting_charges :: Proxy "waiting_charges"
waiting_charges = a

customer_tips :: Proxy "customer_tips"
customer_tips = a

cancellation_charges :: Proxy "cancellation_charges"
cancellation_charges = a

merchant_points :: Proxy "merchant_points"
merchant_points = a

merchant_name :: Proxy "merchant_name"
merchant_name = a

third_party_rides :: Proxy "third_party_rides"
third_party_rides = a

third_party_rides_are_requested_with_by_a_users_from_another_app :: Proxy "third_party_rides_are_requested_with_by_a_users_from_another_app"
third_party_rides_are_requested_with_by_a_users_from_another_app = a

some_features_may_not_be_available :: Proxy "some_features_may_not_be_available"
some_features_may_not_be_available = a

why :: Proxy "why"
why = a

some_features_are_not_available_for_third_party_rides :: Proxy "some_features_are_not_available_for_third_party_rides"
some_features_are_not_available_for_third_party_rides = a

booking_from :: Proxy "booking_from"
booking_from = a

pick_up :: Proxy "pick_up"
pick_up = a

rate_card :: Proxy "rate_card"
rate_card = a

toll_charges :: Proxy "toll_charges"
toll_charges = a

toll_charges_desc :: Proxy "toll_charges_desc"
toll_charges_desc = a

parking_charge :: Proxy "parking_charge"
parking_charge = a

fare_for :: Proxy "fare_for"
fare_for = a

waiting_charge_limit :: Proxy "waiting_charge_limit"
waiting_charge_limit = a

parking_charges_desc :: Proxy "parking_charges_desc"
parking_charges_desc = a

tip_can_be_added :: Proxy "tip_can_be_added"
tip_can_be_added = a

day_time_charges :: Proxy "day_time_charges"
day_time_charges = a

congestion_charges_desc :: Proxy "congestion_charges_desc"
congestion_charges_desc = a

toll_or_parking_charges :: Proxy "toll_or_parking_charges"
toll_or_parking_charges = a

toll_charges_estimated :: Proxy "toll_charges_estimated"
toll_charges_estimated = a

congestion_charges :: Proxy "congestion_charges"
congestion_charges = a

pickup_charge :: Proxy "pickup_charge"
pickup_charge = a

night_time_charges :: Proxy "night_time_charges"
night_time_charges = a

min_fare_upto :: Proxy "min_fare_upto"
min_fare_upto = a

more_than :: Proxy "more_than"
more_than = a

rate_above_min_fare :: Proxy "rate_above_min_fare"
rate_above_min_fare = a

driver_pickup_charges :: Proxy "driver_pickup_charges"
driver_pickup_charges = a

daytime_charges_applicable_at_night :: Proxy "daytime_charges_applicable_at_night"
daytime_charges_applicable_at_night = a

daytime_charges_applied_at_night :: Proxy "daytime_charges_applied_at_night"
daytime_charges_applied_at_night = a

total_fare_may_change_due_to_change_in_route :: Proxy "total_fare_may_change_due_to_change_in_route"
total_fare_may_change_due_to_change_in_route = a

driver_additions :: Proxy "driver_additions"
driver_additions = a

fare_update_policy :: Proxy "fare_update_policy"
fare_update_policy = a

driver_additions_optional :: Proxy "driver_additions_optional"
driver_additions_optional = a

the_driver_may_quote_extra_to_cover_for_traffic :: Proxy "the_driver_may_quote_extra_to_cover_for_traffic"
the_driver_may_quote_extra_to_cover_for_traffic = a

driver_may_not_charge_this_additional_fare :: Proxy "driver_may_not_charge_this_additional_fare"
driver_may_not_charge_this_additional_fare = a

highest_earning_peak_time :: Proxy "highest_earning_peak_time"
highest_earning_peak_time = a

choose_ride_dist :: Proxy "choose_ride_dist"
choose_ride_dist = a

rates_change_as_the_dist :: Proxy "rates_change_as_the_dist"
rates_change_as_the_dist = a

view_booking_pref :: Proxy "view_booking_pref"
view_booking_pref = a

limited_time_offer_until :: Proxy "limited_time_offer_until"
limited_time_offer_until = a

register_your_bike :: Proxy "register_your_bike"
register_your_bike = a

bike_taxi :: Proxy "bike_taxi"
bike_taxi = a

select_facilities :: Proxy "select_facilities"
select_facilities = a

first_aid_kit :: Proxy "first_aid_kit"
first_aid_kit = a

driver_acknowledge :: Proxy "driver_acknowledge"
driver_acknowledge = a

booking_preference :: Proxy "booking_preference"
booking_preference = a

inspection :: Proxy "inspection"
inspection = a

a_f :: Proxy "a_f"
a_f = a

by_proceeding_you_accept_full_responsibility :: Proxy "by_proceeding_you_accept_full_responsibility"
by_proceeding_you_accept_full_responsibility = a

a_c :: Proxy "a_c"
a_c = a

ambulance :: Proxy "ambulance"
ambulance = a

non_ac :: Proxy "non_ac"
non_ac = a

ac :: Proxy "ac"
ac = a

no_oxygen :: Proxy "no_oxygen"
no_oxygen = a

oxygen :: Proxy "oxygen"
oxygen = a

ventilator :: Proxy "ventilator"
ventilator = a

select_one :: Proxy "select_one"
select_one = a

first_ride_free :: Proxy "first_ride_free"
first_ride_free = a

first_rides_free :: Proxy "first_rides_free"
first_rides_free = a

additional_charges_will_be_applicable :: Proxy "additional_charges_will_be_applicable"
additional_charges_will_be_applicable = a

referral_first_ride_description :: Proxy "referral_first_ride_description"
referral_first_ride_description = a

steps :: Proxy "steps"
steps = a

customer_completed_first_ride :: Proxy "customer_completed_first_ride"
customer_completed_first_ride = a

verifying :: Proxy "verifying"
verifying = a

processing :: Proxy "processing"
processing = a

payment_credited :: Proxy "payment_credited"
payment_credited = a

referral_bonus :: Proxy "referral_bonus"
referral_bonus = a

referral_bonus_tracker :: Proxy "referral_bonus_tracker"
referral_bonus_tracker = a

upi_details :: Proxy "upi_details"
upi_details = a

customer_referral_tracker :: Proxy "customer_referral_tracker"
customer_referral_tracker = a

pay_to_add_upi :: Proxy "pay_to_add_upi"
pay_to_add_upi = a

add_upi_to_receive_reward :: Proxy "add_upi_to_receive_reward"
add_upi_to_receive_reward = a

earn_for_each_referral :: Proxy "earn_for_each_referral"
earn_for_each_referral = a

start_referring_now :: Proxy "start_referring_now"
start_referring_now = a

will_get_referral_to_upi_id :: Proxy "will_get_referral_to_upi_id"
will_get_referral_to_upi_id = a

delete_upi_id :: Proxy "delete_upi_id"
delete_upi_id = a

confirm_delete_upi_id :: Proxy "confirm_delete_upi_id"
confirm_delete_upi_id = a

payout_history :: Proxy "payout_history"
payout_history = a

how_to_earn :: Proxy "how_to_earn"
how_to_earn = a

received :: Proxy "received"
received = a

credited_on :: Proxy "credited_on"
credited_on = a

referral_bonus_earned :: Proxy "referral_bonus_earned"
referral_bonus_earned = a

no_activated_referral :: Proxy "no_activated_referral"
no_activated_referral = a

no_active_referral_on_date :: Proxy "no_active_referral_on_date"
no_active_referral_on_date = a

payment_in_progress :: Proxy "payment_in_progress"
payment_in_progress = a

refresh_payment :: Proxy "refresh_payment"
refresh_payment = a

by :: Proxy "by"
by = a

customers :: Proxy "customers"
customers = a

customer :: Proxy "customer"
customer = a

rating :: Proxy "rating"
rating = a

cancellation :: Proxy "cancellation"
cancellation = a

i_speak :: Proxy "i_speak"
i_speak = a

with_nammayatri_for :: Proxy "with_nammayatri_for"
with_nammayatri_for = a

years :: Proxy "years"
years = a

vehicle_number :: Proxy "vehicle_number"
vehicle_number = a

what_people_say :: Proxy "what_people_say"
what_people_say = a

star_rating :: Proxy "star_rating"
star_rating = a

card_texts :: Proxy "card_texts"
card_texts = a

trainings_i_completed :: Proxy "trainings_i_completed"
trainings_i_completed = a

i_pledge :: Proxy "i_pledge"
i_pledge = a

only_5_more_rides_for_n_points :: Proxy "only_5_more_rides_for_n_points"
only_5_more_rides_for_n_points = a

only_3_more_rides_for_n_points :: Proxy "only_3_more_rides_for_n_points"
only_3_more_rides_for_n_points = a

only_4_more_rides_for_n_points :: Proxy "only_4_more_rides_for_n_points"
only_4_more_rides_for_n_points = a

you_got_n_points :: Proxy "you_got_n_points"
you_got_n_points = a

discounted :: Proxy "discounted"
discounted = a

yatri_points_faqs_ques1_ans4 :: Proxy "yatri_points_faqs_ques1_ans4"
yatri_points_faqs_ques1_ans4 = a

yatri_points_tnc :: Proxy "yatri_points_tnc"
yatri_points_tnc = a

yatri_points_faqs_ques2_ans3 :: Proxy "yatri_points_faqs_ques2_ans3"
yatri_points_faqs_ques2_ans3 = a

hotspots :: Proxy "hotspots"
hotspots = a

very_high :: Proxy "very_high"
very_high = a

high :: Proxy "high"
high = a

very_high_demand_area :: Proxy "very_high_demand_area"
very_high_demand_area = a

high_demand_area :: Proxy "high_demand_area"
high_demand_area = a

moderate :: Proxy "moderate"
moderate = a

average_demand_area :: Proxy "average_demand_area"
average_demand_area = a

this_area_is_experiencing_average_searches :: Proxy "this_area_is_experiencing_average_searches"
this_area_is_experiencing_average_searches = a

this_area_is_experiencing_very_high_searches :: Proxy "this_area_is_experiencing_very_high_searches"
this_area_is_experiencing_very_high_searches = a

this_area_is_experiencing_high_searches :: Proxy "this_area_is_experiencing_high_searches"
this_area_is_experiencing_high_searches = a

navigate :: Proxy "navigate"
navigate = a 

hotspots_not_available_currently :: Proxy "hotspots_not_available_currently"
hotspots_not_available_currently = a

gst_with_percentage :: Proxy "gst_with_percentage"
gst_with_percentage = a

discount_points_upto :: Proxy "discount_points_upto"
discount_points_upto = a

cannot_detect_pan_card :: Proxy "cannot_detect_pan_card"
cannot_detect_pan_card = a

cannot_detect_aadhaar :: Proxy "cannot_detect_aadhaar"
cannot_detect_aadhaar = a

document_already_validated :: Proxy "document_already_validated"
document_already_validated = a

document_under_manual_review :: Proxy "document_under_manual_review"
document_under_manual_review = a

document_already_linked_to_another_driver :: Proxy "document_already_linked_to_another_driver"
document_already_linked_to_another_driver = a

pan_already_linked :: Proxy "pan_already_linked"
pan_already_linked = a

exited_by_user :: Proxy "exited_by_user"
exited_by_user = a

app_update :: Proxy "app_update"
app_update = a

app_update_message :: Proxy "app_update_message"
app_update_message = a

aadhaar_front_not_detected :: Proxy "aadhaar_front_not_detected"
aadhaar_front_not_detected = a

aadhaar_back_not_detected :: Proxy "aadhaar_back_not_detected"
aadhaar_back_not_detected = a

unable_to_extract_name :: Proxy "unable_to_extract_name"
unable_to_extract_name = a

unable_to_extract_dob :: Proxy "unable_to_extract_dob"
unable_to_extract_dob = a

unable_to_extract_id :: Proxy "unable_to_extract_id"
unable_to_extract_id = a

image_b_w :: Proxy "image_b_w"
image_b_w = a

partial_doc_detected :: Proxy "partial_doc_detected"
partial_doc_detected = a

doc_is_blurred :: Proxy "doc_is_blurred"
doc_is_blurred = a

face_match_failed :: Proxy "face_match_failed"
face_match_failed = a

pan_not_detected :: Proxy "pan_not_detected"
pan_not_detected = a

unable_to_verify_selfie :: Proxy "unable_to_verify_selfie"
unable_to_verify_selfie = a

blurred_selfie :: Proxy "blurred_selfie"
blurred_selfie = a

eyes_closed_selfie :: Proxy "eyes_closed_selfie"
eyes_closed_selfie = a

multiple_faces_in_selfie :: Proxy "multiple_faces_in_selfie"
multiple_faces_in_selfie = a

face_blocked :: Proxy "face_blocked"
face_blocked = a

remove_eyewere :: Proxy "remove_eyewere"
remove_eyewere = a

image_validation_exceed_limit :: Proxy "image_validation_exceed_limit"
image_validation_exceed_limit = a

parking_charges_included :: Proxy "parking_charges_included"
parking_charges_included = a

invoice_generated_from_driver_to_rider :: Proxy "invoice_generated_from_driver_to_rider"
invoice_generated_from_driver_to_rider = a

included :: Proxy "included"
included = a

db_check_and_name_match_failed :: Proxy "db_check_and_name_match_failed"
db_check_and_name_match_failed = a

complete_your_profile :: Proxy "complete_your_profile"
complete_your_profile = a

add_photos :: Proxy "add_photos"
add_photos = a

add_upto_four :: Proxy "add_upto_four"
add_upto_four = a

card_text :: Proxy "card_text"
card_text = a

pledge :: Proxy "pledge"
pledge = a

safe_journey :: Proxy "safe_journey"
safe_journey = a

clean_car :: Proxy "clean_car"
clean_car = a

on_time_pick_up :: Proxy "on_time_pick_up"
on_time_pick_up = a

maintenance :: Proxy "maintenance"
maintenance = a

vehicle_offer :: Proxy "vehicle_offer"
vehicle_offer = a

gas :: Proxy "gas"
gas = a

radio :: Proxy "radio"
radio = a

eco_friendly :: Proxy "eco_friendly"
eco_friendly = a

device_charging :: Proxy "device_charging"
device_charging = a

boot_space :: Proxy "boot_space"
boot_space = a

pet_friendly :: Proxy "pet_friendly"
pet_friendly = a

hometown :: Proxy "hometown"
hometown = a

why_ny :: Proxy "why_ny"
why_ny = a

new_home :: Proxy "new_home"
new_home = a

kid_education :: Proxy "kid_education"
kid_education = a

new_vehicle :: Proxy "new_vehicle"
new_vehicle = a

add_your_photos :: Proxy "add_your_photos"
add_your_photos = a

add_photo_caption :: Proxy "add_photo_caption"
add_photo_caption = a

complete_profile :: Proxy "complete_profile"
complete_profile = a

complete_profile_msg :: Proxy "complete_profile_msg"
complete_profile_msg = a

edit_profile :: Proxy "edit_profile"
edit_profile = a

save :: Proxy "save"
save = a

manage_vehicle :: Proxy "manage_vehicle"
manage_vehicle = a

is_not_supported_yet :: Proxy "is_not_supported_yet"
is_not_supported_yet = a

we_will_nofity_you_when_it_is_available :: Proxy "we_will_nofity_you_when_it_is_available"
we_will_nofity_you_when_it_is_available = a

add_upi_to_receive_referral_reward :: Proxy "add_upi_to_receive_referral_reward"
add_upi_to_receive_referral_reward = a

do_you_want_to_receive_amount_here :: Proxy "do_you_want_to_receive_amount_here"
do_you_want_to_receive_amount_here = a

yes_pay_to_this_account :: Proxy "yes_pay_to_this_account"
yes_pay_to_this_account = a

i_will_add_different_account :: Proxy "i_will_add_different_account"
i_will_add_different_account = a

add_now :: Proxy "add_now"
add_now = a

recording_audio :: Proxy "recording_audio"
recording_audio = a

recorded_audio :: Proxy "recorded_audio"
recorded_audio = a

share_with_safety_team :: Proxy "share_with_safety_team"
share_with_safety_team = a

record_audio :: Proxy "record_audio"
record_audio = a

cannot_enable_go_home_for_different_city :: Proxy "cannot_enable_go_home_for_different_city"
cannot_enable_go_home_for_different_city = a

ride_cancellation_rate :: Proxy "ride_cancellation_rate"
ride_cancellation_rate = a

cancellation_rate_trivia :: Proxy "cancellation_rate_trivia"
cancellation_rate_trivia = a

high_cancellation_rate :: Proxy "high_cancellation_rate"
high_cancellation_rate = a

last_n_days :: Proxy "last_n_days"
last_n_days = a

cancellation_rate_trivia_2 :: Proxy "cancellation_rate_trivia_2"
cancellation_rate_trivia_2 = a

lifetime_stats :: Proxy "lifetime_stats"
lifetime_stats = a

total_rides_cancelled :: Proxy "total_rides_cancelled"
total_rides_cancelled = a

rental_ride :: Proxy "rental_ride"
rental_ride = a

total_earnings_missed :: Proxy "total_earnings_missed"
total_earnings_missed = a

more_about_me :: Proxy "more_about_me"
more_about_me = a

driving_since :: Proxy "driving_since"
driving_since = a

error_occured_try_again :: Proxy "error_occured_try_again"
error_occured_try_again = a

there_might_be_multiple_stops_in_this_rental_ride :: Proxy "there_might_be_multiple_stops_in_this_rental_ride"
there_might_be_multiple_stops_in_this_rental_ride = a

rental_ride_accepted :: Proxy "rental_ride_accepted"
rental_ride_accepted = a

my_referral_bonus :: Proxy "my_referral_bonus"
my_referral_bonus = a

add_upi_id :: Proxy "add_upi_id"
add_upi_id = a

linked_upi_id :: Proxy "linked_upi_id"
linked_upi_id = a

to_get_money :: Proxy "to_get_money"
to_get_money = a

till :: Proxy "till"
till = a

referral_bonus_will_be_credited_to_bank :: Proxy "referral_bonus_will_be_credited_to_bank"
referral_bonus_will_be_credited_to_bank = a

expert_driving :: Proxy "expert_driving"
expert_driving = a

clean_vehicle :: Proxy "clean_vehicle"
clean_vehicle = a

skilled_navigator :: Proxy "skilled_navigator"
skilled_navigator = a

safe_ride :: Proxy "safe_ride"
safe_ride = a

polite_driver :: Proxy "polite_driver"
polite_driver = a

on_time :: Proxy "on_time"
on_time = a

ac_not_turned_on :: Proxy "ac_not_turned_on"
ac_not_turned_on = a

late_pick_up_arrival :: Proxy "late_pick_up_arrival"
late_pick_up_arrival = a

asked_for_more_fare :: Proxy "asked_for_more_fare"
asked_for_more_fare = a

unhygienic_vehicle :: Proxy "unhygienic_vehicle"
unhygienic_vehicle = a

rash_driving :: Proxy "rash_driving"
rash_driving = a

rude_driver :: Proxy "rude_driver"
rude_driver = a

training :: Proxy "training"
training = a

financial :: Proxy "financial"
financial = a

safety :: Proxy "safety"
safety = a

kids_education :: Proxy "kids_education"
kids_education = a

buy_new_vehicle :: Proxy "buy_new_vehicle"
buy_new_vehicle = a

not_available :: Proxy "not_available"
not_available = a

please_write_something :: Proxy "please_write_something"
please_write_something = a

buy_new_home :: Proxy "buy_new_home"
buy_new_home = a

favourites :: Proxy "favourites"
favourites = a

points_earned_ :: Proxy "points_earned_"
points_earned_ = a

for_metro_pickup_ride :: Proxy "for_metro_pickup_ride"
for_metro_pickup_ride = a

for_metro_drop_ride :: Proxy "for_metro_drop_ride"
for_metro_drop_ride = a

continue_with :: Proxy "continue_with"
continue_with = a

contact_support_for_help :: Proxy "contact_support_for_help"
contact_support_for_help = a

you_have_switched_city_or_vehicle :: Proxy "you_have_switched_city_or_vehicle"
you_have_switched_city_or_vehicle = a

xl_plus :: Proxy "xl_plus"
xl_plus = a

ride_requests :: Proxy "ride_requests"
ride_requests = a

scheduled_ride_accepted :: Proxy "scheduled_ride_accepted"
scheduled_ride_accepted = a

you_can_access_scheduled_rides :: Proxy "you_can_access_scheduled_rides"
you_can_access_scheduled_rides = a

from_your_homescreen :: Proxy "from_your_homescreen"
from_your_homescreen = a

currently_there_are_no_rides_available :: Proxy "currently_there_are_no_rides_available"
currently_there_are_no_rides_available = a

due_to_higher_cancellation_rate_you_are_blocked :: Proxy "due_to_higher_cancellation_rate_you_are_blocked"
due_to_higher_cancellation_rate_you_are_blocked = a

blocked_till :: Proxy "blocked_till"
blocked_till = a

cancel_booking :: Proxy "cancel_booking"
cancel_booking = a

go_to_pickup :: Proxy "go_to_pickup"
go_to_pickup = a

ride_scheduled :: Proxy "ride_scheduled"
ride_scheduled = a

please_be_online :: Proxy "please_be_online"
please_be_online = a

before_the_ride_starts :: Proxy "before_the_ride_starts"
before_the_ride_starts = a

trip_will_be_assigned_to_another_driver :: Proxy "trip_will_be_assigned_to_another_driver"
trip_will_be_assigned_to_another_driver = a

ride_summary :: Proxy "ride_summary"
ride_summary = a

ride_assigned_to_another_driver :: Proxy "ride_assigned_to_another_driver"
ride_assigned_to_another_driver = a

you_can_see_other_available_ride_in_more_rides_section :: Proxy "you_can_see_other_available_ride_in_more_rides_section"
you_can_see_other_available_ride_in_more_rides_section = a

round_trip :: Proxy "round_trip"
round_trip = a

upcoming :: Proxy "upcoming"
upcoming = a

follow_instructions_to_avoid_reassignment_of_ride :: Proxy "follow_instructions_to_avoid_reassignment_of_ride"
follow_instructions_to_avoid_reassignment_of_ride = a

be_within_10km_of_pickup :: Proxy "be_within_10km_of_pickup"
be_within_10km_of_pickup = a

please_collect_parking_charges :: Proxy "please_collect_parking_charges"
please_collect_parking_charges = a

incurred_during_trip :: Proxy "incurred_during_trip"
incurred_during_trip = a

back :: Proxy "back"
back = a

your_ride_starts_in :: Proxy "your_ride_starts_in"
your_ride_starts_in = a

away :: Proxy "away"
away = a

intercity :: Proxy "intercity"
intercity = a

local :: Proxy "local"
local = a

intercity_return :: Proxy "intercity_return"
intercity_return = a

rental :: Proxy "rental"
rental = a

regular :: Proxy "regular"
regular = a

upcoming_ride :: Proxy "upcoming_ride"
upcoming_ride = a

all :: Proxy "all"
all = a

tomorrow :: Proxy "tomorrow"
tomorrow = a

we_are_not_able_to_fetch_your_current_location :: Proxy "we_are_not_able_to_fetch_your_current_location"
we_are_not_able_to_fetch_your_current_location = a 

you_have_an_upcoming :: Proxy "you_have_an_upcoming"
you_have_an_upcoming = a

booking :: Proxy "booking"
booking = a

intercity_ride_accepted :: Proxy "intercity_ride_accepted"
intercity_ride_accepted = a

intercity_ride :: Proxy "intercity_ride"
intercity_ride = a

please_ensure_that_your_vehicle_is_ready_for_intercity_trip :: Proxy "please_ensure_that_your_vehicle_is_ready_for_intercity_trip"
please_ensure_that_your_vehicle_is_ready_for_intercity_trip = a

per_km_charge :: Proxy "per_km_charge"
per_km_charge = a

extra_time_charge :: Proxy "extra_time_charge"
extra_time_charge = a

added_at_end_of_trip :: Proxy "added_at_end_of_trip"
added_at_end_of_trip = a

driver_allowance :: Proxy "driver_allowance"
driver_allowance = a

add_on_km_charge :: Proxy "add_on_km_charge"
add_on_km_charge = a

extra_distance_charges :: Proxy "extra_distance_charges"
extra_distance_charges = a

base_charge :: Proxy "base_charge"
base_charge = a

the_customer_will_pay_post_scheduled_ride_start_time :: Proxy "the_customer_will_pay_post_scheduled_ride_start_time"
the_customer_will_pay_post_scheduled_ride_start_time = a

clean_auto :: Proxy "clean_auto"
clean_auto = a

clean_cab :: Proxy "clean_cab"
clean_cab = a

metro_ride_completed :: Proxy "metro_ride_completed"
metro_ride_completed = a

or_ride_is_cancelled_by_customer :: Proxy "or_ride_is_cancelled_by_customer"
or_ride_is_cancelled_by_customer = a

the_ride_starts :: Proxy "the_ride_starts"
the_ride_starts = a

good_services :: Proxy "good_services"
good_services = a

smooth_driving :: Proxy "smooth_driving"
smooth_driving = a

no_cancellation :: Proxy "no_cancellation"
no_cancellation = a

lets_get_started :: Proxy "lets_get_started"
lets_get_started = a

cab :: Proxy "cab"
cab = a

free_trial_ending_in_n_days :: Proxy "free_trial_ending_in_n_days"
free_trial_ending_in_n_days = a

n_free_rides_completed :: Proxy "n_free_rides_completed"
n_free_rides_completed = a

n_more_free_rides_left :: Proxy "n_more_free_rides_left"
n_more_free_rides_left = a

collect_cash_at_drop :: Proxy "collect_cash_at_drop"
collect_cash_at_drop = a

more_details :: Proxy "more_details"
more_details = a

take_photo_of_parcel :: Proxy "take_photo_of_parcel"
take_photo_of_parcel = a

sender_will_verify_parcel :: Proxy "sender_will_verify_parcel"
sender_will_verify_parcel = a

call_customer_text :: Proxy "call_customer_text"
call_customer_text = a

call_sender :: Proxy "call_sender"
call_sender = a

call_receiver :: Proxy "call_receiver"
call_receiver = a

start :: Proxy "start"
start = a

end :: Proxy "end"
end = a

delivery_bike_service_tier_desc :: Proxy "delivery_bike_service_tier_desc"
delivery_bike_service_tier_desc = a

rate_your_delivery_with :: Proxy "rate_your_delivery_with"
rate_your_delivery_with = a

delivery_details :: Proxy "delivery_details"
delivery_details = a

take_clear_picture_parcel :: Proxy "take_clear_picture_parcel"
take_clear_picture_parcel = a

ensure_adequate_light_parcel_desc :: Proxy "ensure_adequate_light_parcel_desc"
ensure_adequate_light_parcel_desc = a

fit_parcel_correctly :: Proxy "fit_parcel_correctly"
fit_parcel_correctly = a

correct_positioning :: Proxy "correct_positioning"
correct_positioning = a

incorrect_positioning :: Proxy "incorrect_positioning"
incorrect_positioning = a

upload_parcel_image :: Proxy "upload_parcel_image"
upload_parcel_image = a

pickup_instruction :: Proxy "pickup_instruction"
pickup_instruction = a

drop_instruction :: Proxy "drop_instruction"
drop_instruction = a

parcel_is_inappropriate :: Proxy "parcel_is_inappropriate"
parcel_is_inappropriate = a

sender_asking_different_location :: Proxy "sender_asking_different_location"
sender_asking_different_location = a

sender_unavailable_unreachable :: Proxy "sender_unavailable_unreachable"
sender_unavailable_unreachable = a

truck :: Proxy "truck"
truck = a

register_your_truck :: Proxy "register_your_truck"
register_your_truck = a

no_plan_selected :: Proxy "no_plan_selected"
no_plan_selected = a

a_new_way_to_earn_parcel :: Proxy "a_new_way_to_earn_parcel"
a_new_way_to_earn_parcel = a

seamless_earning_experience_click_below :: Proxy "seamless_earning_experience_click_below"
seamless_earning_experience_click_below = a

metro_warrior_mode :: Proxy "metro_warrior_mode"
metro_warrior_mode = Proxy

choose_metro_station :: Proxy "choose_metro_station"
choose_metro_station = Proxy

primary_metro_station :: Proxy "primary_metro_station"
primary_metro_station = Proxy

primary_station_info :: Proxy "primary_station_info"
primary_station_info = Proxy

nearby_stations :: Proxy "nearby_stations"
nearby_stations = Proxy

nearby_station_info :: Proxy "nearby_station_info"
nearby_station_info = Proxy

change :: Proxy "change"
change = Proxy

disable_metro_warriors_info :: Proxy "disable_metro_warriors_info"
disable_metro_warriors_info = Proxy

choose_preferred_metro :: Proxy "choose_preferred_metro"
choose_preferred_metro = Proxy

metro_warriors :: Proxy "metro_warriors"
metro_warriors = Proxy

search :: Proxy "search"
search = Proxy

bus__ :: Proxy "bus__"
bus__ = Proxy

driver_unsubscribed :: Proxy "driver_unsubscribed"
driver_unsubscribed = Proxy

drivers_are_permitted_to_cancel_ambulance_bookings :: Proxy "drivers_are_permitted_to_cancel_ambulance_bookings"
drivers_are_permitted_to_cancel_ambulance_bookings = Proxy

canceling_this_booking_may_affect_the_emergency_medical :: Proxy "canceling_this_booking_may_affect_the_emergency_medical"
canceling_this_booking_may_affect_the_emergency_medical = Proxy

payment_under_maintenance :: Proxy "payment_under_maintenance"
payment_under_maintenance = Proxy

payments_temporarily_unavailable :: Proxy "payments_temporarily_unavailable"
payments_temporarily_unavailable = Proxy

resume_ride :: Proxy "resume_ride"
resume_ride = a

end_ride_with_stops :: Proxy "end_ride_with_stops"
end_ride_with_stops = a

stop :: Proxy "stop"
stop = a 
