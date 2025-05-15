module Resources.LocalizableV2.Types where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Record.Unsafe (unsafeGet)


newtype Languages= Languages{ 
  english :: Keymap
, hindi :: Keymap
, malayalam :: Keymap
, bengali :: Keymap
, tamil :: Keymap
, kannada :: Keymap
, telugu :: Keymap
}


newtype Keymap = Keymap {
  about :: String
, safety :: String
, safety_check_in :: String
, about_app_description :: String
, about_referral_program :: String
, about_referral_program_discription :: String -> String
, account_deletion_confirmation :: String
, add_another_contact :: String
, add_emergency_contacts :: String
, add_favourite :: String
, add_new_address :: String
, add_new_favourite :: String
, add_now :: String
, add_saved_location_from_settings :: String
, add_tag :: String
, address :: String
, addresses :: String
, all_favourites :: String
, all_topics :: String
, already_exists :: String
, also_share_your_ride_status_and_location :: String
, amount_paid :: String
, anonymous_call :: String
, are_you_staring :: String
, are_you_sure_you_want_to_cancel :: String
, are_you_sure_you_want_to_logout :: String
, are_you_sure_you_want_to_remove_contact :: String
, are_you_sure_you_want_to_remove_favourite_ :: String
, ask_for_price :: String
, ask_for_price_info :: String
, asked_for_more_money :: String
, at_drop :: String
, at_pickup :: String
, auto_accepting_selected_ride :: String
, auto_assign_a_ride :: String
, auto_assign_driver :: String
, away :: String
, away_c :: String
, base_fares :: String
, board_the_first :: String
, book_now :: String
, book_ride_ :: String
, booking_preference :: String
, boost_your_ride_chances_and_help_drivers_with_tips :: String
, by_cash :: String
, by_tapping_continue :: String
, call :: String
, call_driver :: String
, call_driver_using :: String
, call_emergency_contacts :: String
, call_emergency_centre :: String
, cancel_ :: String
, cancel_auto_assigning :: String
, cancel_ongoing_search :: String
, cancel_ride :: String
, request_edit :: String
, cancel_search :: String
, cancel_str :: String
, cancelled :: String
, change :: String
, change_drop_location :: String
, change_location :: String
, check_out_live_stats :: String
, check_your_internet_connection_and_try_again :: String
, choose_a_ride_as_per_your_comfort :: String
, choose_between_multiple_drivers :: String
, choose_between_multiple_rides :: String
, choose_on_map :: String
, choose_your_ride :: String
, comfy :: String
, confirm_and_book :: String
, confirm_and_save :: String
, confirm_changes :: String
, confirm_drop_location :: String
, check_revised_fare_and_route :: String
, confirm_emergency_contacts :: String
, confirm_for :: String
, confirm_location :: String
, confirm_pickup_location :: String
, confirm_ride_ :: String
, confirming_the_ride_for_you :: String
, contact_support :: String
, contacts_selected :: String
, continue :: String
, copied :: String
, could_not_connect_to_driver :: String
, current_location :: String
, fare_updated :: String
, please_confirm_with_your_after_requesting :: String
, previously :: String
, route_and_fare_updated :: String
, previous_fare :: String
, your_driver_might_want_to_go_towards_the_current_drop_kindly_ask_them_to_confirm_after_requesting :: String
, currently_we_are_live_in_ :: String -> String
, customer_selected_fare :: String
, customer_tip_description :: String
, dial_112 :: String
, details :: String
, data_collection_authority :: String
, day_time_charges :: String -> String -> String
, daytime_charges_applicable_at_night :: String -> String -> String
, daytime_charges_applied_at_night :: String -> String -> String -> String
, del_account :: String
, delete :: String
, deny_access :: String
, describe_your_issue :: String
, destination_outside_limits :: String
, direct_call :: String
, do_you_need_emergency_help :: String
, download_pdf :: String
, drag_the_map :: String
, driver_pickup_charges :: String -> String
, driver_requested_to_cancel :: String
, driver_was_not_reachable :: String
, driver_was_rude :: String
, drivers_can_charge_an_additional_fare_upto :: String
, drivers_can_charge_between_the_above_range :: String
, drivers_may_quote_extra_to_cover_for_traffic :: String
, driver_addition_limits_are_in_increments :: String
, drop :: String
, drop_location_far_away :: String
, early_end_ride_charges :: String
, early_end_ride_charges_description :: String
, economical :: String
, edit :: String
, edit_favourite :: String
, email :: String
, email_already_exists :: String
, email_id :: String
, trusted_contacs_added_successfully :: String
, emergency_contacts :: String
, edit_emergency_contacts :: String
, emergency_help :: String
, empty_rides :: String
, enable_this_feature_to_choose_your_ride :: String
, enjoy_riding_with_us :: String
, enter_4_digit_otp :: String
, enter_a_location :: String
, enter_mobile_number :: String
, enter_otp :: String
, enter_your_mobile_number :: String
, enter_your_name :: String
, error_404 :: String
, error_occured_try_again :: String
, estimates_changed :: String
, estimates_revised_to :: String
, eta_was_too_long :: String
, eta_was_too_short :: String
, exists_as :: String
, expires_in :: String
, faq :: String
, fare_has_been_updated :: String
, fare_was_high :: String
, favourite :: String
, favourite_added_successfully :: String
, favourite_location :: String
, favourite_removed_successfully :: String
, favourite_updated_successfully :: String
, favourite_your_current_location :: String
, favourites :: String
, female :: String
, finding_rides_near_you :: String
, for_other_issues_write_to_us :: String
, full_name :: String
, gender_str :: String
, get_estimate_fare :: String
, getting_delayed_please_wait :: String
, getting_estimates_for_you :: String
, getting_revised_estimate :: String
, getting_started_and_faqs :: String
, give_this_location_a_name :: String
, go_back_ :: String
, go_home_ :: String
, go_to_home__ :: String
, google_map_ :: String
, got_another_ride_else_where :: String
, got_it :: String
, got_it_tell_us_more :: String
, government_chagres :: String
, grant_access :: String
, cgst :: String
, have_referral_code :: String
, hatchback :: String
, help_and_support :: String
, help_us_with_your_feedback_optional :: String
, help_us_with_your_reason :: String
, hey :: String
, home :: String
, hope_your_ride_was_hassle_free :: String
, how_do_you_identify_yourself :: String
, how_should_we_address_you :: String
, how_the_pricing_works :: String
, how_this_works :: String
, how_was_your_ride_experience :: String
, how_was_your_ride_with :: String
, actual_fare_was_higher_than_what_was_shown :: String
, i_am_on_my_way :: String
, i_have_arrived :: String
, if_you_still_wanna_book_ride_click_continue_and_start_booking_the_ride :: String
, in' :: String
, in_app_tracking :: String
, invalid_code_please_re_enter :: String
, invalid_mobile_number :: String
, invoice :: String
, is_on_the_way :: String
, is_waiting_at_pickup :: String
, it_seems_to_be_a_very_busy_day :: String
, language :: String
, let_try_that_again :: String
, live_stats_dashboard :: String
, load_more :: String
, loading :: String
, location :: String
, location_already :: String
, location_already_exists :: String
, location_already_exists_as :: String
, location_unserviceable :: String
, login_using_the_otp_sent_to :: String
, logo :: String
, logout_ :: String
, looking_for_you_at_pickup :: String
, lost_something :: String
, male :: String
, mandatory :: String
, max_char_limit_reached :: String
, maybe_later :: String
, message :: String
, meters_away_from_your_destination :: String
, min_fare_upto :: String -> String
, more_than :: String
, mins_away :: String
, mobile :: String
, mobile_number_str :: String
, my_rides :: String
, name :: String
, name_already_in_use :: String
, navigate :: String
, nearby :: String
, night_time_charges :: String -> String -> String
, no :: String
, no_contacts_left_on_device_to_add :: String
, no_dont :: String
, no_emergency_contacts_set :: String
, no_favourites_saved_yet :: String
, no_more_rides :: String
, no_tip :: String
, nominal_fare :: String
, customer_cancellation_dues :: String
, not_now :: String
, note :: String
, notify_me :: String
, of' :: String
, ok_i_will_wait :: String
, online_ :: String
, other :: String
, others :: String
, otp :: String
, our_suggested_price_for_this_trip_is :: String
, paid :: String
, pay_directly_to_your_driver_using_cash_upi :: String
, pay_driver_using_cash_or_upi :: String
, pay_driver_using_cash_or_upi_ :: String
, pay_the_driver :: String
, pay_the_driver_info :: String
, pay_the_driver_note :: String
, pay_via_cash_or_upi :: String
, payment_method :: String
, payment_method_string :: String
, payment_method_string_ :: String
, people :: String
, percentage_of_nominal_fare :: String
, personal_details :: String
, pick_up_location :: String
, pick_up_location_incorrect :: String
, pickup_and_drop :: String
, pickup_charge :: String
, place_call :: String
, please_choose_your_preferred_language_to_continue :: String
, please_come_fast_i_am_waiting :: String
, please_come_soon :: String
, please_pay_the_final_amount_to_the_driver_via_cash :: String
, please_tell_us_why_you_want_to_cancel :: String
, please_update_app_to_continue_service :: String
, please_wait_i_will_be_there :: String
, please_wait_while_in_progress :: String
, prefer_not_to_say :: String
, privacy_policy :: String
, problem_at_our_end :: String
, profile_completion :: String
, promotion :: String
, quote_expired :: String
, rate_above_min_fare :: String
, rate_card :: String
, rate_your_driver :: String
, rate_your_ride :: String
, rate_your_ride_with :: String
, refereal_code_discription :: String
, referral_code_applied :: String
, referral_code_successfull :: String
, register_using_different_number :: String
, remove :: String
, remove_favourite :: String
, repeat_ride :: String
, report_an_issue :: String
, report_an_issue_with_this_trip :: String
, request_auto_ride :: String -> String
, request_callback :: String
, request_ride :: String
, request_submitted :: String
, request_to_delete_account :: String
, resend :: String
, ride_completed :: String
, ride_details :: String
, ride_fare :: String
, ride_id :: String
, ride_not_serviceable :: String
, app_not_serviceable :: String
, save :: String
, save_as :: String
, save_place :: String
, saved_address_helps_you_keep_your_favourite_places_handy :: String
, saved_addresses :: String
, search_again_with :: String
, search_again_with_a_tip :: String
, search_again_without_a_tip :: String
, search_contacts :: String
, select_a_ride :: String
, select_an_offer :: String
, select_an_offer_from_our_drivers :: String
, select_an_offer_from_our_drivers_info :: String
, select_contacts :: String
, select_favourite :: String
, select_on_map :: String
, select_your_drop :: String
, select_your_gender :: String
, send_email :: String
, service_charges :: String
, set_location_on_map :: String
, set_now :: String
, set_up_your_account :: String
, share_app :: String
, share_ride_with_emergency_contacts :: String
, show_all_options :: String
, six_digit_referral_code :: String
, skip :: String
, software_license :: String
, sorry_we_couldnt_find_any_rides :: String
, sort_by :: String
, spacious :: String
, start_ :: String
, start_your_chat_using_these_quick_chat_suggestions :: String
, start_your_chat_with_the_driver :: String
, steps_to_complete :: String
, subject :: String
, submit :: String
, submit_feedback :: String
, successful_onboard :: String -> String
, support :: String
, suv :: String
, sedan :: String
, t_and_c_a :: String
, terms_and_conditions :: String
, thank_you_for_writing :: String
, thank_you_for_writing_to_us :: String
, thank_your_driver :: String
, the_trip_is_very_short_and_just_take :: String
, tip :: String
, to_the :: String
, total_amount :: String
, total_fare_may_change_due_to_change_in_route :: String
, total_paid :: String
, track_live_location_using :: String
, trip_charges :: String
, trip_details_ :: String
, trip_id :: String
, try_again :: String
, try_again_with :: String
, try_again_with_a_tip :: String
, try_again_without_tip :: String
, try_connecting_with_the_driver :: String
, try_looking_for_rides_again :: String
, unreachable_please_call_back :: String
, update :: String
, update_personal_details :: String
, setup_now :: String
, update_required :: String
, use_current_location :: String
, user :: String
, verifying_otp :: String
, view_all_rides :: String
, view_breakdown :: String
, view_details :: String
, view_invoice :: String
, visit_my_rides_section_for_ride_specific_complaints :: String
, wait_time :: String
, wait_time_too_long :: String
, waiting_charge :: String
, waiting_charge_description :: String
, waiting_charge_ratecard_description :: String -> String -> String
, waiting_charge_info :: String -> String -> String
, we_have_received_your_issue :: String
, we_have_received_your_issue_well_reach_out_to_you_in_sometime :: String
, we_need_access_to_your_location :: String
, we_will_delete_your_account :: String
, welcome_text :: String
, where_to :: String
, work :: String
, write_a_comment :: String
, write_to_us :: String
, wrong_otp :: String
, yes :: String
, yes_cancel_search :: String
, yes_delete_it :: String
, yes_remove :: String
, yes_try_again :: String
, you_are_about_to_call_namma_yatri_support :: String -> String
, you_are_about_to_call_nearest_emergency_centre :: String
, you_are_offline :: String
, you_can_cancel_ride :: String
, you_can_describe_the_issue_you_faced_here :: String
, you_can_get_referral_code_from_driver :: String -> String
, you_can_take_a_walk_or_continue_with_ride_booking :: String
, you_have_ride_offers_are_you_sure_you_want_to_cancel :: String
, you_havent_taken_a_trip_yet :: String
, you_havent_taken_a_trip_yet_in_past_hours :: String -> String
, you_rated :: String
, you_will_be_asked_to_select_contacts :: String
, your_email_id :: String
, location_permission_subtitle :: String
, your_number_will_be_visible_to_the_driver_use_if_not_calling_from_registered_number :: String
, your_number_will_not_be_shown_to_the_driver_the_call_will_be_recorded_for_compliance :: String
, your_recent_ride :: String
, your_ride_has_started :: String
, your_ride_is_now_complete :: String
, your_rides :: String
, your_trip_is_too_short_you_are_just :: String
, download_invoice :: String
, was_your_call_successful :: String
, driver_additions :: String
, fare_update_policy :: String
, driver_additions_optional :: String
, the_driver_may_quote_extra_to_cover_for_traffic :: String
, driver_additions_are_calculated_at_rate :: String -> String
, driver_may_not_charge_this_additional_fare :: String
, you_may_see_an_updated_final_fare_due_to_any_of_the_below_reasons :: String
, reason_change_in_route_a :: String
, reason_change_in_route_b :: String
, go_to_zone :: String -> String
, request_received_we_will_call_you_back_soon :: String
, contact_removed_successfully :: String
, corporate_address :: String
, corporate_address_description :: String -> String
, corporate_address_description_additional :: String -> String
, registered_address :: String
, registered_address_description :: String -> String
, registered_address_description_additional :: String -> String
, recommended :: String
, complete_your_profile_for_a_personalised_ride_experience :: String
, complete_your_namma_safety_setup_for_safe_ride_experience :: String
, update_now :: String
, we_would_appreciate_your_feedback :: String
, reason_for_deleting_account :: String
, submit_request :: String
, please_enter_a_valid_email :: String
, we_would_appreciate_your_reasoning :: String
, ok_got_it :: String
, wait_for_driver :: String
, no_longer_require_a_ride_due_to_change_in_plans :: String
, cancelling_as_i_got_a_ride_on_another_app :: String
, driver_location_wasnt_changing_on_the_map :: String
, driver_was_taking_too_long_to_reach_the_pickup_location :: String
, the_pickup_location_entered_was_wrong :: String
, your_driver_is_just :: String
, m_away :: String
, driver_has_already_travelled :: String
, please_contact_the_driver_before_cancelling :: String
, confirm_with_your_driver :: String
, change_of_plans :: String
, driver_is_not_moving :: String
, wrong_pickup_location :: String
, different_vehicle_number :: String
, vehicle_number_is_different_from_what_is_shown_in_the_app :: String
, different_auto :: String
, different_cab :: String
, driver_might_be_taking_alternate_route :: String
, driver_is_not_moving_q :: String
, would_you_like_to_check_with_the_driver_before_cancelling :: String
, driver_is_near_your_location :: String
, some_other_reason :: String
, location_permission_subtitle_new_user :: String
, metro_ride :: String
, go_back_text :: String
, driver_preferred_your_special_request_and_is_just :: String
, driver_preferred_your_special_request :: String
, and_has_travelled :: String
, please_find_revised_fare_estimate :: String
, fare_estimate :: String
, tip_selected :: String
, add_a_tip_to_find_a_ride_quicker :: String
, it_seems_to_be_taking_longer_than_usual :: String
, continue_search_with :: String
, continuing_search_with :: String
, searching_with :: String
, the_driver_preferred_your_special_request_and_is_already_on_the_way_to_your_location :: String
, driver_is_already_on_the_way_to_your_location :: String
, allow_location_access :: String
, message_from_driver :: String
, reply :: String
, name_should_be_more_than_2_characters :: String
, this_field_is_required :: String
, email_exists_already :: String
, okay_got_it :: String
, call_namma_yatri_support :: String -> String
, call_112 :: String
, seats :: String
, otp_page_has_been_expired_please_request_otp_again :: String
, otp_entering_limit_exhausted_please_try_again_later :: String
, too_many_login_attempts_please_try_again_later :: String
, something_went_wrong_please_try_again :: String
, sorry_limit_exceeded_you_cant_add_any_more_favourites :: String
, it_seems_like_you_have_an_ongoing_ride_ :: String
, cancellation_unsuccessfull_please_try_again :: String
, no_driver_available_at_the_moment_please_try_again :: String
, otp_for_the_jatri_sathi_zone_has_been_expired_please_try_looking_again :: String -> String
, no_contacts_found_on_the_device_to_be_added :: String
, please_enable_contacts_permission_to_proceed :: String
, limit_reached_3_of_3_emergency_contacts_already_added :: String
, invalid_contact_format :: String
, otp_resent_limit_exhausted_please_try_again_later :: String
, rate_your_experience :: String
, report_issue_ :: String
, done :: String
, please_tell_us_what_went_wrong :: String
, your_feedback_helps_us :: String -> String
, did_you_face_any_issue :: String
, did_the_driver_offer_assistance :: String
, was_the_driver_understanding_of_your_needs :: String
, we_noticed_your_ride_ended_away :: String
, get_callback_from_us :: String
, driver_was_not_ready_to_go :: String
, asking_for_more_money :: String
, vehicle_broken :: String
, we_will_give_you_callback :: String
, your_issue_has_been_reported :: String
, issue_report_already_exists :: String
, otp_resent_successfully :: String
, description_should_be_more_than_10_alphabetic_characters :: String
, incorrect_otp_please_try_again :: String
, n_more_attempts_left :: String
, go_to_selected_pickup_spot :: String
, go_to_selected_pickup_spot_as_autos_are_restricted :: String
, unprofessional_driver :: String
, rash_driving :: String
, driver_charged_more :: String
, uncomfortable_auto :: String
, uncomfortable_cab :: String
, trip_got_delayed :: String
, felt_unsafe :: String
, polite_driver :: String
, expert_driving :: String
, safe_ride :: String
, clean_auto :: String
, clean_cab :: String
, on_time :: String
, skilled_navigator :: String
, rude_driver :: String
, too_many_calls :: String
, reckless_driving :: String
, late_drop_off :: String
, late_pick_up :: String
, poor_experience :: String
, terrible_experience :: String
, needs_improvement :: String
, amazing :: String
, almost_perfect :: String
, asked_for_extra_fare :: String
, anything_that_you_would_like_to_tell_us :: String
, platform_fee :: String
, finding_quotes_text :: String
, please_wait :: String
, pay_driver_using_wallet :: String
, faster :: String
, new_ :: String
, sgst :: String
, otp_expired :: String
, otp_expired_description :: String
, platform_gst :: String
, misc_waiting_charge :: String
, taxi_from_zone :: String -> String
, taxi :: String
, ac :: String
, non_ac :: String
, ac_taxi :: String
, non_ac_taxi :: String
, get_otp_via_whatsapp :: String
, or :: String
, helps_driver_confirm_its_you :: String
, lets_get_you_trip_ready :: String
, got_an_otp :: String
, just_one_last_thing :: String
, toll_charges_will_be_extra :: String
, auto_rickshaw :: String
, cabs_available :: String
, general_disability_description :: String
, pi_pointer_1 :: String
, pi_pointer_2 :: String
, vi_pointer_1 :: String
, vi_pointer_2 :: String
, hi_pointer_1 :: String
, hi_pointer_2 :: String
, accessibility_text :: String -> String
, to_cater_your_specific_needs :: String -> String
, special_assistance :: String
, select_the_condition_that_is_applicable :: String
, disability_claimer_text :: String
, are_you_a_person_with_disability :: String
, do_you_neeed_special_assistance :: String
, assistance_required :: String
, no_disability :: String
, learn_how_text :: String -> String
, update_profile :: String
, now_get_assisted_rides :: String
, sent_otp_via_sms :: String
, sent_otp_via_whatsapp :: String
, please_enable_location_permission :: String -> String
, enable_location_permission_to :: String
, ac_suv :: String
, ac_cab :: String
, ride_type :: String
, ernakulam_limit_charge :: String
, select_location_on_map :: String
, download_driver_receipt :: String
, view_driver_receipt :: String
, driver_receipt :: String
, help :: String
, fare_info_text :: String -> String
, educational_pop_up_slide_1_title :: String
, educational_pop_up_slide_2_title :: String
, educational_pop_up_slide_3_title :: String
, educational_pop_up_slide_4_title :: String
, educational_pop_up_slide_5_title :: String
, educational_pop_up_slide_1_subtitle :: String
, educational_pop_up_slide_2_subtitle :: String
, educational_pop_up_slide_3_subtitle :: String
, educational_pop_up_slide_4_subtitle :: String
, educational_pop_up_slide_5_subtitle :: String
, inclusive_and_accessible :: String
, you_seem_to_be_far_from_pick_up :: String
, are_you_sure_you_want_to_proceed_with_the_booking :: String
, my_tickets :: String
, something_went_wrong_try_again_later :: String
, you_can_book_tickets_to_the_zoo_by_clicking_the_button :: String
, charges_applicable_after_3_mins :: String
, waiting_at_pickup :: String
, reaching_your_destination_in_ :: String
, learn_more :: String
, pickup :: String
, pay_by_cash_or_upi :: String
, wait_timer :: String
, how_long_driver_waited_for_pickup :: String
, you_will_pay_for_every_minute :: String -> String -> String
, chat_with :: String
, quick :: String
, chats :: String
, replies :: String
, namma_safety :: String
, you_sent :: String
, message_your_driver :: String
, check_in_with_your_driver :: String
, check_in_with_your_em :: String -> String
, track_on_google_maps :: String
, otp_expire_timer :: String
, shows_for_how_long_your_otp_ :: String
, if_your_otp_expires_ :: String
, you_have_reached_destination :: String
, places_you_might_like_to_go_to :: String
, suggested_destination :: String
, recent_rides :: String
, one_click_booking_for_your_favourite_journeys :: String
, view_more :: String
, view_less :: String
, have_a_refferal :: String
, your_suggested_destinations_and_recent_rides_will_appear_here :: String
, welcome_to_namma_yatri_ :: String
, book_and_move :: String
, anywhere_in_the_city :: String
, checkout_our_live_stats :: String
, most_loved_app :: String -> String
, pickup_ :: String
, past_searches :: String
, search_results :: String
, edit_destination :: String
, requesting_ride_in :: String
, confirm_fare :: String
, request_change :: String
, requesting_ride :: String
, tap_here_to_stop_auto_requesting :: String
, powered_by :: String
, book_your_ride :: String
, start_typing_to_search_places :: String
, fare_updated_with_charges :: String
, fare_updated_with_shorter_dist :: String
, fare_updated_with_longer_dist :: String
, fare_updated_with_charges_shorter_dist :: String
, fare_updated_with_charges_longer_dist :: String
, did_you_have_a_safe_journey :: String
, trip_was_safe_and_worry_free :: String
, driver_behaved_inappropriately :: String
, i_did_not_feel_safe :: String
, looking_for_another_ride :: String
, the_ride_had_been_cancelled_we_are_finding_you_another :: String
, enjoy_the_ride :: String
, ride_started :: String
, discover_awesome_spots_tailored_just_for_you :: String
, smart :: String
, one_click :: String
, not_serviceable :: String
, we_are_not_live_in_your_area :: String
, account_blocked :: String
, you_can_still_access :: String
, facing_problem_with_app :: String
, tap_here_to_report :: String
, confirm_your_ride :: String
, ride_scheduled :: String
, ride_starts_on :: String
, rental_package :: String
, go_home :: String
, cancel_rental_booking :: String
, add_first_stop :: String
, driver_will_be_assigned_minutes_before_starting_the_ride :: String
, years_ago :: String
, reported_issues :: String
, resolved_issues :: String
, issue_no :: String
, reported :: String
, resolved :: String
, months_ago :: String
, days_ago :: String
, hours_ago :: String
, min_ago :: String
, sec_ago :: String
, ride_related_issue_page_name :: String
, app_related_issue_page_name :: String
, driver_related_issue_page_name :: String
, lost_and_found_issue_page_name :: String
, select_a_ride_to_report :: String
, i_dont_know_which_ride :: String
, your_reports :: String
, view :: String
, add_voice_note :: String
, voice_note :: String
, added_images :: String
, no_images_added :: String
, submit_issue_details :: String
, image_preview :: String
, report_issue_chat_placeholder :: String -> String
, added_voice_note :: String
, no_voice_note_added :: String
, call_driver_title :: String
, call_driver_description :: String
, call_support_title :: String
, call_support_description :: String -> String
, add_image :: String
, add_another :: String
, images :: String
, issue_submitted_text :: String
, issue_resolved_text :: String
, choose_an_option :: String
, image :: String
, issue_marked_as_resolved :: String
, still_having_issue :: String
, record_voice_note :: String
, cancel_button :: String
, max_images :: String
, sos_issue_page_name :: String
, fare_discrepancies_issue_page_name :: String
, payment_related_issue_page_name :: String
, account_related_issue_page_name :: String
, payment_and_fare_related_issue_page_name :: String
, vehicle_related_issue_page_name :: String
, issue :: String
, other_issues :: String
, cant_find_option :: String
, need_help :: String
, safety_issue_page_name :: String
, we_hope_the_issue_is_resolved :: String -> String
, please_select_the_ride_to_call_driver :: String
, add_image_s :: String
, already_have_an_active_ride :: String
, confirm_stop_location :: String
, confirm_drop :: String
, book_metro_with_ny_now :: String
, learn_about_namma_safety :: String
, namma_safety_will_enable_access :: String
, edit_actions :: String
, emergency_actions :: String
, when_you_start_emergency_sos :: String
, ride_share_after_six_pm :: String
, who_can_track_your_ride :: String -> String
, emergency_sharing_with_contacts :: String
, sharing_with :: String
, add_a_contact :: String
, to_ensure_safety_users_should :: String
, about_sos_desc :: String
, few_examples_of_sos_situations :: String
, things_to_do_during_sos_situation :: String
, emergency_request_sent :: String
, sos_triggered_desc :: String
, sos_actions :: String
, call_police :: String
, call_support :: String
, record_video :: String
, stop_and_share_recording :: String
, cancel_sharing :: String
, start_recording :: String
, sharing_the_video_in :: String
, emergency_info_shared :: String
, emergency_info_shared_action :: String
, set_up_your_personal_safety_settings :: String
, activate_live_video_recording_features :: String
, choose_responsive_contacts :: String
, share_location_and_ride_details_emergency_contact :: String
, namma_safety_measures :: String
, safety_guidelines_for_you :: String
, about_sos :: String
, night_time_safety_checks :: String
, share_info_with_emergency_contacts_title :: String
, share_info_with_emergency_contacts_desc :: String
, trigger_alert_to_nammayatri_support_title :: String -> String
, trigger_alert_to_nammayatri_support_desc :: String
, enable_night_time_safety_alerts_title :: String
, enable_night_time_safety_alerts_desc :: String
, almost_done_title :: String
, almost_done_desc :: String
, safety_measure_1 :: String
, safety_measure_2 :: String
, safety_measure_3 :: String
, safety_measure_4 :: String
, safety_measure_5 :: String -> String
, safety_measure_6 :: String
, safety_guidelines_1 :: String
, safety_guidelines_2 :: String
, safety_guidelines_3 :: String
, safety_guidelines_4 :: String
, safety_guidelines_5 :: String
, safety_guidelines_6 :: String
, safety_guidelines_7 :: String
, about_sos_1 :: String
, about_sos_2 :: String
, about_sos_3 :: String
, about_sos_4 :: String
, about_sos_5 :: String
, about_sos_6 :: String
, about_sos_7 :: String
, about_sos_8 :: String
, about_sos_9 :: String
, about_sos_10 :: String
, about_sos_11 :: String -> String
, about_sos_12 :: String
, about_sos_13 :: String
, the_video_will_be_recorded :: String
, emergency_video :: String
, namma_safety_is_set_up :: String
, personal_safety_settings_permission_request :: String
, activate_namma_safety_popup_title :: String
, activate_namma_safety_popup_desc :: String
, activate_namma_safety_popup_action :: String
, dismiss :: String
, send_silent_sos_to_police :: String
, our_safety_partner :: String
, bangaluru_city_police :: String
, get_options_to_directly_call_police :: String
, share_sos_silently_with_police :: String
, call_and_alert_the_nearest_police_centre :: String
, send_a_silent_sos_to_the_police :: String
, send_a_video_recording_to_police :: String
, personal_safety_action_1 :: String
, personal_safety_action_2 :: String -> String
, personal_safety_action_2_police :: String
, personal_safety_action_3 :: String
, send_video_to_police :: String
, finish_setup :: String
, mark_ride_as_safe :: String
, activate_sos :: String
, emergency_info_shared_action_police :: String
, start_setup :: String
, call_support_for_safety :: String -> String
, we_noticed_your_ride_hasnt_moved :: String
, we_noticed_your_ride_is_on_different_route :: String
, we_are_here_for_you :: String
, i_need_help :: String
, i_feel_safe :: String
, everything_okay_q :: String
, please_remain_calm_you_can_request_an_immediate_call :: String
, receive_call_from_support :: String
, add_contacts :: String
, add_contacts_manually :: String
, video_share_info_to_police :: String
, call_police_helpline :: String
, please_remain_calm_call_police :: String
, please_allow_camera_and_microphone_permissions :: String
, activate_namma_safety_will_enable_access :: String
, select_preferred_contacts :: String
, new :: String
, safety_center :: String
, emergency_sos :: String
, automatic_call_placed_to_emergency_contacts :: String
, emergency_contacts_can_follow :: String -> String
, alert_safety_team :: String -> String
, option_to_report_a_safety_issue :: String
, recommend_emergency_contacts_to_install :: String -> String
, test_safety_drill :: String
, start_test_drill :: String
, report_safety_issue :: String
, safety_team_will_be_alerted :: String -> String
, emergency_contacts_can_take_action :: String -> String
, share_ride :: String
, share_ride_description :: String -> String
, share_ride_with_contact :: String -> String
, share_link :: String
, glad_to_know_you_are_safe :: String
, please_stay_calm_team_alerted :: String -> String
, try_another_contact :: String
, your_current_location :: String
, this_is_not_a_real_sos_situation :: String
, your_vehicle_info :: String
, police_view_instruction :: String
, test_sos_activating_in :: String
, sos :: String
, test_sos :: String
, select_contact_to_call :: String
, emergency_sos_activating :: String
, press_to_start_test_drill :: String
, press_in_case_of_emergency :: String
, inform_emergency_contacts :: String
, available_in_real_emergency :: String
, other_safety_actions :: String
, disclaimer :: String
, use_only_in_emergency :: String
, misuse_may_lead_to_legal_action :: String
, use_test_drill :: String
, indication_to_emergency_contacts :: String -> String
, are_you_ready_to_start_drill :: String
, test_drill_desc :: String
, learn_about_safety_mode :: String
, test_emergency_request_sent :: String
, test_sos_triggered_desc :: String
, sos_will_be_disabled :: String
, dial_now :: String
, following :: String -> String
, turn_off_alarm :: String
, choose_a_person_to_follow :: String
, is_in_sos_situation :: String -> String
, marked_ride_safe :: String -> String
, stay_calm_keep_tracking :: String -> String
, you_will_be_notified :: String
, tap_here_to_follow :: String -> String
, have_shared_ride_with_you :: String -> String
, sos_location :: String
, this_is_a_test_mock_drill :: String -> String
, this_is_not_real_drill :: String
, reached_destination_safely :: String -> String
, ride_ended :: String -> String
, complete_your_test_drill :: String
, test_drill :: String
, ride_shared_with_selected_contacts :: String
, terms_and_conditions_updated :: String
, okay :: String
, try_later :: String
, referral_code_is_applied :: String
, you_have_already_used_different_referral_code :: String
, invalid_referral_code :: String
, stops :: String
, green_line :: String
, blue_line :: String
, red_line :: String
, view_route_info :: String
, valid_until :: String
, ticket_number :: String
, ticket :: String
, tickets :: String
, onword_journey :: String
, round_trip_str :: String
, tickets_for_chennai_metro :: String
, active_str :: String
, expired_str :: String
, used_str :: String
, map_str :: String
, ticket_details :: String
, route_details :: String
, uncertain_about_metro_routes :: String
, see_map :: String
, chennai_metro_term_1 :: String
, chennai_metro_term_2 :: String
, chennai_metro_term_event :: String
, free_ticket_cashback :: String
, no_of_passengers :: String
, maximum :: String
, tickets_allowed_per_user :: String
, starting_from :: String
, from :: String
, to :: String
, booking_id :: String
, please_while_gen_ticket :: String
, payment_received :: String
, please_check_back_few_min :: String
, your_booking_pending :: String
, please_retry_booking :: String
, booking_failed :: String
, incase_of_fail :: String
, refresh_status :: String
, date :: String
, no_of_tickets :: String
, active_tickets :: String
, confirming_str :: String
, failed_str :: String
, confirmed_str :: String
, buy_metro_tickets :: String
, get_fare :: String
, metro_booking_timings :: String
, chennai_metro_time :: String -> String -> String
, delhi_metro_time :: String -> String -> String
, please_come_back_later_metro :: String
, no_qoutes_available :: String
, i_agree_to_the :: String
, here_is_metro_ticket :: String
, view_ticket :: String
, destination :: String
, pay :: String
, pending_str :: String
, past_tickets :: String
, one_way_str :: String
, share_ticket :: String
, origin :: String
, history :: String
, always :: String
, always_share_desc :: String
, night_rides_share :: String
, night_rides_desc :: String
, never :: String
, never_share_desc :: String
, share_trip_notificatons :: String
, call_customer_support :: String
, yet_to_start :: String -> String
, message_from :: String -> String
, ride_cancelled :: String
, track_ride_string :: String -> String -> String -> String -> String
, safety_center_is_disabled :: String
, track_on_google_map :: String
, show_walking_direction :: String
, special_pickup_zone :: String
, special_pickup_zone_ride :: String
, we_will_try_to_connect_you_with_driver_in_closest_pickup_zone :: String
, this_provides_you_an_instant_pickup_experience :: String
, driver_at_pickup_location :: String
, driver_almost_at_pickup :: String
, maximum_edit_pickup_attempts_reached :: String
, move_pin_to_the_desired_pickup_point :: String
, change_pickup_location :: String
, location_is_too_far :: String
, a_tip_helps_find_a_ride_quicker :: String
, tip_added :: String
, continue_search_with_no_tip :: String
, searching_with_no_tip :: String
, search_again :: String
, driver_is_on_the_way :: String
, driver_is_waiting_at_pickup :: String
, is_at_pickup_location :: String
, go_to_selected_spot_for_pickup :: String
, select_popular_spot_for_hassle_free_pickup :: String
, ticket_is_non_cancellable :: String
, cancel_booking :: String
, booking_not_cancellable :: String
, bookings_will_be_cancelled :: String
, bookings_will_be_cancelled_with_refund :: String -> String
, refund_not_applicable :: String
, yes_cancel_booking :: String
, would_you_like_to_proceed :: String
, booking_cancelled :: String
, refund_is_in_process :: String
, total_refund :: String
, number_of_tickets :: String
, cancellation_date :: String
, tickets_for_kochi_metro :: String
, your_booked_tickets :: String
, plan_your_journey :: String
, book_round_trip :: String
, by_proceeding_you_agree :: String
, terms_and_conditions_full :: String
, experience_hassle_free_metro_booking :: String -> String
, kochi_metro_term_1 :: String
, kochi_metro_term_2 :: String
, kochi_metro_time :: String -> String -> String
, book_ticket :: String
, prepare_emergency_contacts :: String
, emergency_contacts_will_be_notified :: String
, inform_emergency_contacts_about_test :: String
, recent_ride_issue_desc :: String
, i_need_help_with_my_recent_ride :: String
, continue_with_safety_settings :: String
, tap_where_to_to_book_ride :: String
, last_chosen_variant_not_available :: String
, toll_charges :: String
, toll_charges_desc :: String
, toll_charges_including :: String -> String
, toll_road_changed :: String
, parking_charge :: String
, toll_or_parking_charges :: String
, toll_charges_estimated :: String
, add_tip :: String
, change_ride_type :: String
, try_adding_tip_or_change_ride_type :: String
, applicable_toll_charges :: String
, update_tip_str :: String
, book :: String -> String
, fare_for :: String -> String
, waiting_charge_limit :: String -> String
, time_taken :: String
, trip_distance :: String
, unable_to_cancel_ride :: String
, got_a_referral_from_a_driver_or_friend :: String
, enter_referral_code_ :: String
, referred_users :: String
, show_app_qr :: String
, share_and_refer :: String
, your_referral_code :: String
, refer_your_friends :: String
, referrals :: String
, enter_now :: String
, what_is_referral_program :: String
, users_who_download_app_and_complete_their_first_ride_using_referral_code :: String -> String
, the_referral_program_incentivises_drivers_to_accept_more_rides :: String -> String
, invalid_code :: String
, enter_6_digit_referral_code_below :: String
, apply :: String
, toll_charges_included :: String
, one_tap_bookings :: String
, has_your_driver_set_the_ac_as_per_your_preference :: String
, no_report_an_issue :: String
, great_enjoy_the_trip :: String
, enjoy_your_budget_friendly_non_ac_ride :: String
, ac_is_not_available_on_this_ride :: String
, ac_not_working_desc :: String
, showing_fare_from_multi_provider :: String
, live_chat :: String
, driver_tip_addition :: String
, live_ride_sharing :: String
, enhanced_safety :: String
, confirm_provider :: String
, select_a_provider :: String
, confirming_selected_provider :: String
, book_top_provider :: String
, choose_from_providers :: String
, choose_between_providers :: String
, choose_between_providers_desc :: String
, guaranteed_ride :: String
, this_ride_fulfilled_by :: String -> String
, additional_features_on :: String -> String
, notify_your_ec :: String
, ec_can_respond :: String
, quick_support :: String -> String
, learn_about_app_safety_feat :: String -> String
, other_provider_no_receipt :: String
, ride_fulfilled_by :: String -> String
, congestion_charges :: String
, tip_can_be_added :: String -> String
, congestion_charges_desc :: String -> String
, ac_turned_off :: String
, book_any :: String
, estimates_expiry_error :: String
, estimates_expiry_error_and_fetch_again :: String
, pay_your_driver_by_cash_or_upi :: String
, trip_delayed :: String
, select_vehicle :: String
, book_rental :: String
, confirm_rental :: String
, rental_ride :: String
, select_duration :: String
, select_distance :: String
, rental_options :: String
, booking_on :: String
, included_kms :: String
, base_fare :: String
, tolls_and_parking_fees :: String
, final_fare_description :: String
, excess_distance_charge_description :: String -> String
, additional_charges_description :: String
, parking_fees_and_tolls_not_included :: String
, night_time_fee_description :: String
, choose_your_rental_ride :: String
, first_stop_optional :: String
, january :: String
, february :: String
, march :: String
, april :: String
, may :: String
, june :: String
, july :: String
, august :: String
, september :: String
, october :: String
, november :: String
, december :: String
, hours :: String
, not_added_yet :: String
, next_stop :: String
, time :: String
, distance :: String
, starting_odo :: String
, end_otp :: String
, only_location_within_city_limits :: String
, ride_time :: String
, ride_distance :: String
, ride_started_at :: String
, ride_ended_at :: String
, estimated_fare :: String
, extra_time_fare :: String
, total_fare :: String
, fare_update :: String
, now :: String
, date_invalid_message :: String
, edit_pickup :: String
, add_stop :: String
, enter_pickup_loc :: String
, intercity_options :: String
, proceed :: String
, schedule_ride_available :: String
, rental_ride_until :: String
, extra_time_charges :: String
, dist_based_charges :: String
, time_based_charges :: String
, rental_policy :: String
, select_package :: String
, rental_policy_desc :: String
, rental_policy_desc_1 :: String
, rentals_intercity_available :: String -> String
, check_it_out :: String
, failed_to_cancel :: String
, scheduling_allowed_in_intercity_rental :: String
, special_zone_intercity_ineligible :: String
, no_rides_scheduled_yet :: String
, ride_booking :: String
, special_zone_rental_ineligible :: String
, services :: String
, you_have_upcoming_rental_booking :: String -> String
, scheduled :: String
, upcoming_bookings :: String
, rentals_ :: String
, inter_city_ :: String
, you_have_upcoming_intercity_booking :: String -> String
, a_ride_already_exists :: String
, you_have_an_ride_from_to_scheduled_from_till :: String
, extra_per_km_fare :: String
, extra_per_minute_fare :: String
, pickup_charges :: String
, waiting_charges_after_3_mins :: String
, fare_determined_as_per_karnataka_guidelines :: String
, rental_charges :: String
, rental_info_policy_desc :: String -> String
, rental_info_policy_desc_ :: String
, rental_screen_explainer :: String
, instant :: String
, coming_soon :: String
, cancel_scheduled_ride :: String
, cancel_scheduled_ride_desc :: String
, confirm_cancellation :: String
, intercity_rides_coming_soon :: String
, view_fares :: String
, excess_time_description :: String -> String
, estimated_charges :: String
, your_cancellation_rate_is_high :: String
, avoid_further_cancellations_to_keep_using_app :: String -> String
, night_time_fees :: String
, parking_and_other_charges :: String
, additional_charges :: String
, estimated_base_fare :: String
, included_distance :: String
, included_time :: String
, toll_charges_description :: String
, will_be_added_to_final_fare :: String
, extra_distance_fare :: String
, network_error :: String
, server_error :: String
, unknown_error :: String
, connection_refused :: String
, timeout :: String
, was_toll_exp_smooth :: String
, was_toll_exp_smooth_desc :: String
, was_driver_helpful :: String
, was_ride_safe_desc :: String
, was_ride_safe :: String
, was_driver_helpful_desc :: String
, collect_toll_sep :: String
, final_fare_excludes_toll :: String
, toll_charges_maybe_applicable :: String
, metro_banner_title :: String -> String
, view_on_google_maps :: String
, walking_directions_to_pickup :: String
, explore_city_with_us :: String -> String
, go_to_destination :: String -> String
, walk_to :: String -> String
, bangalore :: String
, kolkata :: String
, paris :: String
, kochi :: String
, delhi :: String
, hyderabad :: String
, mumbai :: String
, chennai :: String
, coimbatore :: String
, pondicherry :: String
, goa :: String
, pune :: String
, mysore :: String
, tumakuru :: String
, noida :: String
, gurugram :: String
, waiting_charges :: String
, quotes_expiry_error_and_fetch_again :: String
, place_a_call :: String
, you_can_write_to_us_at :: String
, chargeable :: String
, booked :: String
, surcharges :: String
, siliguri :: String
, kozhikode :: String
, thrissur :: String
, trivandrum :: String
, metro_free_ticket_event :: String -> String
, metro_free_ticket_event_desc :: String -> String -> String
, next_free_ticket :: String
, free_ticket_available :: String -> String -> String
, additional_charges_will_be_applicable :: String
, parking_charges_included :: String -> String
, app_toll_charges :: String
, app_parking_charges :: String
, app_toll_parking_charges :: String
, parking_charges_desc :: String
, toll_charges_included_in_fair :: String
, please_do_not_pay_extra_to_driver :: String
, vellore :: String
, hosur :: String
, madurai :: String
, thanjavur :: String
, tirunelveli :: String
, salem :: String
, trichy :: String
, davanagere :: String
, shivamogga :: String
, hubli :: String
, mangalore :: String
, gulbarga :: String
, udupi :: String
, cancel_booking_ :: String
, cancel_intercity_booking :: String
, rental_booking :: String
, intercity_booking :: String
, booking :: String
, by :: String
, customers :: String
, rating :: String
, cancellation :: String
, trips :: String
, i_speak :: String
, and :: String
, with_nammayatri_for :: String -> String
, years :: String
, vehicle_number :: String
, what_people_say :: String
, star_rating :: String
, card_texts :: String
, trainings_i_completed :: String
, i_pledge :: String
, clean_bike :: String
, uncomfortable_bike :: String
, driver_available :: String
, drivers_available :: String
, more_safety_measures :: String
, safety_setup :: String
, complete :: String
, trusted_contact_help :: String
, driver_safety_standards :: String
, trusted_contact :: String
, trusted_contact_highlight :: String
, safety_drill :: String
, default_contact :: String
, app_call_chat :: String
, trusted_contact_desc :: String
, enable_live_tracking :: String
, unexpected_event_check :: String
, unexpected_event_check_desc :: String
, unexpected_event_check_timings :: String
, next :: String
, post_ride_check :: String
, post_ride_check_desc :: String
, post_ride_check_timings :: String
, safety_team_notification :: String
, notify_safety_team :: String
, notify_safety_team_sub :: String
, notify_safety_team_note :: String
, emergency_sos_new :: String
, emergency_sos_sub :: String
, shake_to_activate :: String
, shake_to_activate_sub :: String -> String
, automatic_call_sos :: String
, automatic_call_sos_sub :: String
, place_default_call :: String
, default_call_contact :: String
, default_contact_desc :: String
, more_emergency_actions :: String
, siren :: String
, call_police_desc :: String
, record_audio_desc :: String
, siren_desc :: String
, call_safety_team_desc :: String
, safety_drill_desc :: String
, safety_drill_sub :: String
, safety_drill_note :: String
, ride_actions :: String
, ride_actions_sub :: String
, live_tracking :: String
, live_tracking_sub :: String
, chat_with_rider :: String
, chat_with_rider_sub :: String
, emergency_actions_sub :: String
, current_initiatives :: String
, current_initiatives_sub :: String
, driver_verification :: String
, driver_verification_sub :: String
, safety_feedback :: String
, safety_feedback_sub :: String
, safety_training :: String
, safety_training_sub :: String
, driver_id_check :: String
, driver_id_check_sub :: String
, data_privacy :: String
, data_privacy_sub :: String
, favourite_driver :: String
, favourite_driver_sub :: String
, women_drivers :: String
, women_drivers_sub :: String
, dashcam :: String
, dashcam_sub :: String
, never_share_ln :: String
, always_share_ln :: String
, share_with_time_constraints_ln :: String
, never_share_em :: String
, always_share_em :: String
, share_with_time_constraints_em :: String
, live_ride_tracking :: String
, live_ride_tracking_desc :: String
, upcoming_initiatives :: String
, upcoming_initiatives_desc :: String
, receive_call_from_safety_team :: String
, notify_all_emergency_contacts :: String
, record_audio :: String
, call_safety_team :: String
, safety_team_callback_requested :: String
, emergency_contacts_notified :: String
, call_placed :: String
, emergency_sos_activated :: String
, tap_to_call_other_emergency_contacts :: String
, recording_audio :: String
, recorded_audio :: String
, share_with_safety_team :: String
, emergency :: String
, manual_live_tracking :: String
, manual_live_tracking_desc :: String
, automatic_live_tracking :: String
, automatic_live_tracking_desc :: String
, tracking_no_setup :: String
, following_str :: String
, dialing_police_in_time :: String -> String
, reached_destination :: String -> String
, mins :: String
, updated_fare :: String
, how's_trip :: String
, provided_feedback :: String
, favourite_your_driver :: String
, prefer_driver :: String
, write_review :: String
, transit :: String
, intercity_str :: String
, rental_str :: String
, delivery_str :: String
, where_are_you_going :: String
, tap_to_follow :: String
, has_shared_a_ride_with_you :: String -> String
, test_sos_activated :: String
, choose_from_contacts :: String
, add_manually :: String
, default_contact_not_set :: String
, driver :: String
, recommend_share_manually :: String
, cannot_add_own_number :: String
, confirm_pickup_and_drop_location :: String
, confirm_your_delivery :: String
, payment_at_receiving_end :: String
, payment_at_receiving_end_desc :: String
, sender :: String
, receiver :: String
, phone :: String -> String
, building_or_flat :: String -> String
, optional_instruction :: String
, help_us_provide_smooth_pickup :: String
, help_us_provide_smooth_drop :: String
, i_am_the_sender :: String
, i_am_the_receiver :: String
, reckless_handling :: String
, delivery_delayed :: String
, items_missing :: String
, polite_attitude :: String
, smooth_experience :: String
, secure_delivery :: String
, minimal_calling :: String
, rude_behaviour :: String
, package_photo_and_otp :: String
, send_now :: String
, book_for :: String
, delivery_details :: String
, delivery_guidelines :: String
, view_all_guidelines :: String
, items_should_fit_in_backpack :: String -> String
, avoid_sending_high_value_items :: String
, illegal_items_prohibited :: String
, pickup_instruction :: String
, more_about_me :: String
, driving_since :: String
, driver_section_card :: String
, know_your_driver :: String
, rated :: String
, drivers :: String
, locations :: String
, knows_your_driver :: String
, last_trip :: String
, rupees :: String
, paid_by_cash :: String
, remove_from_favourite :: String
, clean_vehicle :: String
, ac_not_turned_on :: String
, late_pick_up_arrival :: String
, asked_for_more_fare :: String
, unhygienic_vehicle :: String
, training :: String
, financial :: String
, kids_education :: String
, buy_new_vehicle :: String
, failed_to_remove_driver :: String
, not_available :: String
, buy_new_home :: String
, you_favourited :: String
, favorite_your_driver :: String
, favourite_driver_preference :: String
, ride_type_with_favourite_driver :: String
, gotit :: String
, no_favourite_yet :: String
, favourite_appear_here :: String
, edit_your_pickup_location_instead :: String
, round_trip_invalid_message :: String
, pickup_time_not_selected :: String
, booking_duration_invalid :: String
, return :: String
, pickup_input :: String
, return_input :: String
, book_a_round_trip :: String
, total_ride_duration :: String
, total_ride_distance :: String
, ride :: String
, accept :: String
, pass :: String
, term_1a :: String
, term_2a :: String
, term_3a :: String
, term_1b :: String
, term_2b :: String
, term_3b :: String
, excluded_charges :: String
, tolls :: String
, state_permit :: String
, excluded_footer :: String
, included_charges :: String
, inc_1 :: String
, inc_2a :: String
, inc_2b :: String
, pickup_drop :: String
, intercity_tc_1 :: String
, intercity_tc_2 :: String
, round_trip_policy_card_1 :: String
, round_trip_policy_card_2 :: String
, round_trip_policy :: String
, distance_fare :: String
, driver_allowance_ :: String
, night_charges :: String
, night_charges_description :: String
, state_charges_description :: String
, parking_charges_description :: String
, toll_and_parking_charges_description :: String
, state_permit_charges :: String
, driver_allowance_description :: String -> String -> String
, driver_allowance_required :: String
, toll_and_parking_charges :: String
, night_shift_charges :: String
, round_trip_explainer :: String
, reserve :: String
, leave_now :: String
, is_your_driver :: String
, your_driver_will_be :: String
, ride_summary :: String
, trip_inclusions :: String
, please_pay_parking_or_other :: String
, toll_and_state_permit :: String
, trip_exclusion :: String
, confirm :: String
, you_have_an_ride_from_without_to :: String
, for_every_extra_hour_you_add :: String
, by_default_one_hour :: String
, you_have_an_upcoming_booking :: String -> String
, addon_km_charge :: String
, time_fare :: String
, trip_fare_includes :: String
, trip_fare_excludes :: String
, extras_will_be_charged_at :: String
, fixed_charges :: String
, pickup_date_and_time :: String
, drop_back_in_at :: String -> String
, per_km :: String
, per_min :: String
, hour :: String
, sec :: String
, bike_taxi :: String
, phone_number_permission :: String
, phone_number_permission_desc :: String
, deny :: String
, allow :: String
, intercity_bus :: String
, driver_assigned :: String
, sender_name :: String
, sender_phone :: String
, senders_building_flat :: String
, pickup_instructions :: String
, receiver_name :: String
, receiver_phone :: String
, receivers_building_flat :: String
, drop_instructions :: String
, out_for_delivery :: String
, pickup_in_progress :: String
, arrived_at_drop :: String
, out_for_pickup :: String
, confirm_pickup :: String
, rate_your_delivery_with :: String
, delivery_completed :: String
, estimated_arrival_by :: String
, back :: String
, photo_not_taken_yet :: String
, please_refresh_once_driver_takes_photo :: String
, package_photo_desc :: String
, start_otp :: String
, package_photo :: String
, refresh :: String
, drop_instruction :: String
, quick_delivery_with :: String -> String
, step :: String
, booking_cannot_proceed_one_party_has_active_booking :: String
, please_enter_a_valid_mobile_number :: String
, please_enter_a_valid_address :: String
, please_enter_a_valid_name :: String
, enter_a_name :: String
, enter_a_address :: String
, explore :: String
, delivered_in_just :: String -> String
, odisha :: String 
, bhubaneswar :: String
, different_bike :: String
, limit_reached :: String
, confirm_contacts :: String
, tickets_for_delhi_metro :: String
, max_parcel_size :: String -> String -> String -> String
, bus__ :: String 
, tickets_for_chennai_bus :: String
, buy_bus_tickets :: String
, book_and_pay :: String
, bus_ticket :: String
, check_spelling_and_try_again :: String
, book_bus_ticket :: String
, book_a_one_way_instant_bus_ticket :: String
, recent_ticket :: String
, experience_hassle_free_bus_bookings_with :: String
, enter_bus_number_or_destination :: String
, destination_stop :: String
, route_bus_number :: String
, pickup_stop :: String
, tickets_for_kolkata_bus :: String
, ticket_validity_30_minutes :: String
, fare_commission_free_wbtc :: String
, select_route_number :: String
, pickup_and_destination_stop :: String
, bus_boarded_confirmation :: String
, towards_station :: String -> String
, bus_no :: String -> String
, verified :: String
, experience_our_pilot_launch_for_bus_ticketing_in_prime_routes :: String -> String
, note_your_ticket_is_only_valid_for :: String -> String
, here_is_bus_ticket :: String
, were_you_asked_to_pay_extra_q :: String
, were_you_asked_to_pay_extra_desc :: String 
, we_are_sorry_to_hear_this_please_click_on_need_help :: String
, driver_asked_extra_money :: String
, driver_is_asking_for_extra_fare :: String
, a_c :: String
, no_oxygen :: String
, oxygen :: String
, ventilator :: String
, ambulance_ :: String
, uncomfortable_ambulance :: String
, clean_ambulance :: String
, ambulance_booking_disclaimer :: String
, different_ambulance :: String
, no_remaining_tickets :: String
, ticket_not_booked_refund_initiated :: String 
, parcel_details :: String
, parcel_type :: String
, parcel_quantity :: String
, unloading_time :: String
, loading_time :: String
, helps_smooth_delivery :: String
, free_loading_unloading_time :: String -> String -> String
, max_capacity_warning :: String
, select_route :: String
}


derive instance ntL :: Newtype Languages _

derive instance ntK :: Newtype Keymap _

a :: forall a. Proxy a
a = Proxy


about :: Proxy "about"
about = a

safety :: Proxy "safety"
safety = a

safety_check_in :: Proxy "safety_check_in"
safety_check_in = a

about_app_description :: Proxy "about_app_description"
about_app_description = a

about_referral_program :: Proxy "about_referral_program"
about_referral_program = a

about_referral_program_discription :: Proxy "about_referral_program_discription"
about_referral_program_discription = a

account_deletion_confirmation :: Proxy "account_deletion_confirmation"
account_deletion_confirmation = a

add_another_contact :: Proxy "add_another_contact"
add_another_contact = a

add_emergency_contacts :: Proxy "add_emergency_contacts"
add_emergency_contacts = a

add_favourite :: Proxy "add_favourite"
add_favourite = a

add_new_address :: Proxy "add_new_address"
add_new_address = a

add_new_favourite :: Proxy "add_new_favourite"
add_new_favourite = a

add_now :: Proxy "add_now"
add_now = a

add_saved_location_from_settings :: Proxy "add_saved_location_from_settings"
add_saved_location_from_settings = a

add_tag :: Proxy "add_tag"
add_tag = a

address :: Proxy "address"
address = a

addresses :: Proxy "addresses"
addresses = a

all_favourites :: Proxy "all_favourites"
all_favourites = a

all_topics :: Proxy "all_topics"
all_topics = a

already_exists :: Proxy "already_exists"
already_exists = a

also_share_your_ride_status_and_location :: Proxy "also_share_your_ride_status_and_location"
also_share_your_ride_status_and_location = a

amount_paid :: Proxy "amount_paid"
amount_paid = a

anonymous_call :: Proxy "anonymous_call"
anonymous_call = a

are_you_staring :: Proxy "are_you_staring"
are_you_staring = a

are_you_sure_you_want_to_cancel :: Proxy "are_you_sure_you_want_to_cancel"
are_you_sure_you_want_to_cancel = a

are_you_sure_you_want_to_logout :: Proxy "are_you_sure_you_want_to_logout"
are_you_sure_you_want_to_logout = a

are_you_sure_you_want_to_remove_contact :: Proxy "are_you_sure_you_want_to_remove_contact"
are_you_sure_you_want_to_remove_contact = a

are_you_sure_you_want_to_remove_favourite_ :: Proxy "are_you_sure_you_want_to_remove_favourite_"
are_you_sure_you_want_to_remove_favourite_ = a

ask_for_price :: Proxy "ask_for_price"
ask_for_price = a

ask_for_price_info :: Proxy "ask_for_price_info"
ask_for_price_info = a

asked_for_more_money :: Proxy "asked_for_more_money"
asked_for_more_money = a

at_drop :: Proxy "at_drop"
at_drop = a

at_pickup :: Proxy "at_pickup"
at_pickup = a

auto_accepting_selected_ride :: Proxy "auto_accepting_selected_ride"
auto_accepting_selected_ride = a

auto_assign_a_ride :: Proxy "auto_assign_a_ride"
auto_assign_a_ride = a

auto_assign_driver :: Proxy "auto_assign_driver"
auto_assign_driver = a

away :: Proxy "away"
away = a

away_c :: Proxy "away_c"
away_c = a

base_fares :: Proxy "base_fares"
base_fares = a

board_the_first :: Proxy "board_the_first"
board_the_first = a

book_now :: Proxy "book_now"
book_now = a

book_ride_ :: Proxy "book_ride_"
book_ride_ = a

booking_preference :: Proxy "booking_preference"
booking_preference = a

boost_your_ride_chances_and_help_drivers_with_tips :: Proxy "boost_your_ride_chances_and_help_drivers_with_tips"
boost_your_ride_chances_and_help_drivers_with_tips = a

by_cash :: Proxy "by_cash"
by_cash = a

by_tapping_continue :: Proxy "by_tapping_continue"
by_tapping_continue = a

call :: Proxy "call"
call = a

call_driver :: Proxy "call_driver"
call_driver = a

call_driver_using :: Proxy "call_driver_using"
call_driver_using = a

call_emergency_contacts :: Proxy "call_emergency_contacts"
call_emergency_contacts = a

call_emergency_centre :: Proxy "call_emergency_centre"
call_emergency_centre = a

cancel_ :: Proxy "cancel_"
cancel_ = a

cancel_auto_assigning :: Proxy "cancel_auto_assigning"
cancel_auto_assigning = a

cancel_ongoing_search :: Proxy "cancel_ongoing_search"
cancel_ongoing_search = a

cancel_ride :: Proxy "cancel_ride"
cancel_ride = a

request_edit :: Proxy "request_edit"
request_edit = a

cancel_search :: Proxy "cancel_search"
cancel_search = a

cancel_str :: Proxy "cancel_str"
cancel_str = a

cancelled :: Proxy "cancelled"
cancelled = a

change :: Proxy "change"
change = a

change_drop_location :: Proxy "change_drop_location"
change_drop_location = a

change_location :: Proxy "change_location"
change_location = a

check_out_live_stats :: Proxy "check_out_live_stats"
check_out_live_stats = a

check_your_internet_connection_and_try_again :: Proxy "check_your_internet_connection_and_try_again"
check_your_internet_connection_and_try_again = a

choose_a_ride_as_per_your_comfort :: Proxy "choose_a_ride_as_per_your_comfort"
choose_a_ride_as_per_your_comfort = a

choose_between_multiple_drivers :: Proxy "choose_between_multiple_drivers"
choose_between_multiple_drivers = a

choose_between_multiple_rides :: Proxy "choose_between_multiple_rides"
choose_between_multiple_rides = a

choose_on_map :: Proxy "choose_on_map"
choose_on_map = a

choose_your_ride :: Proxy "choose_your_ride"
choose_your_ride = a

comfy :: Proxy "comfy"
comfy = a

confirm_and_book :: Proxy "confirm_and_book"
confirm_and_book = a

confirm_and_save :: Proxy "confirm_and_save"
confirm_and_save = a

confirm_changes :: Proxy "confirm_changes"
confirm_changes = a

confirm_drop_location :: Proxy "confirm_drop_location"
confirm_drop_location = a

check_revised_fare_and_route :: Proxy "check_revised_fare_and_route"
check_revised_fare_and_route = a

confirm_emergency_contacts :: Proxy "confirm_emergency_contacts"
confirm_emergency_contacts = a

confirm_for :: Proxy "confirm_for"
confirm_for = a

confirm_location :: Proxy "confirm_location"
confirm_location = a

confirm_pickup_location :: Proxy "confirm_pickup_location"
confirm_pickup_location = a

confirm_ride_ :: Proxy "confirm_ride_"
confirm_ride_ = a

confirming_the_ride_for_you :: Proxy "confirming_the_ride_for_you"
confirming_the_ride_for_you = a

contact_support :: Proxy "contact_support"
contact_support = a

contacts_selected :: Proxy "contacts_selected"
contacts_selected = a

continue :: Proxy "continue"
continue = a

copied :: Proxy "copied"
copied = a

could_not_connect_to_driver :: Proxy "could_not_connect_to_driver"
could_not_connect_to_driver = a

current_location :: Proxy "current_location"
current_location = a

fare_updated :: Proxy "fare_updated"
fare_updated = a

please_confirm_with_your_after_requesting :: Proxy "please_confirm_with_your_after_requesting"
please_confirm_with_your_after_requesting = a

previously :: Proxy "previously"
previously = a

route_and_fare_updated :: Proxy "route_and_fare_updated"
route_and_fare_updated = a

previous_fare :: Proxy "previous_fare"
previous_fare = a

your_driver_might_want_to_go_towards_the_current_drop_kindly_ask_them_to_confirm_after_requesting :: Proxy "your_driver_might_want_to_go_towards_the_current_drop_kindly_ask_them_to_confirm_after_requesting"
your_driver_might_want_to_go_towards_the_current_drop_kindly_ask_them_to_confirm_after_requesting = a

currently_we_are_live_in_ :: Proxy "currently_we_are_live_in_"
currently_we_are_live_in_ = a

customer_selected_fare :: Proxy "customer_selected_fare"
customer_selected_fare = a

customer_tip_description :: Proxy "customer_tip_description"
customer_tip_description = a

dial_112 :: Proxy "dial_112"
dial_112 = a

details :: Proxy "details"
details = a

data_collection_authority :: Proxy "data_collection_authority"
data_collection_authority = a

day_time_charges :: Proxy "day_time_charges"
day_time_charges = a

daytime_charges_applicable_at_night :: Proxy "daytime_charges_applicable_at_night"
daytime_charges_applicable_at_night = a

daytime_charges_applied_at_night :: Proxy "daytime_charges_applied_at_night"
daytime_charges_applied_at_night = a

del_account :: Proxy "del_account"
del_account = a

delete :: Proxy "delete"
delete = a

deny_access :: Proxy "deny_access"
deny_access = a

describe_your_issue :: Proxy "describe_your_issue"
describe_your_issue = a

destination_outside_limits :: Proxy "destination_outside_limits"
destination_outside_limits = a

direct_call :: Proxy "direct_call"
direct_call = a

do_you_need_emergency_help :: Proxy "do_you_need_emergency_help"
do_you_need_emergency_help = a

download_pdf :: Proxy "download_pdf"
download_pdf = a

drag_the_map :: Proxy "drag_the_map"
drag_the_map = a

driver_pickup_charges :: Proxy "driver_pickup_charges"
driver_pickup_charges = a

driver_requested_to_cancel :: Proxy "driver_requested_to_cancel"
driver_requested_to_cancel = a

driver_was_not_reachable :: Proxy "driver_was_not_reachable"
driver_was_not_reachable = a

driver_was_rude :: Proxy "driver_was_rude"
driver_was_rude = a

drivers_can_charge_an_additional_fare_upto :: Proxy "drivers_can_charge_an_additional_fare_upto"
drivers_can_charge_an_additional_fare_upto = a

drivers_can_charge_between_the_above_range :: Proxy "drivers_can_charge_between_the_above_range"
drivers_can_charge_between_the_above_range = a

drivers_may_quote_extra_to_cover_for_traffic :: Proxy "drivers_may_quote_extra_to_cover_for_traffic"
drivers_may_quote_extra_to_cover_for_traffic = a

driver_addition_limits_are_in_increments :: Proxy "driver_addition_limits_are_in_increments"
driver_addition_limits_are_in_increments = a

drop :: Proxy "drop"
drop = a

drop_location_far_away :: Proxy "drop_location_far_away"
drop_location_far_away = a

early_end_ride_charges :: Proxy "early_end_ride_charges"
early_end_ride_charges = a

early_end_ride_charges_description :: Proxy "early_end_ride_charges_description"
early_end_ride_charges_description = a

economical :: Proxy "economical"
economical = a

edit :: Proxy "edit"
edit = a

edit_favourite :: Proxy "edit_favourite"
edit_favourite = a

email :: Proxy "email"
email = a

email_already_exists :: Proxy "email_already_exists"
email_already_exists = a

email_id :: Proxy "email_id"
email_id = a

trusted_contacs_added_successfully :: Proxy "trusted_contacs_added_successfully"
trusted_contacs_added_successfully = a

emergency_contacts :: Proxy "emergency_contacts"
emergency_contacts = a

edit_emergency_contacts :: Proxy "edit_emergency_contacts"
edit_emergency_contacts = a

emergency_help :: Proxy "emergency_help"
emergency_help = a

empty_rides :: Proxy "empty_rides"
empty_rides = a

enable_this_feature_to_choose_your_ride :: Proxy "enable_this_feature_to_choose_your_ride"
enable_this_feature_to_choose_your_ride = a

enjoy_riding_with_us :: Proxy "enjoy_riding_with_us"
enjoy_riding_with_us = a

enter_4_digit_otp :: Proxy "enter_4_digit_otp"
enter_4_digit_otp = a

enter_a_location :: Proxy "enter_a_location"
enter_a_location = a

enter_mobile_number :: Proxy "enter_mobile_number"
enter_mobile_number = a

enter_otp :: Proxy "enter_otp"
enter_otp = a

enter_your_mobile_number :: Proxy "enter_your_mobile_number"
enter_your_mobile_number = a

enter_your_name :: Proxy "enter_your_name"
enter_your_name = a

error_404 :: Proxy "error_404"
error_404 = a

error_occured_try_again :: Proxy "error_occured_try_again"
error_occured_try_again = a

estimates_changed :: Proxy "estimates_changed"
estimates_changed = a

estimates_revised_to :: Proxy "estimates_revised_to"
estimates_revised_to = a

eta_was_too_long :: Proxy "eta_was_too_long"
eta_was_too_long = a

eta_was_too_short :: Proxy "eta_was_too_short"
eta_was_too_short = a

exists_as :: Proxy "exists_as"
exists_as = a

expires_in :: Proxy "expires_in"
expires_in = a

faq :: Proxy "faq"
faq = a

fare_has_been_updated :: Proxy "fare_has_been_updated"
fare_has_been_updated = a

fare_was_high :: Proxy "fare_was_high"
fare_was_high = a

favourite :: Proxy "favourite"
favourite = a

favourite_added_successfully :: Proxy "favourite_added_successfully"
favourite_added_successfully = a

favourite_location :: Proxy "favourite_location"
favourite_location = a

favourite_removed_successfully :: Proxy "favourite_removed_successfully"
favourite_removed_successfully = a

favourite_updated_successfully :: Proxy "favourite_updated_successfully"
favourite_updated_successfully = a

favourite_your_current_location :: Proxy "favourite_your_current_location"
favourite_your_current_location = a

favourites :: Proxy "favourites"
favourites = a

female :: Proxy "female"
female = a

finding_rides_near_you :: Proxy "finding_rides_near_you"
finding_rides_near_you = a

for_other_issues_write_to_us :: Proxy "for_other_issues_write_to_us"
for_other_issues_write_to_us = a

full_name :: Proxy "full_name"
full_name = a

gender_str :: Proxy "gender_str"
gender_str = a

get_estimate_fare :: Proxy "get_estimate_fare"
get_estimate_fare = a

getting_delayed_please_wait :: Proxy "getting_delayed_please_wait"
getting_delayed_please_wait = a

getting_estimates_for_you :: Proxy "getting_estimates_for_you"
getting_estimates_for_you = a

getting_revised_estimate :: Proxy "getting_revised_estimate"
getting_revised_estimate = a

getting_started_and_faqs :: Proxy "getting_started_and_faqs"
getting_started_and_faqs = a

give_this_location_a_name :: Proxy "give_this_location_a_name"
give_this_location_a_name = a

go_back_ :: Proxy "go_back_"
go_back_ = a

go_home_ :: Proxy "go_home_"
go_home_ = a

go_to_home__ :: Proxy "go_to_home__"
go_to_home__ = a

google_map_ :: Proxy "google_map_"
google_map_ = a

got_another_ride_else_where :: Proxy "got_another_ride_else_where"
got_another_ride_else_where = a

got_it :: Proxy "got_it"
got_it = a

got_it_tell_us_more :: Proxy "got_it_tell_us_more"
got_it_tell_us_more = a

government_chagres :: Proxy "government_chagres"
government_chagres = a

grant_access :: Proxy "grant_access"
grant_access = a

cgst :: Proxy "cgst"
cgst = a

have_referral_code :: Proxy "have_referral_code"
have_referral_code = a

hatchback :: Proxy "hatchback"
hatchback = a

help_and_support :: Proxy "help_and_support"
help_and_support = a

help_us_with_your_feedback_optional :: Proxy "help_us_with_your_feedback_optional"
help_us_with_your_feedback_optional = a

help_us_with_your_reason :: Proxy "help_us_with_your_reason"
help_us_with_your_reason = a

hey :: Proxy "hey"
hey = a

home :: Proxy "home"
home = a

hope_your_ride_was_hassle_free :: Proxy "hope_your_ride_was_hassle_free"
hope_your_ride_was_hassle_free = a

how_do_you_identify_yourself :: Proxy "how_do_you_identify_yourself"
how_do_you_identify_yourself = a

how_should_we_address_you :: Proxy "how_should_we_address_you"
how_should_we_address_you = a

how_the_pricing_works :: Proxy "how_the_pricing_works"
how_the_pricing_works = a

how_this_works :: Proxy "how_this_works"
how_this_works = a

how_was_your_ride_experience :: Proxy "how_was_your_ride_experience"
how_was_your_ride_experience = a

how_was_your_ride_with :: Proxy "how_was_your_ride_with"
how_was_your_ride_with = a

actual_fare_was_higher_than_what_was_shown :: Proxy "actual_fare_was_higher_than_what_was_shown"
actual_fare_was_higher_than_what_was_shown = a

i_am_on_my_way :: Proxy "i_am_on_my_way"
i_am_on_my_way = a

i_have_arrived :: Proxy "i_have_arrived"
i_have_arrived = a

if_you_still_wanna_book_ride_click_continue_and_start_booking_the_ride :: Proxy "if_you_still_wanna_book_ride_click_continue_and_start_booking_the_ride"
if_you_still_wanna_book_ride_click_continue_and_start_booking_the_ride = a

in' :: Proxy "in'"
in' = a

in_app_tracking :: Proxy "in_app_tracking"
in_app_tracking = a

invalid_code_please_re_enter :: Proxy "invalid_code_please_re_enter"
invalid_code_please_re_enter = a

invalid_mobile_number :: Proxy "invalid_mobile_number"
invalid_mobile_number = a

invoice :: Proxy "invoice"
invoice = a

is_on_the_way :: Proxy "is_on_the_way"
is_on_the_way = a

is_waiting_at_pickup :: Proxy "is_waiting_at_pickup"
is_waiting_at_pickup = a

it_seems_to_be_a_very_busy_day :: Proxy "it_seems_to_be_a_very_busy_day"
it_seems_to_be_a_very_busy_day = a

language :: Proxy "language"
language = a

let_try_that_again :: Proxy "let_try_that_again"
let_try_that_again = a

live_stats_dashboard :: Proxy "live_stats_dashboard"
live_stats_dashboard = a

load_more :: Proxy "load_more"
load_more = a

loading :: Proxy "loading"
loading = a

location :: Proxy "location"
location = a

location_already :: Proxy "location_already"
location_already = a

location_already_exists :: Proxy "location_already_exists"
location_already_exists = a

location_already_exists_as :: Proxy "location_already_exists_as"
location_already_exists_as = a

location_unserviceable :: Proxy "location_unserviceable"
location_unserviceable = a

login_using_the_otp_sent_to :: Proxy "login_using_the_otp_sent_to"
login_using_the_otp_sent_to = a

logo :: Proxy "logo"
logo = a

logout_ :: Proxy "logout_"
logout_ = a

looking_for_you_at_pickup :: Proxy "looking_for_you_at_pickup"
looking_for_you_at_pickup = a

lost_something :: Proxy "lost_something"
lost_something = a

male :: Proxy "male"
male = a

mandatory :: Proxy "mandatory"
mandatory = a

max_char_limit_reached :: Proxy "max_char_limit_reached"
max_char_limit_reached = a

maybe_later :: Proxy "maybe_later"
maybe_later = a

message :: Proxy "message"
message = a

meters_away_from_your_destination :: Proxy "meters_away_from_your_destination"
meters_away_from_your_destination = a

min_fare_upto :: Proxy "min_fare_upto"
min_fare_upto = a

more_than :: Proxy "more_than"
more_than = a

mins_away :: Proxy "mins_away"
mins_away = a

mobile :: Proxy "mobile"
mobile = a

mobile_number_str :: Proxy "mobile_number_str"
mobile_number_str = a

my_rides :: Proxy "my_rides"
my_rides = a

name :: Proxy "name"
name = a

name_already_in_use :: Proxy "name_already_in_use"
name_already_in_use = a

navigate :: Proxy "navigate"
navigate = a

nearby :: Proxy "nearby"
nearby = a

night_time_charges :: Proxy "night_time_charges"
night_time_charges = a

no :: Proxy "no"
no = a

no_contacts_left_on_device_to_add :: Proxy "no_contacts_left_on_device_to_add"
no_contacts_left_on_device_to_add = a

no_dont :: Proxy "no_dont"
no_dont = a

no_emergency_contacts_set :: Proxy "no_emergency_contacts_set"
no_emergency_contacts_set = a

no_favourites_saved_yet :: Proxy "no_favourites_saved_yet"
no_favourites_saved_yet = a

no_more_rides :: Proxy "no_more_rides"
no_more_rides = a

no_tip :: Proxy "no_tip"
no_tip = a

nominal_fare :: Proxy "nominal_fare"
nominal_fare = a

customer_cancellation_dues :: Proxy "customer_cancellation_dues"
customer_cancellation_dues = a

not_now :: Proxy "not_now"
not_now = a

note :: Proxy "note"
note = a

notify_me :: Proxy "notify_me"
notify_me = a

of' :: Proxy "of'"
of' = a

ok_i_will_wait :: Proxy "ok_i_will_wait"
ok_i_will_wait = a

online_ :: Proxy "online_"
online_ = a

other :: Proxy "other"
other = a

others :: Proxy "others"
others = a

otp :: Proxy "otp"
otp = a

our_suggested_price_for_this_trip_is :: Proxy "our_suggested_price_for_this_trip_is"
our_suggested_price_for_this_trip_is = a

paid :: Proxy "paid"
paid = a

pay_directly_to_your_driver_using_cash_upi :: Proxy "pay_directly_to_your_driver_using_cash_upi"
pay_directly_to_your_driver_using_cash_upi = a

pay_driver_using_cash_or_upi :: Proxy "pay_driver_using_cash_or_upi"
pay_driver_using_cash_or_upi = a

pay_driver_using_cash_or_upi_ :: Proxy "pay_driver_using_cash_or_upi_"
pay_driver_using_cash_or_upi_ = a

pay_the_driver :: Proxy "pay_the_driver"
pay_the_driver = a

pay_the_driver_info :: Proxy "pay_the_driver_info"
pay_the_driver_info = a

pay_the_driver_note :: Proxy "pay_the_driver_note"
pay_the_driver_note = a

pay_via_cash_or_upi :: Proxy "pay_via_cash_or_upi"
pay_via_cash_or_upi = a

payment_method :: Proxy "payment_method"
payment_method = a

payment_method_string :: Proxy "payment_method_string"
payment_method_string = a

payment_method_string_ :: Proxy "payment_method_string_"
payment_method_string_ = a

people :: Proxy "people"
people = a

percentage_of_nominal_fare :: Proxy "percentage_of_nominal_fare"
percentage_of_nominal_fare = a

personal_details :: Proxy "personal_details"
personal_details = a

pick_up_location :: Proxy "pick_up_location"
pick_up_location = a

pick_up_location_incorrect :: Proxy "pick_up_location_incorrect"
pick_up_location_incorrect = a

pickup_and_drop :: Proxy "pickup_and_drop"
pickup_and_drop = a

pickup_charge :: Proxy "pickup_charge"
pickup_charge = a

place_call :: Proxy "place_call"
place_call = a

please_choose_your_preferred_language_to_continue :: Proxy "please_choose_your_preferred_language_to_continue"
please_choose_your_preferred_language_to_continue = a

please_come_fast_i_am_waiting :: Proxy "please_come_fast_i_am_waiting"
please_come_fast_i_am_waiting = a

please_come_soon :: Proxy "please_come_soon"
please_come_soon = a

please_pay_the_final_amount_to_the_driver_via_cash :: Proxy "please_pay_the_final_amount_to_the_driver_via_cash"
please_pay_the_final_amount_to_the_driver_via_cash = a

please_tell_us_why_you_want_to_cancel :: Proxy "please_tell_us_why_you_want_to_cancel"
please_tell_us_why_you_want_to_cancel = a

please_update_app_to_continue_service :: Proxy "please_update_app_to_continue_service"
please_update_app_to_continue_service = a

please_wait_i_will_be_there :: Proxy "please_wait_i_will_be_there"
please_wait_i_will_be_there = a

please_wait_while_in_progress :: Proxy "please_wait_while_in_progress"
please_wait_while_in_progress = a

prefer_not_to_say :: Proxy "prefer_not_to_say"
prefer_not_to_say = a

privacy_policy :: Proxy "privacy_policy"
privacy_policy = a

problem_at_our_end :: Proxy "problem_at_our_end"
problem_at_our_end = a

profile_completion :: Proxy "profile_completion"
profile_completion = a

promotion :: Proxy "promotion"
promotion = a

quote_expired :: Proxy "quote_expired"
quote_expired = a

rate_above_min_fare :: Proxy "rate_above_min_fare"
rate_above_min_fare = a

rate_card :: Proxy "rate_card"
rate_card = a

rate_your_driver :: Proxy "rate_your_driver"
rate_your_driver = a

rate_your_ride :: Proxy "rate_your_ride"
rate_your_ride = a

rate_your_ride_with :: Proxy "rate_your_ride_with"
rate_your_ride_with = a

refereal_code_discription :: Proxy "refereal_code_discription"
refereal_code_discription = a

referral_code_applied :: Proxy "referral_code_applied"
referral_code_applied = a

referral_code_successfull :: Proxy "referral_code_successfull"
referral_code_successfull = a

register_using_different_number :: Proxy "register_using_different_number"
register_using_different_number = a

remove :: Proxy "remove"
remove = a

remove_favourite :: Proxy "remove_favourite"
remove_favourite = a

repeat_ride :: Proxy "repeat_ride"
repeat_ride = a

report_an_issue :: Proxy "report_an_issue"
report_an_issue = a

report_an_issue_with_this_trip :: Proxy "report_an_issue_with_this_trip"
report_an_issue_with_this_trip = a

request_auto_ride :: Proxy "request_auto_ride"
request_auto_ride = a

request_callback :: Proxy "request_callback"
request_callback = a

request_ride :: Proxy "request_ride"
request_ride = a

request_submitted :: Proxy "request_submitted"
request_submitted = a

request_to_delete_account :: Proxy "request_to_delete_account"
request_to_delete_account = a

resend :: Proxy "resend"
resend = a

ride_completed :: Proxy "ride_completed"
ride_completed = a

ride_details :: Proxy "ride_details"
ride_details = a

ride_fare :: Proxy "ride_fare"
ride_fare = a

ride_id :: Proxy "ride_id"
ride_id = a

ride_not_serviceable :: Proxy "ride_not_serviceable"
ride_not_serviceable = a

app_not_serviceable :: Proxy "app_not_serviceable"
app_not_serviceable = a

save :: Proxy "save"
save = a

save_as :: Proxy "save_as"
save_as = a

save_place :: Proxy "save_place"
save_place = a

saved_address_helps_you_keep_your_favourite_places_handy :: Proxy "saved_address_helps_you_keep_your_favourite_places_handy"
saved_address_helps_you_keep_your_favourite_places_handy = a

saved_addresses :: Proxy "saved_addresses"
saved_addresses = a

search_again_with :: Proxy "search_again_with"
search_again_with = a

search_again_with_a_tip :: Proxy "search_again_with_a_tip"
search_again_with_a_tip = a

search_again_without_a_tip :: Proxy "search_again_without_a_tip"
search_again_without_a_tip = a

search_contacts :: Proxy "search_contacts"
search_contacts = a

select_a_ride :: Proxy "select_a_ride"
select_a_ride = a

select_an_offer :: Proxy "select_an_offer"
select_an_offer = a

select_an_offer_from_our_drivers :: Proxy "select_an_offer_from_our_drivers"
select_an_offer_from_our_drivers = a

select_an_offer_from_our_drivers_info :: Proxy "select_an_offer_from_our_drivers_info"
select_an_offer_from_our_drivers_info = a

select_contacts :: Proxy "select_contacts"
select_contacts = a

select_favourite :: Proxy "select_favourite"
select_favourite = a

select_on_map :: Proxy "select_on_map"
select_on_map = a

select_your_drop :: Proxy "select_your_drop"
select_your_drop = a

select_your_gender :: Proxy "select_your_gender"
select_your_gender = a

send_email :: Proxy "send_email"
send_email = a

service_charges :: Proxy "service_charges"
service_charges = a

set_location_on_map :: Proxy "set_location_on_map"
set_location_on_map = a

set_now :: Proxy "set_now"
set_now = a

set_up_your_account :: Proxy "set_up_your_account"
set_up_your_account = a

share_app :: Proxy "share_app"
share_app = a

share_ride_with_emergency_contacts :: Proxy "share_ride_with_emergency_contacts"
share_ride_with_emergency_contacts = a

show_all_options :: Proxy "show_all_options"
show_all_options = a

six_digit_referral_code :: Proxy "six_digit_referral_code"
six_digit_referral_code = a

skip :: Proxy "skip"
skip = a

software_license :: Proxy "software_license"
software_license = a

sorry_we_couldnt_find_any_rides :: Proxy "sorry_we_couldnt_find_any_rides"
sorry_we_couldnt_find_any_rides = a

sort_by :: Proxy "sort_by"
sort_by = a

spacious :: Proxy "spacious"
spacious = a

start_ :: Proxy "start_"
start_ = a

start_your_chat_using_these_quick_chat_suggestions :: Proxy "start_your_chat_using_these_quick_chat_suggestions"
start_your_chat_using_these_quick_chat_suggestions = a

start_your_chat_with_the_driver :: Proxy "start_your_chat_with_the_driver"
start_your_chat_with_the_driver = a

steps_to_complete :: Proxy "steps_to_complete"
steps_to_complete = a

subject :: Proxy "subject"
subject = a

submit :: Proxy "submit"
submit = a

submit_feedback :: Proxy "submit_feedback"
submit_feedback = a

successful_onboard :: Proxy "successful_onboard"
successful_onboard = a

support :: Proxy "support"
support = a

suv :: Proxy "suv"
suv = a

sedan :: Proxy "sedan"
sedan = a

t_and_c_a :: Proxy "t_and_c_a"
t_and_c_a = a

terms_and_conditions :: Proxy "terms_and_conditions"
terms_and_conditions = a

thank_you_for_writing :: Proxy "thank_you_for_writing"
thank_you_for_writing = a

thank_you_for_writing_to_us :: Proxy "thank_you_for_writing_to_us"
thank_you_for_writing_to_us = a

thank_your_driver :: Proxy "thank_your_driver"
thank_your_driver = a

the_trip_is_very_short_and_just_take :: Proxy "the_trip_is_very_short_and_just_take"
the_trip_is_very_short_and_just_take = a

tip :: Proxy "tip"
tip = a

to_the :: Proxy "to_the"
to_the = a

total_amount :: Proxy "total_amount"
total_amount = a

total_fare_may_change_due_to_change_in_route :: Proxy "total_fare_may_change_due_to_change_in_route"
total_fare_may_change_due_to_change_in_route = a

total_paid :: Proxy "total_paid"
total_paid = a

track_live_location_using :: Proxy "track_live_location_using"
track_live_location_using = a

trip_charges :: Proxy "trip_charges"
trip_charges = a

trip_details_ :: Proxy "trip_details_"
trip_details_ = a

trip_id :: Proxy "trip_id"
trip_id = a

try_again :: Proxy "try_again"
try_again = a

try_again_with :: Proxy "try_again_with"
try_again_with = a

try_again_with_a_tip :: Proxy "try_again_with_a_tip"
try_again_with_a_tip = a

try_again_without_tip :: Proxy "try_again_without_tip"
try_again_without_tip = a

try_connecting_with_the_driver :: Proxy "try_connecting_with_the_driver"
try_connecting_with_the_driver = a

try_looking_for_rides_again :: Proxy "try_looking_for_rides_again"
try_looking_for_rides_again = a

unreachable_please_call_back :: Proxy "unreachable_please_call_back"
unreachable_please_call_back = a

update :: Proxy "update"
update = a

update_personal_details :: Proxy "update_personal_details"
update_personal_details = a

setup_now :: Proxy "setup_now"
setup_now = a

update_required :: Proxy "update_required"
update_required = a

use_current_location :: Proxy "use_current_location"
use_current_location = a

user :: Proxy "user"
user = a

verifying_otp :: Proxy "verifying_otp"
verifying_otp = a

view_all_rides :: Proxy "view_all_rides"
view_all_rides = a

view_breakdown :: Proxy "view_breakdown"
view_breakdown = a

view_details :: Proxy "view_details"
view_details = a

view_invoice :: Proxy "view_invoice"
view_invoice = a

visit_my_rides_section_for_ride_specific_complaints :: Proxy "visit_my_rides_section_for_ride_specific_complaints"
visit_my_rides_section_for_ride_specific_complaints = a

wait_time :: Proxy "wait_time"
wait_time = a

wait_time_too_long :: Proxy "wait_time_too_long"
wait_time_too_long = a

waiting_charge :: Proxy "waiting_charge"
waiting_charge = a

waiting_charge_description :: Proxy "waiting_charge_description"
waiting_charge_description = a

waiting_charge_ratecard_description :: Proxy "waiting_charge_ratecard_description"
waiting_charge_ratecard_description = a

waiting_charge_info :: Proxy "waiting_charge_info"
waiting_charge_info = a

we_have_received_your_issue :: Proxy "we_have_received_your_issue"
we_have_received_your_issue = a

we_have_received_your_issue_well_reach_out_to_you_in_sometime :: Proxy "we_have_received_your_issue_well_reach_out_to_you_in_sometime"
we_have_received_your_issue_well_reach_out_to_you_in_sometime = a

we_need_access_to_your_location :: Proxy "we_need_access_to_your_location"
we_need_access_to_your_location = a

we_will_delete_your_account :: Proxy "we_will_delete_your_account"
we_will_delete_your_account = a

welcome_text :: Proxy "welcome_text"
welcome_text = a

where_to :: Proxy "where_to"
where_to = a

work :: Proxy "work"
work = a

write_a_comment :: Proxy "write_a_comment"
write_a_comment = a

write_to_us :: Proxy "write_to_us"
write_to_us = a

wrong_otp :: Proxy "wrong_otp"
wrong_otp = a

yes :: Proxy "yes"
yes = a

yes_cancel_search :: Proxy "yes_cancel_search"
yes_cancel_search = a

yes_delete_it :: Proxy "yes_delete_it"
yes_delete_it = a

yes_remove :: Proxy "yes_remove"
yes_remove = a

yes_try_again :: Proxy "yes_try_again"
yes_try_again = a

you_are_about_to_call_namma_yatri_support :: Proxy "you_are_about_to_call_namma_yatri_support"
you_are_about_to_call_namma_yatri_support = a

you_are_about_to_call_nearest_emergency_centre :: Proxy "you_are_about_to_call_nearest_emergency_centre"
you_are_about_to_call_nearest_emergency_centre = a

you_are_offline :: Proxy "you_are_offline"
you_are_offline = a

you_can_cancel_ride :: Proxy "you_can_cancel_ride"
you_can_cancel_ride = a

you_can_describe_the_issue_you_faced_here :: Proxy "you_can_describe_the_issue_you_faced_here"
you_can_describe_the_issue_you_faced_here = a

you_can_get_referral_code_from_driver :: Proxy "you_can_get_referral_code_from_driver"
you_can_get_referral_code_from_driver = a

you_can_take_a_walk_or_continue_with_ride_booking :: Proxy "you_can_take_a_walk_or_continue_with_ride_booking"
you_can_take_a_walk_or_continue_with_ride_booking = a

you_have_ride_offers_are_you_sure_you_want_to_cancel :: Proxy "you_have_ride_offers_are_you_sure_you_want_to_cancel"
you_have_ride_offers_are_you_sure_you_want_to_cancel = a

you_havent_taken_a_trip_yet :: Proxy "you_havent_taken_a_trip_yet"
you_havent_taken_a_trip_yet = a

you_havent_taken_a_trip_yet_in_past_hours :: Proxy "you_havent_taken_a_trip_yet_in_past_hours"
you_havent_taken_a_trip_yet_in_past_hours = a

you_rated :: Proxy "you_rated"
you_rated = a

you_will_be_asked_to_select_contacts :: Proxy "you_will_be_asked_to_select_contacts"
you_will_be_asked_to_select_contacts = a

your_email_id :: Proxy "your_email_id"
your_email_id = a

location_permission_subtitle :: Proxy "location_permission_subtitle"
location_permission_subtitle = a

your_number_will_be_visible_to_the_driver_use_if_not_calling_from_registered_number :: Proxy "your_number_will_be_visible_to_the_driver_use_if_not_calling_from_registered_number"
your_number_will_be_visible_to_the_driver_use_if_not_calling_from_registered_number = a

your_number_will_not_be_shown_to_the_driver_the_call_will_be_recorded_for_compliance :: Proxy "your_number_will_not_be_shown_to_the_driver_the_call_will_be_recorded_for_compliance"
your_number_will_not_be_shown_to_the_driver_the_call_will_be_recorded_for_compliance = a

your_recent_ride :: Proxy "your_recent_ride"
your_recent_ride = a

your_ride_has_started :: Proxy "your_ride_has_started"
your_ride_has_started = a

your_ride_is_now_complete :: Proxy "your_ride_is_now_complete"
your_ride_is_now_complete = a

your_rides :: Proxy "your_rides"
your_rides = a

your_trip_is_too_short_you_are_just :: Proxy "your_trip_is_too_short_you_are_just"
your_trip_is_too_short_you_are_just = a

download_invoice :: Proxy "download_invoice"
download_invoice = a

was_your_call_successful :: Proxy "was_your_call_successful"
was_your_call_successful = a

driver_additions :: Proxy "driver_additions"
driver_additions = a

fare_update_policy :: Proxy "fare_update_policy"
fare_update_policy = a

driver_additions_optional :: Proxy "driver_additions_optional"
driver_additions_optional = a

the_driver_may_quote_extra_to_cover_for_traffic :: Proxy "the_driver_may_quote_extra_to_cover_for_traffic"
the_driver_may_quote_extra_to_cover_for_traffic = a

driver_additions_are_calculated_at_rate :: Proxy "driver_additions_are_calculated_at_rate"
driver_additions_are_calculated_at_rate = a

driver_may_not_charge_this_additional_fare :: Proxy "driver_may_not_charge_this_additional_fare"
driver_may_not_charge_this_additional_fare = a

you_may_see_an_updated_final_fare_due_to_any_of_the_below_reasons :: Proxy "you_may_see_an_updated_final_fare_due_to_any_of_the_below_reasons"
you_may_see_an_updated_final_fare_due_to_any_of_the_below_reasons = a

reason_change_in_route_a :: Proxy "reason_change_in_route_a"
reason_change_in_route_a = a

reason_change_in_route_b :: Proxy "reason_change_in_route_b"
reason_change_in_route_b = a

go_to_zone :: Proxy "go_to_zone"
go_to_zone = a

request_received_we_will_call_you_back_soon :: Proxy "request_received_we_will_call_you_back_soon"
request_received_we_will_call_you_back_soon = a

contact_removed_successfully :: Proxy "contact_removed_successfully"
contact_removed_successfully = a

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

recommended :: Proxy "recommended"
recommended = a

complete_your_profile_for_a_personalised_ride_experience :: Proxy "complete_your_profile_for_a_personalised_ride_experience"
complete_your_profile_for_a_personalised_ride_experience = a

complete_your_namma_safety_setup_for_safe_ride_experience :: Proxy "complete_your_namma_safety_setup_for_safe_ride_experience"
complete_your_namma_safety_setup_for_safe_ride_experience = a

update_now :: Proxy "update_now"
update_now = a

we_would_appreciate_your_feedback :: Proxy "we_would_appreciate_your_feedback"
we_would_appreciate_your_feedback = a

reason_for_deleting_account :: Proxy "reason_for_deleting_account"
reason_for_deleting_account = a

submit_request :: Proxy "submit_request"
submit_request = a

please_enter_a_valid_email :: Proxy "please_enter_a_valid_email"
please_enter_a_valid_email = a

we_would_appreciate_your_reasoning :: Proxy "we_would_appreciate_your_reasoning"
we_would_appreciate_your_reasoning = a

ok_got_it :: Proxy "ok_got_it"
ok_got_it = a

wait_for_driver :: Proxy "wait_for_driver"
wait_for_driver = a

no_longer_require_a_ride_due_to_change_in_plans :: Proxy "no_longer_require_a_ride_due_to_change_in_plans"
no_longer_require_a_ride_due_to_change_in_plans = a

cancelling_as_i_got_a_ride_on_another_app :: Proxy "cancelling_as_i_got_a_ride_on_another_app"
cancelling_as_i_got_a_ride_on_another_app = a

driver_location_wasnt_changing_on_the_map :: Proxy "driver_location_wasnt_changing_on_the_map"
driver_location_wasnt_changing_on_the_map = a

driver_was_taking_too_long_to_reach_the_pickup_location :: Proxy "driver_was_taking_too_long_to_reach_the_pickup_location"
driver_was_taking_too_long_to_reach_the_pickup_location = a

the_pickup_location_entered_was_wrong :: Proxy "the_pickup_location_entered_was_wrong"
the_pickup_location_entered_was_wrong = a

your_driver_is_just :: Proxy "your_driver_is_just"
your_driver_is_just = a

m_away :: Proxy "m_away"
m_away = a

driver_has_already_travelled :: Proxy "driver_has_already_travelled"
driver_has_already_travelled = a

please_contact_the_driver_before_cancelling :: Proxy "please_contact_the_driver_before_cancelling"
please_contact_the_driver_before_cancelling = a

confirm_with_your_driver :: Proxy "confirm_with_your_driver"
confirm_with_your_driver = a

change_of_plans :: Proxy "change_of_plans"
change_of_plans = a

driver_is_not_moving :: Proxy "driver_is_not_moving"
driver_is_not_moving = a

wrong_pickup_location :: Proxy "wrong_pickup_location"
wrong_pickup_location = a

different_vehicle_number :: Proxy "different_vehicle_number"
different_vehicle_number = a

vehicle_number_is_different_from_what_is_shown_in_the_app :: Proxy "vehicle_number_is_different_from_what_is_shown_in_the_app"
vehicle_number_is_different_from_what_is_shown_in_the_app = a

different_auto :: Proxy "different_auto"
different_auto = a

different_cab :: Proxy "different_cab"
different_cab = a

driver_might_be_taking_alternate_route :: Proxy "driver_might_be_taking_alternate_route"
driver_might_be_taking_alternate_route = a

driver_is_not_moving_q :: Proxy "driver_is_not_moving_q"
driver_is_not_moving_q = a

would_you_like_to_check_with_the_driver_before_cancelling :: Proxy "would_you_like_to_check_with_the_driver_before_cancelling"
would_you_like_to_check_with_the_driver_before_cancelling = a

driver_is_near_your_location :: Proxy "driver_is_near_your_location"
driver_is_near_your_location = a

some_other_reason :: Proxy "some_other_reason"
some_other_reason = a

location_permission_subtitle_new_user :: Proxy "location_permission_subtitle_new_user"
location_permission_subtitle_new_user = a

metro_ride :: Proxy "metro_ride"
metro_ride = a

go_back_text :: Proxy "go_back_text"
go_back_text = a

driver_preferred_your_special_request_and_is_just :: Proxy "driver_preferred_your_special_request_and_is_just"
driver_preferred_your_special_request_and_is_just = a

driver_preferred_your_special_request :: Proxy "driver_preferred_your_special_request"
driver_preferred_your_special_request = a

and_has_travelled :: Proxy "and_has_travelled"
and_has_travelled = a

please_find_revised_fare_estimate :: Proxy "please_find_revised_fare_estimate"
please_find_revised_fare_estimate = a

fare_estimate :: Proxy "fare_estimate"
fare_estimate = a

tip_selected :: Proxy "tip_selected"
tip_selected = a

add_a_tip_to_find_a_ride_quicker :: Proxy "add_a_tip_to_find_a_ride_quicker"
add_a_tip_to_find_a_ride_quicker = a

it_seems_to_be_taking_longer_than_usual :: Proxy "it_seems_to_be_taking_longer_than_usual"
it_seems_to_be_taking_longer_than_usual = a

continue_search_with :: Proxy "continue_search_with"
continue_search_with = a

continuing_search_with :: Proxy "continuing_search_with"
continuing_search_with = a

searching_with :: Proxy "searching_with"
searching_with = a

the_driver_preferred_your_special_request_and_is_already_on_the_way_to_your_location :: Proxy "the_driver_preferred_your_special_request_and_is_already_on_the_way_to_your_location"
the_driver_preferred_your_special_request_and_is_already_on_the_way_to_your_location = a

driver_is_already_on_the_way_to_your_location :: Proxy "driver_is_already_on_the_way_to_your_location"
driver_is_already_on_the_way_to_your_location = a

allow_location_access :: Proxy "allow_location_access"
allow_location_access = a

message_from_driver :: Proxy "message_from_driver"
message_from_driver = a

reply :: Proxy "reply"
reply = a

name_should_be_more_than_2_characters :: Proxy "name_should_be_more_than_2_characters"
name_should_be_more_than_2_characters = a

this_field_is_required :: Proxy "this_field_is_required"
this_field_is_required = a

email_exists_already :: Proxy "email_exists_already"
email_exists_already = a

okay_got_it :: Proxy "okay_got_it"
okay_got_it = a

call_namma_yatri_support :: Proxy "call_namma_yatri_support"
call_namma_yatri_support = a

call_112 :: Proxy "call_112"
call_112 = a

seats :: Proxy "seats"
seats = a

otp_page_has_been_expired_please_request_otp_again :: Proxy "otp_page_has_been_expired_please_request_otp_again"
otp_page_has_been_expired_please_request_otp_again = a

otp_entering_limit_exhausted_please_try_again_later :: Proxy "otp_entering_limit_exhausted_please_try_again_later"
otp_entering_limit_exhausted_please_try_again_later = a

too_many_login_attempts_please_try_again_later :: Proxy "too_many_login_attempts_please_try_again_later"
too_many_login_attempts_please_try_again_later = a

something_went_wrong_please_try_again :: Proxy "something_went_wrong_please_try_again"
something_went_wrong_please_try_again = a

sorry_limit_exceeded_you_cant_add_any_more_favourites :: Proxy "sorry_limit_exceeded_you_cant_add_any_more_favourites"
sorry_limit_exceeded_you_cant_add_any_more_favourites = a

it_seems_like_you_have_an_ongoing_ride_ :: Proxy "it_seems_like_you_have_an_ongoing_ride_"
it_seems_like_you_have_an_ongoing_ride_ = a

cancellation_unsuccessfull_please_try_again :: Proxy "cancellation_unsuccessfull_please_try_again"
cancellation_unsuccessfull_please_try_again = a

no_driver_available_at_the_moment_please_try_again :: Proxy "no_driver_available_at_the_moment_please_try_again"
no_driver_available_at_the_moment_please_try_again = a

otp_for_the_jatri_sathi_zone_has_been_expired_please_try_looking_again :: Proxy "otp_for_the_jatri_sathi_zone_has_been_expired_please_try_looking_again"
otp_for_the_jatri_sathi_zone_has_been_expired_please_try_looking_again = a

no_contacts_found_on_the_device_to_be_added :: Proxy "no_contacts_found_on_the_device_to_be_added"
no_contacts_found_on_the_device_to_be_added = a

please_enable_contacts_permission_to_proceed :: Proxy "please_enable_contacts_permission_to_proceed"
please_enable_contacts_permission_to_proceed = a

limit_reached_3_of_3_emergency_contacts_already_added :: Proxy "limit_reached_3_of_3_emergency_contacts_already_added"
limit_reached_3_of_3_emergency_contacts_already_added = a

invalid_contact_format :: Proxy "invalid_contact_format"
invalid_contact_format = a

otp_resent_limit_exhausted_please_try_again_later :: Proxy "otp_resent_limit_exhausted_please_try_again_later"
otp_resent_limit_exhausted_please_try_again_later = a

rate_your_experience :: Proxy "rate_your_experience"
rate_your_experience = a

report_issue_ :: Proxy "report_issue_"
report_issue_ = a

done :: Proxy "done"
done = a

please_tell_us_what_went_wrong :: Proxy "please_tell_us_what_went_wrong"
please_tell_us_what_went_wrong = a

your_feedback_helps_us :: Proxy "your_feedback_helps_us"
your_feedback_helps_us = a

did_you_face_any_issue :: Proxy "did_you_face_any_issue"
did_you_face_any_issue = a

did_the_driver_offer_assistance :: Proxy "did_the_driver_offer_assistance"
did_the_driver_offer_assistance = a

was_the_driver_understanding_of_your_needs :: Proxy "was_the_driver_understanding_of_your_needs"
was_the_driver_understanding_of_your_needs = a

we_noticed_your_ride_ended_away :: Proxy "we_noticed_your_ride_ended_away"
we_noticed_your_ride_ended_away = a

get_callback_from_us :: Proxy "get_callback_from_us"
get_callback_from_us = a

driver_was_not_ready_to_go :: Proxy "driver_was_not_ready_to_go"
driver_was_not_ready_to_go = a

asking_for_more_money :: Proxy "asking_for_more_money"
asking_for_more_money = a

vehicle_broken :: Proxy "vehicle_broken"
vehicle_broken = a

we_will_give_you_callback :: Proxy "we_will_give_you_callback"
we_will_give_you_callback = a

your_issue_has_been_reported :: Proxy "your_issue_has_been_reported"
your_issue_has_been_reported = a

issue_report_already_exists :: Proxy "issue_report_already_exists"
issue_report_already_exists = a

otp_resent_successfully :: Proxy "otp_resent_successfully"
otp_resent_successfully = a

description_should_be_more_than_10_alphabetic_characters :: Proxy "description_should_be_more_than_10_alphabetic_characters"
description_should_be_more_than_10_alphabetic_characters = a

incorrect_otp_please_try_again :: Proxy "incorrect_otp_please_try_again"
incorrect_otp_please_try_again = a

n_more_attempts_left :: Proxy "n_more_attempts_left"
n_more_attempts_left = a

go_to_selected_pickup_spot :: Proxy "go_to_selected_pickup_spot"
go_to_selected_pickup_spot = a

go_to_selected_pickup_spot_as_autos_are_restricted :: Proxy "go_to_selected_pickup_spot_as_autos_are_restricted"
go_to_selected_pickup_spot_as_autos_are_restricted = a

unprofessional_driver :: Proxy "unprofessional_driver"
unprofessional_driver = a

rash_driving :: Proxy "rash_driving"
rash_driving = a

driver_charged_more :: Proxy "driver_charged_more"
driver_charged_more = a

uncomfortable_auto :: Proxy "uncomfortable_auto"
uncomfortable_auto = a

uncomfortable_cab :: Proxy "uncomfortable_cab"
uncomfortable_cab = a

trip_got_delayed :: Proxy "trip_got_delayed"
trip_got_delayed = a

felt_unsafe :: Proxy "felt_unsafe"
felt_unsafe = a

polite_driver :: Proxy "polite_driver"
polite_driver = a

expert_driving :: Proxy "expert_driving"
expert_driving = a

safe_ride :: Proxy "safe_ride"
safe_ride = a

clean_auto :: Proxy "clean_auto"
clean_auto = a

clean_cab :: Proxy "clean_cab"
clean_cab = a

on_time :: Proxy "on_time"
on_time = a

skilled_navigator :: Proxy "skilled_navigator"
skilled_navigator = a

rude_driver :: Proxy "rude_driver"
rude_driver = a

too_many_calls :: Proxy "too_many_calls"
too_many_calls = a

reckless_driving :: Proxy "reckless_driving"
reckless_driving = a

late_drop_off :: Proxy "late_drop_off"
late_drop_off = a

late_pick_up :: Proxy "late_pick_up"
late_pick_up = a

poor_experience :: Proxy "poor_experience"
poor_experience = a

terrible_experience :: Proxy "terrible_experience"
terrible_experience = a

needs_improvement :: Proxy "needs_improvement"
needs_improvement = a

amazing :: Proxy "amazing"
amazing = a

almost_perfect :: Proxy "almost_perfect"
almost_perfect = a

asked_for_extra_fare :: Proxy "asked_for_extra_fare"
asked_for_extra_fare = a

anything_that_you_would_like_to_tell_us :: Proxy "anything_that_you_would_like_to_tell_us"
anything_that_you_would_like_to_tell_us = a

platform_fee :: Proxy "platform_fee"
platform_fee = a

finding_quotes_text :: Proxy "finding_quotes_text"
finding_quotes_text = a

please_wait :: Proxy "please_wait"
please_wait = a

pay_driver_using_wallet :: Proxy "pay_driver_using_wallet"
pay_driver_using_wallet = a

faster :: Proxy "faster"
faster = a

new_ :: Proxy "new_"
new_ = a

sgst :: Proxy "sgst"
sgst = a

otp_expired :: Proxy "otp_expired"
otp_expired = a

otp_expired_description :: Proxy "otp_expired_description"
otp_expired_description = a

platform_gst :: Proxy "platform_gst"
platform_gst = a

misc_waiting_charge :: Proxy "misc_waiting_charge"
misc_waiting_charge = a

taxi_from_zone :: Proxy "taxi_from_zone"
taxi_from_zone = a

taxi :: Proxy "taxi"
taxi = a

ac :: Proxy "ac"
ac = a

non_ac :: Proxy "non_ac"
non_ac = a

ac_taxi :: Proxy "ac_taxi"
ac_taxi = a

non_ac_taxi :: Proxy "non_ac_taxi"
non_ac_taxi = a

get_otp_via_whatsapp :: Proxy "get_otp_via_whatsapp"
get_otp_via_whatsapp = a

or :: Proxy "or"
or = a

helps_driver_confirm_its_you :: Proxy "helps_driver_confirm_its_you"
helps_driver_confirm_its_you = a

lets_get_you_trip_ready :: Proxy "lets_get_you_trip_ready"
lets_get_you_trip_ready = a

got_an_otp :: Proxy "got_an_otp"
got_an_otp = a

just_one_last_thing :: Proxy "just_one_last_thing"
just_one_last_thing = a

toll_charges_will_be_extra :: Proxy "toll_charges_will_be_extra"
toll_charges_will_be_extra = a

auto_rickshaw :: Proxy "auto_rickshaw"
auto_rickshaw = a

cabs_available :: Proxy "cabs_available"
cabs_available = a

general_disability_description :: Proxy "general_disability_description"
general_disability_description = a

pi_pointer_1 :: Proxy "pi_pointer_1"
pi_pointer_1 = a

pi_pointer_2 :: Proxy "pi_pointer_2"
pi_pointer_2 = a

vi_pointer_1 :: Proxy "vi_pointer_1"
vi_pointer_1 = a

vi_pointer_2 :: Proxy "vi_pointer_2"
vi_pointer_2 = a

hi_pointer_1 :: Proxy "hi_pointer_1"
hi_pointer_1 = a

hi_pointer_2 :: Proxy "hi_pointer_2"
hi_pointer_2 = a

accessibility_text :: Proxy "accessibility_text"
accessibility_text = a

to_cater_your_specific_needs :: Proxy "to_cater_your_specific_needs"
to_cater_your_specific_needs = a

special_assistance :: Proxy "special_assistance"
special_assistance = a

select_the_condition_that_is_applicable :: Proxy "select_the_condition_that_is_applicable"
select_the_condition_that_is_applicable = a

disability_claimer_text :: Proxy "disability_claimer_text"
disability_claimer_text = a

are_you_a_person_with_disability :: Proxy "are_you_a_person_with_disability"
are_you_a_person_with_disability = a

do_you_neeed_special_assistance :: Proxy "do_you_neeed_special_assistance"
do_you_neeed_special_assistance = a

assistance_required :: Proxy "assistance_required"
assistance_required = a

no_disability :: Proxy "no_disability"
no_disability = a

learn_how_text :: Proxy "learn_how_text"
learn_how_text = a

update_profile :: Proxy "update_profile"
update_profile = a

now_get_assisted_rides :: Proxy "now_get_assisted_rides"
now_get_assisted_rides = a

sent_otp_via_sms :: Proxy "sent_otp_via_sms"
sent_otp_via_sms = a

sent_otp_via_whatsapp :: Proxy "sent_otp_via_whatsapp"
sent_otp_via_whatsapp = a

please_enable_location_permission :: Proxy "please_enable_location_permission"
please_enable_location_permission = a

enable_location_permission_to :: Proxy "enable_location_permission_to"
enable_location_permission_to = a

ac_suv :: Proxy "ac_suv"
ac_suv = a

ac_cab :: Proxy "ac_cab"
ac_cab = a

ride_type :: Proxy "ride_type"
ride_type = a

ernakulam_limit_charge :: Proxy "ernakulam_limit_charge"
ernakulam_limit_charge = a

select_location_on_map :: Proxy "select_location_on_map"
select_location_on_map = a

download_driver_receipt :: Proxy "download_driver_receipt"
download_driver_receipt = a

view_driver_receipt :: Proxy "view_driver_receipt"
view_driver_receipt = a

driver_receipt :: Proxy "driver_receipt"
driver_receipt = a

help :: Proxy "help"
help = a

fare_info_text :: Proxy "fare_info_text"
fare_info_text = a

educational_pop_up_slide_1_title :: Proxy "educational_pop_up_slide_1_title"
educational_pop_up_slide_1_title = a

educational_pop_up_slide_2_title :: Proxy "educational_pop_up_slide_2_title"
educational_pop_up_slide_2_title = a

educational_pop_up_slide_3_title :: Proxy "educational_pop_up_slide_3_title"
educational_pop_up_slide_3_title = a

educational_pop_up_slide_4_title :: Proxy "educational_pop_up_slide_4_title"
educational_pop_up_slide_4_title = a

educational_pop_up_slide_5_title :: Proxy "educational_pop_up_slide_5_title"
educational_pop_up_slide_5_title = a

educational_pop_up_slide_1_subtitle :: Proxy "educational_pop_up_slide_1_subtitle"
educational_pop_up_slide_1_subtitle = a

educational_pop_up_slide_2_subtitle :: Proxy "educational_pop_up_slide_2_subtitle"
educational_pop_up_slide_2_subtitle = a

educational_pop_up_slide_3_subtitle :: Proxy "educational_pop_up_slide_3_subtitle"
educational_pop_up_slide_3_subtitle = a

educational_pop_up_slide_4_subtitle :: Proxy "educational_pop_up_slide_4_subtitle"
educational_pop_up_slide_4_subtitle = a

educational_pop_up_slide_5_subtitle :: Proxy "educational_pop_up_slide_5_subtitle"
educational_pop_up_slide_5_subtitle = a

inclusive_and_accessible :: Proxy "inclusive_and_accessible"
inclusive_and_accessible = a

you_seem_to_be_far_from_pick_up :: Proxy "you_seem_to_be_far_from_pick_up"
you_seem_to_be_far_from_pick_up = a

are_you_sure_you_want_to_proceed_with_the_booking :: Proxy "are_you_sure_you_want_to_proceed_with_the_booking"
are_you_sure_you_want_to_proceed_with_the_booking = a

my_tickets :: Proxy "my_tickets"
my_tickets = a

something_went_wrong_try_again_later :: Proxy "something_went_wrong_try_again_later"
something_went_wrong_try_again_later = a

you_can_book_tickets_to_the_zoo_by_clicking_the_button :: Proxy "you_can_book_tickets_to_the_zoo_by_clicking_the_button"
you_can_book_tickets_to_the_zoo_by_clicking_the_button = a

charges_applicable_after_3_mins :: Proxy "charges_applicable_after_3_mins"
charges_applicable_after_3_mins = a

waiting_at_pickup :: Proxy "waiting_at_pickup"
waiting_at_pickup = a

reaching_your_destination_in_ :: Proxy "reaching_your_destination_in_"
reaching_your_destination_in_ = a

learn_more :: Proxy "learn_more"
learn_more = a

pickup :: Proxy "pickup"
pickup = a

pay_by_cash_or_upi :: Proxy "pay_by_cash_or_upi"
pay_by_cash_or_upi = a

wait_timer :: Proxy "wait_timer"
wait_timer = a

how_long_driver_waited_for_pickup :: Proxy "how_long_driver_waited_for_pickup"
how_long_driver_waited_for_pickup = a

you_will_pay_for_every_minute :: Proxy "you_will_pay_for_every_minute"
you_will_pay_for_every_minute = a

chat_with :: Proxy "chat_with"
chat_with = a

quick :: Proxy "quick"
quick = a

chats :: Proxy "chats"
chats = a

replies :: Proxy "replies"
replies = a

namma_safety :: Proxy "namma_safety"
namma_safety = a

you_sent :: Proxy "you_sent"
you_sent = a

message_your_driver :: Proxy "message_your_driver"
message_your_driver = a

check_in_with_your_driver :: Proxy "check_in_with_your_driver"
check_in_with_your_driver = a

check_in_with_your_em :: Proxy "check_in_with_your_em"
check_in_with_your_em = a

track_on_google_maps :: Proxy "track_on_google_maps"
track_on_google_maps = a

otp_expire_timer :: Proxy "otp_expire_timer"
otp_expire_timer = a

shows_for_how_long_your_otp_ :: Proxy "shows_for_how_long_your_otp_"
shows_for_how_long_your_otp_ = a

if_your_otp_expires_ :: Proxy "if_your_otp_expires_"
if_your_otp_expires_ = a

you_have_reached_destination :: Proxy "you_have_reached_destination"
you_have_reached_destination = a

places_you_might_like_to_go_to :: Proxy "places_you_might_like_to_go_to"
places_you_might_like_to_go_to = a

suggested_destination :: Proxy "suggested_destination"
suggested_destination = a

recent_rides :: Proxy "recent_rides"
recent_rides = a

one_click_booking_for_your_favourite_journeys :: Proxy "one_click_booking_for_your_favourite_journeys"
one_click_booking_for_your_favourite_journeys = a

view_more :: Proxy "view_more"
view_more = a

view_less :: Proxy "view_less"
view_less = a

have_a_refferal :: Proxy "have_a_refferal"
have_a_refferal = a

your_suggested_destinations_and_recent_rides_will_appear_here :: Proxy "your_suggested_destinations_and_recent_rides_will_appear_here"
your_suggested_destinations_and_recent_rides_will_appear_here = a

welcome_to_namma_yatri_ :: Proxy "welcome_to_namma_yatri_"
welcome_to_namma_yatri_ = a

book_and_move :: Proxy "book_and_move"
book_and_move = a

anywhere_in_the_city :: Proxy "anywhere_in_the_city"
anywhere_in_the_city = a

checkout_our_live_stats :: Proxy "checkout_our_live_stats"
checkout_our_live_stats = a

most_loved_app :: Proxy "most_loved_app"
most_loved_app = a

pickup_ :: Proxy "pickup_"
pickup_ = a

past_searches :: Proxy "past_searches"
past_searches = a

search_results :: Proxy "search_results"
search_results = a

edit_destination :: Proxy "edit_destination"
edit_destination = a

requesting_ride_in :: Proxy "requesting_ride_in"
requesting_ride_in = a

confirm_fare :: Proxy "confirm_fare"
confirm_fare = a

request_change :: Proxy "request_change"
request_change = a

requesting_ride :: Proxy "requesting_ride"
requesting_ride = a

tap_here_to_stop_auto_requesting :: Proxy "tap_here_to_stop_auto_requesting"
tap_here_to_stop_auto_requesting = a

powered_by :: Proxy "powered_by"
powered_by = a

book_your_ride :: Proxy "book_your_ride"
book_your_ride = a

start_typing_to_search_places :: Proxy "start_typing_to_search_places"
start_typing_to_search_places = a

fare_updated_with_charges :: Proxy "fare_updated_with_charges"
fare_updated_with_charges = a

fare_updated_with_shorter_dist :: Proxy "fare_updated_with_shorter_dist"
fare_updated_with_shorter_dist = a

fare_updated_with_longer_dist :: Proxy "fare_updated_with_longer_dist"
fare_updated_with_longer_dist = a

fare_updated_with_charges_shorter_dist :: Proxy "fare_updated_with_charges_shorter_dist"
fare_updated_with_charges_shorter_dist = a

fare_updated_with_charges_longer_dist :: Proxy "fare_updated_with_charges_longer_dist"
fare_updated_with_charges_longer_dist = a

did_you_have_a_safe_journey :: Proxy "did_you_have_a_safe_journey"
did_you_have_a_safe_journey = a

trip_was_safe_and_worry_free :: Proxy "trip_was_safe_and_worry_free"
trip_was_safe_and_worry_free = a

driver_behaved_inappropriately :: Proxy "driver_behaved_inappropriately"
driver_behaved_inappropriately = a

i_did_not_feel_safe :: Proxy "i_did_not_feel_safe"
i_did_not_feel_safe = a

looking_for_another_ride :: Proxy "looking_for_another_ride"
looking_for_another_ride = a

the_ride_had_been_cancelled_we_are_finding_you_another :: Proxy "the_ride_had_been_cancelled_we_are_finding_you_another"
the_ride_had_been_cancelled_we_are_finding_you_another = a

enjoy_the_ride :: Proxy "enjoy_the_ride"
enjoy_the_ride = a

ride_started :: Proxy "ride_started"
ride_started = a

discover_awesome_spots_tailored_just_for_you :: Proxy "discover_awesome_spots_tailored_just_for_you"
discover_awesome_spots_tailored_just_for_you = a

smart :: Proxy "smart"
smart = a

one_click :: Proxy "one_click"
one_click = a

not_serviceable :: Proxy "not_serviceable"
not_serviceable = a

we_are_not_live_in_your_area :: Proxy "we_are_not_live_in_your_area"
we_are_not_live_in_your_area = a

account_blocked :: Proxy "account_blocked"
account_blocked = a

you_can_still_access :: Proxy "you_can_still_access"
you_can_still_access = a

facing_problem_with_app :: Proxy "facing_problem_with_app"
facing_problem_with_app = a

tap_here_to_report :: Proxy "tap_here_to_report"
tap_here_to_report = a

confirm_your_ride :: Proxy "confirm_your_ride"
confirm_your_ride = a

ride_scheduled :: Proxy "ride_scheduled"
ride_scheduled = a

ride_starts_on :: Proxy "ride_starts_on"
ride_starts_on = a

rental_package :: Proxy "rental_package"
rental_package = a

go_home :: Proxy "go_home"
go_home = a

cancel_rental_booking :: Proxy "cancel_rental_booking"
cancel_rental_booking = a

add_first_stop :: Proxy "add_first_stop"
add_first_stop = a

driver_will_be_assigned_minutes_before_starting_the_ride :: Proxy "driver_will_be_assigned_minutes_before_starting_the_ride"
driver_will_be_assigned_minutes_before_starting_the_ride = a

years_ago :: Proxy "years_ago"
years_ago = a

reported_issues :: Proxy "reported_issues"
reported_issues = a

resolved_issues :: Proxy "resolved_issues"
resolved_issues = a

issue_no :: Proxy "issue_no"
issue_no = a

reported :: Proxy "reported"
reported = a

resolved :: Proxy "resolved"
resolved = a

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

ride_related_issue_page_name :: Proxy "ride_related_issue_page_name"
ride_related_issue_page_name = a

app_related_issue_page_name :: Proxy "app_related_issue_page_name"
app_related_issue_page_name = a

driver_related_issue_page_name :: Proxy "driver_related_issue_page_name"
driver_related_issue_page_name = a

lost_and_found_issue_page_name :: Proxy "lost_and_found_issue_page_name"
lost_and_found_issue_page_name = a

select_a_ride_to_report :: Proxy "select_a_ride_to_report"
select_a_ride_to_report = a

i_dont_know_which_ride :: Proxy "i_dont_know_which_ride"
i_dont_know_which_ride = a

your_reports :: Proxy "your_reports"
your_reports = a

view :: Proxy "view"
view = a

add_voice_note :: Proxy "add_voice_note"
add_voice_note = a

voice_note :: Proxy "voice_note"
voice_note = a

added_images :: Proxy "added_images"
added_images = a

no_images_added :: Proxy "no_images_added"
no_images_added = a

submit_issue_details :: Proxy "submit_issue_details"
submit_issue_details = a

image_preview :: Proxy "image_preview"
image_preview = a

report_issue_chat_placeholder :: Proxy "report_issue_chat_placeholder"
report_issue_chat_placeholder = a

added_voice_note :: Proxy "added_voice_note"
added_voice_note = a

no_voice_note_added :: Proxy "no_voice_note_added"
no_voice_note_added = a

call_driver_title :: Proxy "call_driver_title"
call_driver_title = a

call_driver_description :: Proxy "call_driver_description"
call_driver_description = a

call_support_title :: Proxy "call_support_title"
call_support_title = a

call_support_description :: Proxy "call_support_description"
call_support_description = a

add_image :: Proxy "add_image"
add_image = a

add_another :: Proxy "add_another"
add_another = a

images :: Proxy "images"
images = a

issue_submitted_text :: Proxy "issue_submitted_text"
issue_submitted_text = a

issue_resolved_text :: Proxy "issue_resolved_text"
issue_resolved_text = a

choose_an_option :: Proxy "choose_an_option"
choose_an_option = a

image :: Proxy "image"
image = a

issue_marked_as_resolved :: Proxy "issue_marked_as_resolved"
issue_marked_as_resolved = a

still_having_issue :: Proxy "still_having_issue"
still_having_issue = a

record_voice_note :: Proxy "record_voice_note"
record_voice_note = a

cancel_button :: Proxy "cancel_button"
cancel_button = a

max_images :: Proxy "max_images"
max_images = a

sos_issue_page_name :: Proxy "sos_issue_page_name"
sos_issue_page_name = a

fare_discrepancies_issue_page_name :: Proxy "fare_discrepancies_issue_page_name"
fare_discrepancies_issue_page_name = a

payment_related_issue_page_name :: Proxy "payment_related_issue_page_name"
payment_related_issue_page_name = a

account_related_issue_page_name :: Proxy "account_related_issue_page_name"
account_related_issue_page_name = a

payment_and_fare_related_issue_page_name :: Proxy "payment_and_fare_related_issue_page_name"
payment_and_fare_related_issue_page_name = a

vehicle_related_issue_page_name :: Proxy "vehicle_related_issue_page_name"
vehicle_related_issue_page_name = a

issue :: Proxy "issue"
issue = a

other_issues :: Proxy "other_issues"
other_issues = a

cant_find_option :: Proxy "cant_find_option"
cant_find_option = a

need_help :: Proxy "need_help"
need_help = a

safety_issue_page_name :: Proxy "safety_issue_page_name"
safety_issue_page_name = a

we_hope_the_issue_is_resolved :: Proxy "we_hope_the_issue_is_resolved"
we_hope_the_issue_is_resolved = a

please_select_the_ride_to_call_driver :: Proxy "please_select_the_ride_to_call_driver"
please_select_the_ride_to_call_driver = a

add_image_s :: Proxy "add_image_s"
add_image_s = a

already_have_an_active_ride :: Proxy "already_have_an_active_ride"
already_have_an_active_ride = a

confirm_stop_location :: Proxy "confirm_stop_location"
confirm_stop_location = a

confirm_drop :: Proxy "confirm_drop"
confirm_drop = a

book_metro_with_ny_now :: Proxy "book_metro_with_ny_now"
book_metro_with_ny_now = a

learn_about_namma_safety :: Proxy "learn_about_namma_safety"
learn_about_namma_safety = a

namma_safety_will_enable_access :: Proxy "namma_safety_will_enable_access"
namma_safety_will_enable_access = a

edit_actions :: Proxy "edit_actions"
edit_actions = a

emergency_actions :: Proxy "emergency_actions"
emergency_actions = a

when_you_start_emergency_sos :: Proxy "when_you_start_emergency_sos"
when_you_start_emergency_sos = a

ride_share_after_six_pm :: Proxy "ride_share_after_six_pm"
ride_share_after_six_pm = a

who_can_track_your_ride :: Proxy "who_can_track_your_ride"
who_can_track_your_ride = a

emergency_sharing_with_contacts :: Proxy "emergency_sharing_with_contacts"
emergency_sharing_with_contacts = a

sharing_with :: Proxy "sharing_with"
sharing_with = a

add_a_contact :: Proxy "add_a_contact"
add_a_contact = a

to_ensure_safety_users_should :: Proxy "to_ensure_safety_users_should"
to_ensure_safety_users_should = a

about_sos_desc :: Proxy "about_sos_desc"
about_sos_desc = a

few_examples_of_sos_situations :: Proxy "few_examples_of_sos_situations"
few_examples_of_sos_situations = a

things_to_do_during_sos_situation :: Proxy "things_to_do_during_sos_situation"
things_to_do_during_sos_situation = a

emergency_request_sent :: Proxy "emergency_request_sent"
emergency_request_sent = a

sos_triggered_desc :: Proxy "sos_triggered_desc"
sos_triggered_desc = a

sos_actions :: Proxy "sos_actions"
sos_actions = a

call_police :: Proxy "call_police"
call_police = a

call_support :: Proxy "call_support"
call_support = a

record_video :: Proxy "record_video"
record_video = a

stop_and_share_recording :: Proxy "stop_and_share_recording"
stop_and_share_recording = a

cancel_sharing :: Proxy "cancel_sharing"
cancel_sharing = a

start_recording :: Proxy "start_recording"
start_recording = a

sharing_the_video_in :: Proxy "sharing_the_video_in"
sharing_the_video_in = a

emergency_info_shared :: Proxy "emergency_info_shared"
emergency_info_shared = a

emergency_info_shared_action :: Proxy "emergency_info_shared_action"
emergency_info_shared_action = a

set_up_your_personal_safety_settings :: Proxy "set_up_your_personal_safety_settings"
set_up_your_personal_safety_settings = a

activate_live_video_recording_features :: Proxy "activate_live_video_recording_features"
activate_live_video_recording_features = a

choose_responsive_contacts :: Proxy "choose_responsive_contacts"
choose_responsive_contacts = a

share_location_and_ride_details_emergency_contact :: Proxy "share_location_and_ride_details_emergency_contact"
share_location_and_ride_details_emergency_contact = a

namma_safety_measures :: Proxy "namma_safety_measures"
namma_safety_measures = a

safety_guidelines_for_you :: Proxy "safety_guidelines_for_you"
safety_guidelines_for_you = a

about_sos :: Proxy "about_sos"
about_sos = a

night_time_safety_checks :: Proxy "night_time_safety_checks"
night_time_safety_checks = a

share_info_with_emergency_contacts_title :: Proxy "share_info_with_emergency_contacts_title"
share_info_with_emergency_contacts_title = a

share_info_with_emergency_contacts_desc :: Proxy "share_info_with_emergency_contacts_desc"
share_info_with_emergency_contacts_desc = a

trigger_alert_to_nammayatri_support_title :: Proxy "trigger_alert_to_nammayatri_support_title"
trigger_alert_to_nammayatri_support_title = a

trigger_alert_to_nammayatri_support_desc :: Proxy "trigger_alert_to_nammayatri_support_desc"
trigger_alert_to_nammayatri_support_desc = a

enable_night_time_safety_alerts_title :: Proxy "enable_night_time_safety_alerts_title"
enable_night_time_safety_alerts_title = a

enable_night_time_safety_alerts_desc :: Proxy "enable_night_time_safety_alerts_desc"
enable_night_time_safety_alerts_desc = a

almost_done_title :: Proxy "almost_done_title"
almost_done_title = a

almost_done_desc :: Proxy "almost_done_desc"
almost_done_desc = a

safety_measure_1 :: Proxy "safety_measure_1"
safety_measure_1 = a

safety_measure_2 :: Proxy "safety_measure_2"
safety_measure_2 = a

safety_measure_3 :: Proxy "safety_measure_3"
safety_measure_3 = a

safety_measure_4 :: Proxy "safety_measure_4"
safety_measure_4 = a

safety_measure_5 :: Proxy "safety_measure_5"
safety_measure_5 = a

safety_measure_6 :: Proxy "safety_measure_6"
safety_measure_6 = a

safety_guidelines_1 :: Proxy "safety_guidelines_1"
safety_guidelines_1 = a

safety_guidelines_2 :: Proxy "safety_guidelines_2"
safety_guidelines_2 = a

safety_guidelines_3 :: Proxy "safety_guidelines_3"
safety_guidelines_3 = a

safety_guidelines_4 :: Proxy "safety_guidelines_4"
safety_guidelines_4 = a

safety_guidelines_5 :: Proxy "safety_guidelines_5"
safety_guidelines_5 = a

safety_guidelines_6 :: Proxy "safety_guidelines_6"
safety_guidelines_6 = a

safety_guidelines_7 :: Proxy "safety_guidelines_7"
safety_guidelines_7 = a

about_sos_1 :: Proxy "about_sos_1"
about_sos_1 = a

about_sos_2 :: Proxy "about_sos_2"
about_sos_2 = a

about_sos_3 :: Proxy "about_sos_3"
about_sos_3 = a

about_sos_4 :: Proxy "about_sos_4"
about_sos_4 = a

about_sos_5 :: Proxy "about_sos_5"
about_sos_5 = a

about_sos_6 :: Proxy "about_sos_6"
about_sos_6 = a

about_sos_7 :: Proxy "about_sos_7"
about_sos_7 = a

about_sos_8 :: Proxy "about_sos_8"
about_sos_8 = a

about_sos_9 :: Proxy "about_sos_9"
about_sos_9 = a

about_sos_10 :: Proxy "about_sos_10"
about_sos_10 = a

about_sos_11 :: Proxy "about_sos_11"
about_sos_11 = a

about_sos_12 :: Proxy "about_sos_12"
about_sos_12 = a

about_sos_13 :: Proxy "about_sos_13"
about_sos_13 = a

the_video_will_be_recorded :: Proxy "the_video_will_be_recorded"
the_video_will_be_recorded = a

emergency_video :: Proxy "emergency_video"
emergency_video = a

namma_safety_is_set_up :: Proxy "namma_safety_is_set_up"
namma_safety_is_set_up = a

personal_safety_settings_permission_request :: Proxy "personal_safety_settings_permission_request"
personal_safety_settings_permission_request = a

activate_namma_safety_popup_title :: Proxy "activate_namma_safety_popup_title"
activate_namma_safety_popup_title = a

activate_namma_safety_popup_desc :: Proxy "activate_namma_safety_popup_desc"
activate_namma_safety_popup_desc = a

activate_namma_safety_popup_action :: Proxy "activate_namma_safety_popup_action"
activate_namma_safety_popup_action = a

dismiss :: Proxy "dismiss"
dismiss = a

send_silent_sos_to_police :: Proxy "send_silent_sos_to_police"
send_silent_sos_to_police = a

our_safety_partner :: Proxy "our_safety_partner"
our_safety_partner = a

bangaluru_city_police :: Proxy "bangaluru_city_police"
bangaluru_city_police = a

get_options_to_directly_call_police :: Proxy "get_options_to_directly_call_police"
get_options_to_directly_call_police = a

share_sos_silently_with_police :: Proxy "share_sos_silently_with_police"
share_sos_silently_with_police = a

call_and_alert_the_nearest_police_centre :: Proxy "call_and_alert_the_nearest_police_centre"
call_and_alert_the_nearest_police_centre = a

send_a_silent_sos_to_the_police :: Proxy "send_a_silent_sos_to_the_police"
send_a_silent_sos_to_the_police = a

send_a_video_recording_to_police :: Proxy "send_a_video_recording_to_police"
send_a_video_recording_to_police = a

personal_safety_action_1 :: Proxy "personal_safety_action_1"
personal_safety_action_1 = a

personal_safety_action_2 :: Proxy "personal_safety_action_2"
personal_safety_action_2 = a

personal_safety_action_2_police :: Proxy "personal_safety_action_2_police"
personal_safety_action_2_police = a

personal_safety_action_3 :: Proxy "personal_safety_action_3"
personal_safety_action_3 = a

send_video_to_police :: Proxy "send_video_to_police"
send_video_to_police = a

finish_setup :: Proxy "finish_setup"
finish_setup = a

mark_ride_as_safe :: Proxy "mark_ride_as_safe"
mark_ride_as_safe = a

activate_sos :: Proxy "activate_sos"
activate_sos = a

emergency_info_shared_action_police :: Proxy "emergency_info_shared_action_police"
emergency_info_shared_action_police = a

start_setup :: Proxy "start_setup"
start_setup = a

call_support_for_safety :: Proxy "call_support_for_safety"
call_support_for_safety = a

we_noticed_your_ride_hasnt_moved :: Proxy "we_noticed_your_ride_hasnt_moved"
we_noticed_your_ride_hasnt_moved = a

we_noticed_your_ride_is_on_different_route :: Proxy "we_noticed_your_ride_is_on_different_route"
we_noticed_your_ride_is_on_different_route = a

we_are_here_for_you :: Proxy "we_are_here_for_you"
we_are_here_for_you = a

i_need_help :: Proxy "i_need_help"
i_need_help = a

i_feel_safe :: Proxy "i_feel_safe"
i_feel_safe = a

everything_okay_q :: Proxy "everything_okay_q"
everything_okay_q = a

please_remain_calm_you_can_request_an_immediate_call :: Proxy "please_remain_calm_you_can_request_an_immediate_call"
please_remain_calm_you_can_request_an_immediate_call = a

receive_call_from_support :: Proxy "receive_call_from_support"
receive_call_from_support = a

add_contacts :: Proxy "add_contacts"
add_contacts = a

add_contacts_manually :: Proxy "add_contacts_manually"
add_contacts_manually = a

video_share_info_to_police :: Proxy "video_share_info_to_police"
video_share_info_to_police = a

call_police_helpline :: Proxy "call_police_helpline"
call_police_helpline = a

please_remain_calm_call_police :: Proxy "please_remain_calm_call_police"
please_remain_calm_call_police = a

please_allow_camera_and_microphone_permissions :: Proxy "please_allow_camera_and_microphone_permissions"
please_allow_camera_and_microphone_permissions = a

activate_namma_safety_will_enable_access :: Proxy "activate_namma_safety_will_enable_access"
activate_namma_safety_will_enable_access = a

select_preferred_contacts :: Proxy "select_preferred_contacts"
select_preferred_contacts = a

new :: Proxy "new"
new = a

safety_center :: Proxy "safety_center"
safety_center = a

emergency_sos :: Proxy "emergency_sos"
emergency_sos = a

automatic_call_placed_to_emergency_contacts :: Proxy "automatic_call_placed_to_emergency_contacts"
automatic_call_placed_to_emergency_contacts = a

emergency_contacts_can_follow :: Proxy "emergency_contacts_can_follow"
emergency_contacts_can_follow = a

alert_safety_team :: Proxy "alert_safety_team"
alert_safety_team = a

option_to_report_a_safety_issue :: Proxy "option_to_report_a_safety_issue"
option_to_report_a_safety_issue = a

recommend_emergency_contacts_to_install :: Proxy "recommend_emergency_contacts_to_install"
recommend_emergency_contacts_to_install = a

test_safety_drill :: Proxy "test_safety_drill"
test_safety_drill = a

start_test_drill :: Proxy "start_test_drill"
start_test_drill = a

report_safety_issue :: Proxy "report_safety_issue"
report_safety_issue = a

safety_team_will_be_alerted :: Proxy "safety_team_will_be_alerted"
safety_team_will_be_alerted = a

emergency_contacts_can_take_action :: Proxy "emergency_contacts_can_take_action"
emergency_contacts_can_take_action = a

share_ride :: Proxy "share_ride"
share_ride = a

share_ride_description :: Proxy "share_ride_description"
share_ride_description = a

share_ride_with_contact :: Proxy "share_ride_with_contact"
share_ride_with_contact = a

share_link :: Proxy "share_link"
share_link = a

glad_to_know_you_are_safe :: Proxy "glad_to_know_you_are_safe"
glad_to_know_you_are_safe = a

please_stay_calm_team_alerted :: Proxy "please_stay_calm_team_alerted"
please_stay_calm_team_alerted = a

try_another_contact :: Proxy "try_another_contact"
try_another_contact = a

your_current_location :: Proxy "your_current_location"
your_current_location = a

this_is_not_a_real_sos_situation :: Proxy "this_is_not_a_real_sos_situation"
this_is_not_a_real_sos_situation = a

your_vehicle_info :: Proxy "your_vehicle_info"
your_vehicle_info = a

police_view_instruction :: Proxy "police_view_instruction"
police_view_instruction = a

test_sos_activating_in :: Proxy "test_sos_activating_in"
test_sos_activating_in = a

sos :: Proxy "sos"
sos = a

test_sos :: Proxy "test_sos"
test_sos = a

select_contact_to_call :: Proxy "select_contact_to_call"
select_contact_to_call = a

emergency_sos_activating :: Proxy "emergency_sos_activating"
emergency_sos_activating = a

press_to_start_test_drill :: Proxy "press_to_start_test_drill"
press_to_start_test_drill = a

press_in_case_of_emergency :: Proxy "press_in_case_of_emergency"
press_in_case_of_emergency = a

inform_emergency_contacts :: Proxy "inform_emergency_contacts"
inform_emergency_contacts = a

available_in_real_emergency :: Proxy "available_in_real_emergency"
available_in_real_emergency = a

other_safety_actions :: Proxy "other_safety_actions"
other_safety_actions = a

disclaimer :: Proxy "disclaimer"
disclaimer = a

use_only_in_emergency :: Proxy "use_only_in_emergency"
use_only_in_emergency = a

misuse_may_lead_to_legal_action :: Proxy "misuse_may_lead_to_legal_action"
misuse_may_lead_to_legal_action = a

use_test_drill :: Proxy "use_test_drill"
use_test_drill = a

indication_to_emergency_contacts :: Proxy "indication_to_emergency_contacts"
indication_to_emergency_contacts = a

are_you_ready_to_start_drill :: Proxy "are_you_ready_to_start_drill"
are_you_ready_to_start_drill = a

test_drill_desc :: Proxy "test_drill_desc"
test_drill_desc = a

learn_about_safety_mode :: Proxy "learn_about_safety_mode"
learn_about_safety_mode = a

test_emergency_request_sent :: Proxy "test_emergency_request_sent"
test_emergency_request_sent = a

test_sos_triggered_desc :: Proxy "test_sos_triggered_desc"
test_sos_triggered_desc = a

sos_will_be_disabled :: Proxy "sos_will_be_disabled"
sos_will_be_disabled = a

dial_now :: Proxy "dial_now"
dial_now = a

following :: Proxy "following"
following = a

turn_off_alarm :: Proxy "turn_off_alarm"
turn_off_alarm = a

choose_a_person_to_follow :: Proxy "choose_a_person_to_follow"
choose_a_person_to_follow = a

is_in_sos_situation :: Proxy "is_in_sos_situation"
is_in_sos_situation = a

marked_ride_safe :: Proxy "marked_ride_safe"
marked_ride_safe = a

stay_calm_keep_tracking :: Proxy "stay_calm_keep_tracking"
stay_calm_keep_tracking = a

you_will_be_notified :: Proxy "you_will_be_notified"
you_will_be_notified = a

tap_here_to_follow :: Proxy "tap_here_to_follow"
tap_here_to_follow = a

have_shared_ride_with_you :: Proxy "have_shared_ride_with_you"
have_shared_ride_with_you = a

sos_location :: Proxy "sos_location"
sos_location = a

this_is_a_test_mock_drill :: Proxy "this_is_a_test_mock_drill"
this_is_a_test_mock_drill = a

this_is_not_real_drill :: Proxy "this_is_not_real_drill"
this_is_not_real_drill = a

reached_destination_safely :: Proxy "reached_destination_safely"
reached_destination_safely = a

ride_ended :: Proxy "ride_ended"
ride_ended = a

complete_your_test_drill :: Proxy "complete_your_test_drill"
complete_your_test_drill = a

test_drill :: Proxy "test_drill"
test_drill = a

ride_shared_with_selected_contacts :: Proxy "ride_shared_with_selected_contacts"
ride_shared_with_selected_contacts = a

terms_and_conditions_updated :: Proxy "terms_and_conditions_updated"
terms_and_conditions_updated = a

okay :: Proxy "okay"
okay = a

try_later :: Proxy "try_later"
try_later = a

referral_code_is_applied :: Proxy "referral_code_is_applied"
referral_code_is_applied = a

you_have_already_used_different_referral_code :: Proxy "you_have_already_used_different_referral_code"
you_have_already_used_different_referral_code = a

invalid_referral_code :: Proxy "invalid_referral_code"
invalid_referral_code = a

stops :: Proxy "stops"
stops = a

green_line :: Proxy "green_line"
green_line = a

blue_line :: Proxy "blue_line"
blue_line = a

red_line :: Proxy "red_line"
red_line = a

view_route_info :: Proxy "view_route_info"
view_route_info = a

valid_until :: Proxy "valid_until"
valid_until = a

ticket_number :: Proxy "ticket_number"
ticket_number = a

ticket :: Proxy "ticket"
ticket = a

tickets :: Proxy "tickets"
tickets = a

onword_journey :: Proxy "onword_journey"
onword_journey = a

round_trip_str :: Proxy "round_trip_str"
round_trip_str = a

tickets_for_chennai_metro :: Proxy "tickets_for_chennai_metro"
tickets_for_chennai_metro = a

active_str :: Proxy "active_str"
active_str = a

expired_str :: Proxy "expired_str"
expired_str = a

used_str :: Proxy "used_str"
used_str = a

map_str :: Proxy "map_str"
map_str = a

ticket_details :: Proxy "ticket_details"
ticket_details = a

route_details :: Proxy "route_details"
route_details = a

uncertain_about_metro_routes :: Proxy "uncertain_about_metro_routes"
uncertain_about_metro_routes = a

see_map :: Proxy "see_map"
see_map = a

chennai_metro_term_1 :: Proxy "chennai_metro_term_1"
chennai_metro_term_1 = a

chennai_metro_term_2 :: Proxy "chennai_metro_term_2"
chennai_metro_term_2 = a

chennai_metro_term_event :: Proxy "chennai_metro_term_event"
chennai_metro_term_event = a

free_ticket_cashback :: Proxy "free_ticket_cashback"
free_ticket_cashback = a

no_of_passengers :: Proxy "no_of_passengers"
no_of_passengers = a

maximum :: Proxy "maximum"
maximum = a

tickets_allowed_per_user :: Proxy "tickets_allowed_per_user"
tickets_allowed_per_user = a

starting_from :: Proxy "starting_from"
starting_from = a

from :: Proxy "from"
from = a

to :: Proxy "to"
to = a

booking_id :: Proxy "booking_id"
booking_id = a

please_while_gen_ticket :: Proxy "please_while_gen_ticket"
please_while_gen_ticket = a

payment_received :: Proxy "payment_received"
payment_received = a

please_check_back_few_min :: Proxy "please_check_back_few_min"
please_check_back_few_min = a

your_booking_pending :: Proxy "your_booking_pending"
your_booking_pending = a

please_retry_booking :: Proxy "please_retry_booking"
please_retry_booking = a

booking_failed :: Proxy "booking_failed"
booking_failed = a

incase_of_fail :: Proxy "incase_of_fail"
incase_of_fail = a

refresh_status :: Proxy "refresh_status"
refresh_status = a

date :: Proxy "date"
date = a

no_of_tickets :: Proxy "no_of_tickets"
no_of_tickets = a

active_tickets :: Proxy "active_tickets"
active_tickets = a

confirming_str :: Proxy "confirming_str"
confirming_str = a

failed_str :: Proxy "failed_str"
failed_str = a

confirmed_str :: Proxy "confirmed_str"
confirmed_str = a

buy_metro_tickets :: Proxy "buy_metro_tickets"
buy_metro_tickets = a

get_fare :: Proxy "get_fare"
get_fare = a

metro_booking_timings :: Proxy "metro_booking_timings"
metro_booking_timings = a

chennai_metro_time :: Proxy "chennai_metro_time"
chennai_metro_time = a

delhi_metro_time :: Proxy "delhi_metro_time"
delhi_metro_time = a

please_come_back_later_metro :: Proxy "please_come_back_later_metro"
please_come_back_later_metro = a

no_qoutes_available :: Proxy "no_qoutes_available"
no_qoutes_available = a

i_agree_to_the :: Proxy "i_agree_to_the"
i_agree_to_the = a

here_is_metro_ticket :: Proxy "here_is_metro_ticket"
here_is_metro_ticket = a

view_ticket :: Proxy "view_ticket"
view_ticket = a

destination :: Proxy "destination"
destination = a

pay :: Proxy "pay"
pay = a

pending_str :: Proxy "pending_str"
pending_str = a

past_tickets :: Proxy "past_tickets"
past_tickets = a

one_way_str :: Proxy "one_way_str"
one_way_str = a

share_ticket :: Proxy "share_ticket"
share_ticket = a

origin :: Proxy "origin"
origin = a

history :: Proxy "history"
history = a

always :: Proxy "always"
always = a

always_share_desc :: Proxy "always_share_desc"
always_share_desc = a

night_rides_share :: Proxy "night_rides_share"
night_rides_share = a

night_rides_desc :: Proxy "night_rides_desc"
night_rides_desc = a

never :: Proxy "never"
never = a

never_share_desc :: Proxy "never_share_desc"
never_share_desc = a

share_trip_notificatons :: Proxy "share_trip_notificatons"
share_trip_notificatons = a

call_customer_support :: Proxy "call_customer_support"
call_customer_support = a

yet_to_start :: Proxy "yet_to_start"
yet_to_start = a

message_from :: Proxy "message_from"
message_from = a

ride_cancelled :: Proxy "ride_cancelled"
ride_cancelled = a

track_ride_string :: Proxy "track_ride_string"
track_ride_string = a

safety_center_is_disabled :: Proxy "safety_center_is_disabled"
safety_center_is_disabled = a

track_on_google_map :: Proxy "track_on_google_map"
track_on_google_map = a

show_walking_direction :: Proxy "show_walking_direction"
show_walking_direction = a

special_pickup_zone :: Proxy "special_pickup_zone"
special_pickup_zone = a

special_pickup_zone_ride :: Proxy "special_pickup_zone_ride"
special_pickup_zone_ride = a

we_will_try_to_connect_you_with_driver_in_closest_pickup_zone :: Proxy "we_will_try_to_connect_you_with_driver_in_closest_pickup_zone"
we_will_try_to_connect_you_with_driver_in_closest_pickup_zone = a

this_provides_you_an_instant_pickup_experience :: Proxy "this_provides_you_an_instant_pickup_experience"
this_provides_you_an_instant_pickup_experience = a

driver_at_pickup_location :: Proxy "driver_at_pickup_location"
driver_at_pickup_location = a

driver_almost_at_pickup :: Proxy "driver_almost_at_pickup"
driver_almost_at_pickup = a

maximum_edit_pickup_attempts_reached :: Proxy "maximum_edit_pickup_attempts_reached"
maximum_edit_pickup_attempts_reached = a

move_pin_to_the_desired_pickup_point :: Proxy "move_pin_to_the_desired_pickup_point"
move_pin_to_the_desired_pickup_point = a

change_pickup_location :: Proxy "change_pickup_location"
change_pickup_location = a

location_is_too_far :: Proxy "location_is_too_far"
location_is_too_far = a

a_tip_helps_find_a_ride_quicker :: Proxy "a_tip_helps_find_a_ride_quicker"
a_tip_helps_find_a_ride_quicker = a

tip_added :: Proxy "tip_added"
tip_added = a

continue_search_with_no_tip :: Proxy "continue_search_with_no_tip"
continue_search_with_no_tip = a

searching_with_no_tip :: Proxy "searching_with_no_tip"
searching_with_no_tip = a

search_again :: Proxy "search_again"
search_again = a

driver_is_on_the_way :: Proxy "driver_is_on_the_way"
driver_is_on_the_way = a

driver_is_waiting_at_pickup :: Proxy "driver_is_waiting_at_pickup"
driver_is_waiting_at_pickup = a

is_at_pickup_location :: Proxy "is_at_pickup_location"
is_at_pickup_location = a

go_to_selected_spot_for_pickup :: Proxy "go_to_selected_spot_for_pickup"
go_to_selected_spot_for_pickup = a

select_popular_spot_for_hassle_free_pickup :: Proxy "select_popular_spot_for_hassle_free_pickup"
select_popular_spot_for_hassle_free_pickup = a

ticket_is_non_cancellable :: Proxy "ticket_is_non_cancellable"
ticket_is_non_cancellable = a

cancel_booking :: Proxy "cancel_booking"
cancel_booking = a

booking_not_cancellable :: Proxy "booking_not_cancellable"
booking_not_cancellable = a

bookings_will_be_cancelled :: Proxy "bookings_will_be_cancelled"
bookings_will_be_cancelled = a

bookings_will_be_cancelled_with_refund :: Proxy "bookings_will_be_cancelled_with_refund"
bookings_will_be_cancelled_with_refund = a

refund_not_applicable :: Proxy "refund_not_applicable"
refund_not_applicable = a

yes_cancel_booking :: Proxy "yes_cancel_booking"
yes_cancel_booking = a

would_you_like_to_proceed :: Proxy "would_you_like_to_proceed"
would_you_like_to_proceed = a

booking_cancelled :: Proxy "booking_cancelled"
booking_cancelled = a

refund_is_in_process :: Proxy "refund_is_in_process"
refund_is_in_process = a

total_refund :: Proxy "total_refund"
total_refund = a

number_of_tickets :: Proxy "number_of_tickets"
number_of_tickets = a

cancellation_date :: Proxy "cancellation_date"
cancellation_date = a

tickets_for_kochi_metro :: Proxy "tickets_for_kochi_metro"
tickets_for_kochi_metro = a

your_booked_tickets :: Proxy "your_booked_tickets"
your_booked_tickets = a

plan_your_journey :: Proxy "plan_your_journey"
plan_your_journey = a

book_round_trip :: Proxy "book_round_trip"
book_round_trip = a

by_proceeding_you_agree :: Proxy "by_proceeding_you_agree"
by_proceeding_you_agree = a

terms_and_conditions_full :: Proxy "terms_and_conditions_full"
terms_and_conditions_full = a

experience_hassle_free_metro_booking :: Proxy "experience_hassle_free_metro_booking"
experience_hassle_free_metro_booking = a

kochi_metro_term_1 :: Proxy "kochi_metro_term_1"
kochi_metro_term_1 = a

kochi_metro_term_2 :: Proxy "kochi_metro_term_2"
kochi_metro_term_2 = a

kochi_metro_time :: Proxy "kochi_metro_time"
kochi_metro_time = a

book_ticket :: Proxy "book_ticket"
book_ticket = a

prepare_emergency_contacts :: Proxy "prepare_emergency_contacts"
prepare_emergency_contacts = a

emergency_contacts_will_be_notified :: Proxy "emergency_contacts_will_be_notified"
emergency_contacts_will_be_notified = a

inform_emergency_contacts_about_test :: Proxy "inform_emergency_contacts_about_test"
inform_emergency_contacts_about_test = a

recent_ride_issue_desc :: Proxy "recent_ride_issue_desc"
recent_ride_issue_desc = a

i_need_help_with_my_recent_ride :: Proxy "i_need_help_with_my_recent_ride"
i_need_help_with_my_recent_ride = a

continue_with_safety_settings :: Proxy "continue_with_safety_settings"
continue_with_safety_settings = a

tap_where_to_to_book_ride :: Proxy "tap_where_to_to_book_ride"
tap_where_to_to_book_ride = a

last_chosen_variant_not_available :: Proxy "last_chosen_variant_not_available"
last_chosen_variant_not_available = a

toll_charges :: Proxy "toll_charges"
toll_charges = a

toll_charges_desc :: Proxy "toll_charges_desc"
toll_charges_desc = a

toll_charges_including :: Proxy "toll_charges_including"
toll_charges_including = a

toll_road_changed :: Proxy "toll_road_changed"
toll_road_changed = a

parking_charge :: Proxy "parking_charge"
parking_charge = a

toll_or_parking_charges :: Proxy "toll_or_parking_charges"
toll_or_parking_charges = a

toll_charges_estimated :: Proxy "toll_charges_estimated"
toll_charges_estimated = a

add_tip :: Proxy "add_tip"
add_tip = a

change_ride_type :: Proxy "change_ride_type"
change_ride_type = a

try_adding_tip_or_change_ride_type :: Proxy "try_adding_tip_or_change_ride_type"
try_adding_tip_or_change_ride_type = a

applicable_toll_charges :: Proxy "applicable_toll_charges"
applicable_toll_charges = a

update_tip_str :: Proxy "update_tip_str"
update_tip_str = a

book :: Proxy "book"
book = a

fare_for :: Proxy "fare_for"
fare_for = a

waiting_charge_limit :: Proxy "waiting_charge_limit"
waiting_charge_limit = a

time_taken :: Proxy "time_taken"
time_taken = a

trip_distance :: Proxy "trip_distance"
trip_distance = a

unable_to_cancel_ride :: Proxy "unable_to_cancel_ride"
unable_to_cancel_ride = a

got_a_referral_from_a_driver_or_friend :: Proxy "got_a_referral_from_a_driver_or_friend"
got_a_referral_from_a_driver_or_friend = a

enter_referral_code_ :: Proxy "enter_referral_code_"
enter_referral_code_ = a

referred_users :: Proxy "referred_users"
referred_users = a

show_app_qr :: Proxy "show_app_qr"
show_app_qr = a

share_and_refer :: Proxy "share_and_refer"
share_and_refer = a

your_referral_code :: Proxy "your_referral_code"
your_referral_code = a

refer_your_friends :: Proxy "refer_your_friends"
refer_your_friends = a

referrals :: Proxy "referrals"
referrals = a

enter_now :: Proxy "enter_now"
enter_now = a

what_is_referral_program :: Proxy "what_is_referral_program"
what_is_referral_program = a

users_who_download_app_and_complete_their_first_ride_using_referral_code :: Proxy "users_who_download_app_and_complete_their_first_ride_using_referral_code"
users_who_download_app_and_complete_their_first_ride_using_referral_code = a

the_referral_program_incentivises_drivers_to_accept_more_rides :: Proxy "the_referral_program_incentivises_drivers_to_accept_more_rides"
the_referral_program_incentivises_drivers_to_accept_more_rides = a

invalid_code :: Proxy "invalid_code"
invalid_code = a

enter_6_digit_referral_code_below :: Proxy "enter_6_digit_referral_code_below"
enter_6_digit_referral_code_below = a

apply :: Proxy "apply"
apply = a

toll_charges_included :: Proxy "toll_charges_included"
toll_charges_included = a

one_tap_bookings :: Proxy "one_tap_bookings"
one_tap_bookings = a

has_your_driver_set_the_ac_as_per_your_preference :: Proxy "has_your_driver_set_the_ac_as_per_your_preference"
has_your_driver_set_the_ac_as_per_your_preference = a

no_report_an_issue :: Proxy "no_report_an_issue"
no_report_an_issue = a

great_enjoy_the_trip :: Proxy "great_enjoy_the_trip"
great_enjoy_the_trip = a

enjoy_your_budget_friendly_non_ac_ride :: Proxy "enjoy_your_budget_friendly_non_ac_ride"
enjoy_your_budget_friendly_non_ac_ride = a

ac_is_not_available_on_this_ride :: Proxy "ac_is_not_available_on_this_ride"
ac_is_not_available_on_this_ride = a

ac_not_working_desc :: Proxy "ac_not_working_desc"
ac_not_working_desc = a

showing_fare_from_multi_provider :: Proxy "showing_fare_from_multi_provider"
showing_fare_from_multi_provider = a

live_chat :: Proxy "live_chat"
live_chat = a

driver_tip_addition :: Proxy "driver_tip_addition"
driver_tip_addition = a

live_ride_sharing :: Proxy "live_ride_sharing"
live_ride_sharing = a

enhanced_safety :: Proxy "enhanced_safety"
enhanced_safety = a

confirm_provider :: Proxy "confirm_provider"
confirm_provider = a

select_a_provider :: Proxy "select_a_provider"
select_a_provider = a

confirming_selected_provider :: Proxy "confirming_selected_provider"
confirming_selected_provider = a

book_top_provider :: Proxy "book_top_provider"
book_top_provider = a

choose_from_providers :: Proxy "choose_from_providers"
choose_from_providers = a

choose_between_providers :: Proxy "choose_between_providers"
choose_between_providers = a

choose_between_providers_desc :: Proxy "choose_between_providers_desc"
choose_between_providers_desc = a

guaranteed_ride :: Proxy "guaranteed_ride"
guaranteed_ride = a

this_ride_fulfilled_by :: Proxy "this_ride_fulfilled_by"
this_ride_fulfilled_by = a

additional_features_on :: Proxy "additional_features_on"
additional_features_on = a

notify_your_ec :: Proxy "notify_your_ec"
notify_your_ec = a

ec_can_respond :: Proxy "ec_can_respond"
ec_can_respond = a

quick_support :: Proxy "quick_support"
quick_support = a

learn_about_app_safety_feat :: Proxy "learn_about_app_safety_feat"
learn_about_app_safety_feat = a

other_provider_no_receipt :: Proxy "other_provider_no_receipt"
other_provider_no_receipt = a

ride_fulfilled_by :: Proxy "ride_fulfilled_by"
ride_fulfilled_by = a

congestion_charges :: Proxy "congestion_charges"
congestion_charges = a

tip_can_be_added :: Proxy "tip_can_be_added"
tip_can_be_added = a

congestion_charges_desc :: Proxy "congestion_charges_desc"
congestion_charges_desc = a

ac_turned_off :: Proxy "ac_turned_off"
ac_turned_off = a

book_any :: Proxy "book_any"
book_any = a

estimates_expiry_error :: Proxy "estimates_expiry_error"
estimates_expiry_error = a

estimates_expiry_error_and_fetch_again :: Proxy "estimates_expiry_error_and_fetch_again"
estimates_expiry_error_and_fetch_again = a

pay_your_driver_by_cash_or_upi :: Proxy "pay_your_driver_by_cash_or_upi"
pay_your_driver_by_cash_or_upi = a

trip_delayed :: Proxy "trip_delayed"
trip_delayed = a

select_vehicle :: Proxy "select_vehicle"
select_vehicle = a

book_rental :: Proxy "book_rental"
book_rental = a

confirm_rental :: Proxy "confirm_rental"
confirm_rental = a

rental_ride :: Proxy "rental_ride"
rental_ride = a

select_duration :: Proxy "select_duration"
select_duration = a

select_distance :: Proxy "select_distance"
select_distance = a

rental_options :: Proxy "rental_options"
rental_options = a

booking_on :: Proxy "booking_on"
booking_on = a

included_kms :: Proxy "included_kms"
included_kms = a

base_fare :: Proxy "base_fare"
base_fare = a

tolls_and_parking_fees :: Proxy "tolls_and_parking_fees"
tolls_and_parking_fees = a

final_fare_description :: Proxy "final_fare_description"
final_fare_description = a

excess_distance_charge_description :: Proxy "excess_distance_charge_description"
excess_distance_charge_description = a

additional_charges_description :: Proxy "additional_charges_description"
additional_charges_description = a

parking_fees_and_tolls_not_included :: Proxy "parking_fees_and_tolls_not_included"
parking_fees_and_tolls_not_included = a

night_time_fee_description :: Proxy "night_time_fee_description"
night_time_fee_description = a

choose_your_rental_ride :: Proxy "choose_your_rental_ride"
choose_your_rental_ride = a

first_stop_optional :: Proxy "first_stop_optional"
first_stop_optional = a

january :: Proxy "january"
january = a

february :: Proxy "february"
february = a

march :: Proxy "march"
march = a

april :: Proxy "april"
april = a

may :: Proxy "may"
may = a

june :: Proxy "june"
june = a

july :: Proxy "july"
july = a

august :: Proxy "august"
august = a

september :: Proxy "september"
september = a

october :: Proxy "october"
october = a

november :: Proxy "november"
november = a

december :: Proxy "december"
december = a

hours :: Proxy "hours"
hours = a

not_added_yet :: Proxy "not_added_yet"
not_added_yet = a

next_stop :: Proxy "next_stop"
next_stop = a

time :: Proxy "time"
time = a

distance :: Proxy "distance"
distance = a

starting_odo :: Proxy "starting_odo"
starting_odo = a

end_otp :: Proxy "end_otp"
end_otp = a

only_location_within_city_limits :: Proxy "only_location_within_city_limits"
only_location_within_city_limits = a

ride_time :: Proxy "ride_time"
ride_time = a

ride_distance :: Proxy "ride_distance"
ride_distance = a

ride_started_at :: Proxy "ride_started_at"
ride_started_at = a

ride_ended_at :: Proxy "ride_ended_at"
ride_ended_at = a

estimated_fare :: Proxy "estimated_fare"
estimated_fare = a

extra_time_fare :: Proxy "extra_time_fare"
extra_time_fare = a

total_fare :: Proxy "total_fare"
total_fare = a

fare_update :: Proxy "fare_update"
fare_update = a

now :: Proxy "now"
now = a

date_invalid_message :: Proxy "date_invalid_message"
date_invalid_message = a

edit_pickup :: Proxy "edit_pickup"
edit_pickup = a

add_stop :: Proxy "add_stop"
add_stop = a

enter_pickup_loc :: Proxy "enter_pickup_loc"
enter_pickup_loc = a

intercity_options :: Proxy "intercity_options"
intercity_options = a

proceed :: Proxy "proceed"
proceed = a

schedule_ride_available :: Proxy "schedule_ride_available"
schedule_ride_available = a

rental_ride_until :: Proxy "rental_ride_until"
rental_ride_until = a

extra_time_charges :: Proxy "extra_time_charges"
extra_time_charges = a

dist_based_charges :: Proxy "dist_based_charges"
dist_based_charges = a

time_based_charges :: Proxy "time_based_charges"
time_based_charges = a

rental_policy :: Proxy "rental_policy"
rental_policy = a

select_package :: Proxy "select_package"
select_package = a

rental_policy_desc :: Proxy "rental_policy_desc"
rental_policy_desc = a

rental_policy_desc_1 :: Proxy "rental_policy_desc_1"
rental_policy_desc_1 = a

rentals_intercity_available :: Proxy "rentals_intercity_available"
rentals_intercity_available = a

check_it_out :: Proxy "check_it_out"
check_it_out = a

failed_to_cancel :: Proxy "failed_to_cancel"
failed_to_cancel = a

scheduling_allowed_in_intercity_rental :: Proxy "scheduling_allowed_in_intercity_rental"
scheduling_allowed_in_intercity_rental = a

special_zone_intercity_ineligible :: Proxy "special_zone_intercity_ineligible"
special_zone_intercity_ineligible = a

no_rides_scheduled_yet :: Proxy "no_rides_scheduled_yet"
no_rides_scheduled_yet = a

ride_booking :: Proxy "ride_booking"
ride_booking = a

special_zone_rental_ineligible :: Proxy "special_zone_rental_ineligible"
special_zone_rental_ineligible = a

services :: Proxy "services"
services = a

you_have_upcoming_rental_booking :: Proxy "you_have_upcoming_rental_booking"
you_have_upcoming_rental_booking = a

scheduled :: Proxy "scheduled"
scheduled = a

upcoming_bookings :: Proxy "upcoming_bookings"
upcoming_bookings = a

rentals_ :: Proxy "rentals_"
rentals_ = a

inter_city_ :: Proxy "inter_city_"
inter_city_ = a

you_have_upcoming_intercity_booking :: Proxy "you_have_upcoming_intercity_booking"
you_have_upcoming_intercity_booking = a

a_ride_already_exists :: Proxy "a_ride_already_exists"
a_ride_already_exists = a

you_have_an_ride_from_to_scheduled_from_till :: Proxy "you_have_an_ride_from_to_scheduled_from_till"
you_have_an_ride_from_to_scheduled_from_till = a

extra_per_km_fare :: Proxy "extra_per_km_fare"
extra_per_km_fare = a

extra_per_minute_fare :: Proxy "extra_per_minute_fare"
extra_per_minute_fare = a

pickup_charges :: Proxy "pickup_charges"
pickup_charges = a

waiting_charges_after_3_mins :: Proxy "waiting_charges_after_3_mins"
waiting_charges_after_3_mins = a

fare_determined_as_per_karnataka_guidelines :: Proxy "fare_determined_as_per_karnataka_guidelines"
fare_determined_as_per_karnataka_guidelines = a

rental_charges :: Proxy "rental_charges"
rental_charges = a

rental_info_policy_desc :: Proxy "rental_info_policy_desc"
rental_info_policy_desc = a

rental_info_policy_desc_ :: Proxy "rental_info_policy_desc_"
rental_info_policy_desc_ = a

rental_screen_explainer :: Proxy "rental_screen_explainer"
rental_screen_explainer = a

instant :: Proxy "instant"
instant = a

coming_soon :: Proxy "coming_soon"
coming_soon = a

cancel_scheduled_ride :: Proxy "cancel_scheduled_ride"
cancel_scheduled_ride = a

cancel_scheduled_ride_desc :: Proxy "cancel_scheduled_ride_desc"
cancel_scheduled_ride_desc = a

confirm_cancellation :: Proxy "confirm_cancellation"
confirm_cancellation = a

intercity_rides_coming_soon :: Proxy "intercity_rides_coming_soon"
intercity_rides_coming_soon = a

view_fares :: Proxy "view_fares"
view_fares = a

excess_time_description :: Proxy "excess_time_description"
excess_time_description = a

estimated_charges :: Proxy "estimated_charges"
estimated_charges = a

your_cancellation_rate_is_high :: Proxy "your_cancellation_rate_is_high"
your_cancellation_rate_is_high = a

avoid_further_cancellations_to_keep_using_app :: Proxy "avoid_further_cancellations_to_keep_using_app"
avoid_further_cancellations_to_keep_using_app = a

night_time_fees :: Proxy "night_time_fees"
night_time_fees = a

parking_and_other_charges :: Proxy "parking_and_other_charges"
parking_and_other_charges = a

additional_charges :: Proxy "additional_charges"
additional_charges = a

estimated_base_fare :: Proxy "estimated_base_fare"
estimated_base_fare = a

included_distance :: Proxy "included_distance"
included_distance = a

included_time :: Proxy "included_time"
included_time = a

toll_charges_description :: Proxy "toll_charges_description"
toll_charges_description = a

will_be_added_to_final_fare :: Proxy "will_be_added_to_final_fare"
will_be_added_to_final_fare = a

extra_distance_fare :: Proxy "extra_distance_fare"
extra_distance_fare = a

network_error :: Proxy "network_error"
network_error = a

server_error :: Proxy "server_error"
server_error = a

unknown_error :: Proxy "unknown_error"
unknown_error = a

connection_refused :: Proxy "connection_refused"
connection_refused = a

timeout :: Proxy "timeout"
timeout = a

was_toll_exp_smooth :: Proxy "was_toll_exp_smooth"
was_toll_exp_smooth = a

was_toll_exp_smooth_desc :: Proxy "was_toll_exp_smooth_desc"
was_toll_exp_smooth_desc = a

was_driver_helpful :: Proxy "was_driver_helpful"
was_driver_helpful = a

was_ride_safe_desc :: Proxy "was_ride_safe_desc"
was_ride_safe_desc = a

was_ride_safe :: Proxy "was_ride_safe"
was_ride_safe = a

was_driver_helpful_desc :: Proxy "was_driver_helpful_desc"
was_driver_helpful_desc = a

collect_toll_sep :: Proxy "collect_toll_sep"
collect_toll_sep = a

final_fare_excludes_toll :: Proxy "final_fare_excludes_toll"
final_fare_excludes_toll = a

toll_charges_maybe_applicable :: Proxy "toll_charges_maybe_applicable"
toll_charges_maybe_applicable = a

metro_banner_title :: Proxy "metro_banner_title"
metro_banner_title = a

view_on_google_maps :: Proxy "view_on_google_maps"
view_on_google_maps = a

walking_directions_to_pickup :: Proxy "walking_directions_to_pickup"
walking_directions_to_pickup = a

explore_city_with_us :: Proxy "explore_city_with_us"
explore_city_with_us = a

go_to_destination :: Proxy "go_to_destination"
go_to_destination = a

walk_to :: Proxy "walk_to"
walk_to = a

bangalore :: Proxy "bangalore"
bangalore = a

kolkata :: Proxy "kolkata"
kolkata = a

paris :: Proxy "paris"
paris = a

kochi :: Proxy "kochi"
kochi = a

delhi :: Proxy "delhi"
delhi = a

hyderabad :: Proxy "hyderabad"
hyderabad = a

mumbai :: Proxy "mumbai"
mumbai = a

chennai :: Proxy "chennai"
chennai = a

coimbatore :: Proxy "coimbatore"
coimbatore = a

pondicherry :: Proxy "pondicherry"
pondicherry = a

goa :: Proxy "goa"
goa = a

pune :: Proxy "pune"
pune = a

mysore :: Proxy "mysore"
mysore = a

tumakuru :: Proxy "tumakuru"
tumakuru = a

noida :: Proxy "noida"
noida = a

gurugram :: Proxy "gurugram"
gurugram = a

waiting_charges :: Proxy "waiting_charges"
waiting_charges = a

quotes_expiry_error_and_fetch_again :: Proxy "quotes_expiry_error_and_fetch_again"
quotes_expiry_error_and_fetch_again = a

place_a_call :: Proxy "place_a_call"
place_a_call = a

you_can_write_to_us_at :: Proxy "you_can_write_to_us_at"
you_can_write_to_us_at = a

chargeable :: Proxy "chargeable"
chargeable = a

booked :: Proxy "booked"
booked = a

surcharges :: Proxy "surcharges"
surcharges = a

siliguri :: Proxy "siliguri"
siliguri = a

kozhikode :: Proxy "kozhikode"
kozhikode = a

thrissur :: Proxy "thrissur"
thrissur = a

trivandrum :: Proxy "trivandrum"
trivandrum = a

metro_free_ticket_event :: Proxy "metro_free_ticket_event"
metro_free_ticket_event = a

metro_free_ticket_event_desc :: Proxy "metro_free_ticket_event_desc"
metro_free_ticket_event_desc = a

next_free_ticket :: Proxy "next_free_ticket"
next_free_ticket = a

free_ticket_available :: Proxy "free_ticket_available"
free_ticket_available = a

additional_charges_will_be_applicable :: Proxy "additional_charges_will_be_applicable"
additional_charges_will_be_applicable = a

parking_charges_included :: Proxy "parking_charges_included"
parking_charges_included = a

app_toll_charges :: Proxy "app_toll_charges"
app_toll_charges = a

app_parking_charges :: Proxy "app_parking_charges"
app_parking_charges = a

app_toll_parking_charges :: Proxy "app_toll_parking_charges"
app_toll_parking_charges = a

parking_charges_desc :: Proxy "parking_charges_desc"
parking_charges_desc = a

toll_charges_included_in_fair :: Proxy "toll_charges_included_in_fair"
toll_charges_included_in_fair = a

please_do_not_pay_extra_to_driver :: Proxy "please_do_not_pay_extra_to_driver"
please_do_not_pay_extra_to_driver = a

vellore :: Proxy "vellore"
vellore = a

hosur :: Proxy "hosur"
hosur = a

madurai :: Proxy "madurai"
madurai = a

thanjavur :: Proxy "thanjavur"
thanjavur = a

tirunelveli :: Proxy "tirunelveli"
tirunelveli = a

salem :: Proxy "salem"
salem = a

trichy :: Proxy "trichy"
trichy = a

davanagere :: Proxy "davanagere"
davanagere = a

shivamogga :: Proxy "shivamogga"
shivamogga = a

hubli :: Proxy "hubli"
hubli = a

mangalore :: Proxy "mangalore"
mangalore = a

gulbarga :: Proxy "gulbarga"
gulbarga = a

udupi :: Proxy "udupi"
udupi = a

cancel_booking_ :: Proxy "cancel_booking_"
cancel_booking_ = a

cancel_intercity_booking :: Proxy "cancel_intercity_booking"
cancel_intercity_booking = a

rental_booking :: Proxy "rental_booking"
rental_booking = a

intercity_booking :: Proxy "intercity_booking"
intercity_booking = a

booking :: Proxy "booking"
booking = a

by :: Proxy "by"
by = a

customers :: Proxy "customers"
customers = a

rating :: Proxy "rating"
rating = a

cancellation :: Proxy "cancellation"
cancellation = a

trips :: Proxy "trips"
trips = a

i_speak :: Proxy "i_speak"
i_speak = a

and :: Proxy "and"
and = a

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

clean_bike :: Proxy "clean_bike"
clean_bike = a

uncomfortable_bike :: Proxy "uncomfortable_bike"
uncomfortable_bike = a

driver_available :: Proxy "driver_available"
driver_available = a

drivers_available :: Proxy "drivers_available"
drivers_available = a

more_safety_measures :: Proxy "more_safety_measures"
more_safety_measures = a

safety_setup :: Proxy "safety_setup"
safety_setup = a

complete :: Proxy "complete"
complete = a

trusted_contact_help :: Proxy "trusted_contact_help"
trusted_contact_help = a

driver_safety_standards :: Proxy "driver_safety_standards"
driver_safety_standards = a

trusted_contact :: Proxy "trusted_contact"
trusted_contact = a

trusted_contact_highlight :: Proxy "trusted_contact_highlight"
trusted_contact_highlight = a

safety_drill :: Proxy "safety_drill"
safety_drill = a

default_contact :: Proxy "default_contact"
default_contact = a

app_call_chat :: Proxy "app_call_chat"
app_call_chat = a

trusted_contact_desc :: Proxy "trusted_contact_desc"
trusted_contact_desc = a

enable_live_tracking :: Proxy "enable_live_tracking"
enable_live_tracking = a

unexpected_event_check :: Proxy "unexpected_event_check"
unexpected_event_check = a

unexpected_event_check_desc :: Proxy "unexpected_event_check_desc"
unexpected_event_check_desc = a

unexpected_event_check_timings :: Proxy "unexpected_event_check_timings"
unexpected_event_check_timings = a

next :: Proxy "next"
next = a

post_ride_check :: Proxy "post_ride_check"
post_ride_check = a

post_ride_check_desc :: Proxy "post_ride_check_desc"
post_ride_check_desc = a

post_ride_check_timings :: Proxy "post_ride_check_timings"
post_ride_check_timings = a

safety_team_notification :: Proxy "safety_team_notification"
safety_team_notification = a

notify_safety_team :: Proxy "notify_safety_team"
notify_safety_team = a

notify_safety_team_sub :: Proxy "notify_safety_team_sub"
notify_safety_team_sub = a

notify_safety_team_note :: Proxy "notify_safety_team_note"
notify_safety_team_note = a

emergency_sos_new :: Proxy "emergency_sos_new"
emergency_sos_new = a

emergency_sos_sub :: Proxy "emergency_sos_sub"
emergency_sos_sub = a

shake_to_activate :: Proxy "shake_to_activate"
shake_to_activate = a

shake_to_activate_sub :: Proxy "shake_to_activate_sub"
shake_to_activate_sub = a

automatic_call_sos :: Proxy "automatic_call_sos"
automatic_call_sos = a

automatic_call_sos_sub :: Proxy "automatic_call_sos_sub"
automatic_call_sos_sub = a

place_default_call :: Proxy "place_default_call"
place_default_call = a

default_call_contact :: Proxy "default_call_contact"
default_call_contact = a

default_contact_desc :: Proxy "default_contact_desc"
default_contact_desc = a

more_emergency_actions :: Proxy "more_emergency_actions"
more_emergency_actions = a

siren :: Proxy "siren"
siren = a

call_police_desc :: Proxy "call_police_desc"
call_police_desc = a

record_audio_desc :: Proxy "record_audio_desc"
record_audio_desc = a

siren_desc :: Proxy "siren_desc"
siren_desc = a

call_safety_team_desc :: Proxy "call_safety_team_desc"
call_safety_team_desc = a

safety_drill_desc :: Proxy "safety_drill_desc"
safety_drill_desc = a

safety_drill_sub :: Proxy "safety_drill_sub"
safety_drill_sub = a

safety_drill_note :: Proxy "safety_drill_note"
safety_drill_note = a

ride_actions :: Proxy "ride_actions"
ride_actions = a

ride_actions_sub :: Proxy "ride_actions_sub"
ride_actions_sub = a

live_tracking :: Proxy "live_tracking"
live_tracking = a

live_tracking_sub :: Proxy "live_tracking_sub"
live_tracking_sub = a

chat_with_rider :: Proxy "chat_with_rider"
chat_with_rider = a

chat_with_rider_sub :: Proxy "chat_with_rider_sub"
chat_with_rider_sub = a

emergency_actions_sub :: Proxy "emergency_actions_sub"
emergency_actions_sub = a

current_initiatives :: Proxy "current_initiatives"
current_initiatives = a

current_initiatives_sub :: Proxy "current_initiatives_sub"
current_initiatives_sub = a

driver_verification :: Proxy "driver_verification"
driver_verification = a

driver_verification_sub :: Proxy "driver_verification_sub"
driver_verification_sub = a

safety_feedback :: Proxy "safety_feedback"
safety_feedback = a

safety_feedback_sub :: Proxy "safety_feedback_sub"
safety_feedback_sub = a

safety_training :: Proxy "safety_training"
safety_training = a

safety_training_sub :: Proxy "safety_training_sub"
safety_training_sub = a

driver_id_check :: Proxy "driver_id_check"
driver_id_check = a

driver_id_check_sub :: Proxy "driver_id_check_sub"
driver_id_check_sub = a

data_privacy :: Proxy "data_privacy"
data_privacy = a

data_privacy_sub :: Proxy "data_privacy_sub"
data_privacy_sub = a

favourite_driver :: Proxy "favourite_driver"
favourite_driver = a

favourite_driver_sub :: Proxy "favourite_driver_sub"
favourite_driver_sub = a

women_drivers :: Proxy "women_drivers"
women_drivers = a

women_drivers_sub :: Proxy "women_drivers_sub"
women_drivers_sub = a

dashcam :: Proxy "dashcam"
dashcam = a

dashcam_sub :: Proxy "dashcam_sub"
dashcam_sub = a

never_share_ln :: Proxy "never_share_ln"
never_share_ln = a

always_share_ln :: Proxy "always_share_ln"
always_share_ln = a

share_with_time_constraints_ln :: Proxy "share_with_time_constraints_ln"
share_with_time_constraints_ln = a

never_share_em :: Proxy "never_share_em"
never_share_em = a

always_share_em :: Proxy "always_share_em"
always_share_em = a

share_with_time_constraints_em :: Proxy "share_with_time_constraints_em"
share_with_time_constraints_em = a

live_ride_tracking :: Proxy "live_ride_tracking"
live_ride_tracking = a

live_ride_tracking_desc :: Proxy "live_ride_tracking_desc"
live_ride_tracking_desc = a

upcoming_initiatives :: Proxy "upcoming_initiatives"
upcoming_initiatives = a

upcoming_initiatives_desc :: Proxy "upcoming_initiatives_desc"
upcoming_initiatives_desc = a

receive_call_from_safety_team :: Proxy "receive_call_from_safety_team"
receive_call_from_safety_team = a

notify_all_emergency_contacts :: Proxy "notify_all_emergency_contacts"
notify_all_emergency_contacts = a

record_audio :: Proxy "record_audio"
record_audio = a

call_safety_team :: Proxy "call_safety_team"
call_safety_team = a

safety_team_callback_requested :: Proxy "safety_team_callback_requested"
safety_team_callback_requested = a

emergency_contacts_notified :: Proxy "emergency_contacts_notified"
emergency_contacts_notified = a

call_placed :: Proxy "call_placed"
call_placed = a

emergency_sos_activated :: Proxy "emergency_sos_activated"
emergency_sos_activated = a

tap_to_call_other_emergency_contacts :: Proxy "tap_to_call_other_emergency_contacts"
tap_to_call_other_emergency_contacts = a

recording_audio :: Proxy "recording_audio"
recording_audio = a

recorded_audio :: Proxy "recorded_audio"
recorded_audio = a

share_with_safety_team :: Proxy "share_with_safety_team"
share_with_safety_team = a

emergency :: Proxy "emergency"
emergency = a

manual_live_tracking :: Proxy "manual_live_tracking"
manual_live_tracking = a

manual_live_tracking_desc :: Proxy "manual_live_tracking_desc"
manual_live_tracking_desc = a

automatic_live_tracking :: Proxy "automatic_live_tracking"
automatic_live_tracking = a

automatic_live_tracking_desc :: Proxy "automatic_live_tracking_desc"
automatic_live_tracking_desc = a

tracking_no_setup :: Proxy "tracking_no_setup"
tracking_no_setup = a

following_str :: Proxy "following_str"
following_str = a

dialing_police_in_time :: Proxy "dialing_police_in_time"
dialing_police_in_time = a

reached_destination :: Proxy "reached_destination"
reached_destination = a

mins :: Proxy "mins"
mins = a

updated_fare :: Proxy "updated_fare"
updated_fare = a

how's_trip :: Proxy "how's_trip"
how's_trip = a

provided_feedback :: Proxy "provided_feedback"
provided_feedback = a

favourite_your_driver :: Proxy "favourite_your_driver"
favourite_your_driver = a

prefer_driver :: Proxy "prefer_driver"
prefer_driver = a

write_review :: Proxy "write_review"
write_review = a

transit :: Proxy "transit"
transit = a

intercity_str :: Proxy "intercity_str"
intercity_str = a

rental_str :: Proxy "rental_str"
rental_str = a

delivery_str :: Proxy "delivery_str"
delivery_str = a

where_are_you_going :: Proxy "where_are_you_going"
where_are_you_going = a

tap_to_follow :: Proxy "tap_to_follow"
tap_to_follow = a

has_shared_a_ride_with_you :: Proxy "has_shared_a_ride_with_you"
has_shared_a_ride_with_you = a

test_sos_activated :: Proxy "test_sos_activated"
test_sos_activated = a

choose_from_contacts :: Proxy "choose_from_contacts"
choose_from_contacts = a

add_manually :: Proxy "add_manually"
add_manually = a

default_contact_not_set :: Proxy "default_contact_not_set"
default_contact_not_set = a

driver :: Proxy "driver"
driver = a

recommend_share_manually :: Proxy "recommend_share_manually"
recommend_share_manually = a

cannot_add_own_number :: Proxy "cannot_add_own_number"
cannot_add_own_number = a

confirm_pickup_and_drop_location :: Proxy "confirm_pickup_and_drop_location"
confirm_pickup_and_drop_location = a

confirm_your_delivery :: Proxy "confirm_your_delivery"
confirm_your_delivery = a

payment_at_receiving_end :: Proxy "payment_at_receiving_end"
payment_at_receiving_end = a

payment_at_receiving_end_desc :: Proxy "payment_at_receiving_end_desc"
payment_at_receiving_end_desc = a

sender :: Proxy "sender"
sender = a

receiver :: Proxy "receiver"
receiver = a

phone :: Proxy "phone"
phone = a

building_or_flat :: Proxy "building_or_flat"
building_or_flat = a

optional_instruction :: Proxy "optional_instruction"
optional_instruction = a

help_us_provide_smooth_pickup :: Proxy "help_us_provide_smooth_pickup"
help_us_provide_smooth_pickup = a

help_us_provide_smooth_drop :: Proxy "help_us_provide_smooth_drop"
help_us_provide_smooth_drop = a

i_am_the_sender :: Proxy "i_am_the_sender"
i_am_the_sender = a

i_am_the_receiver :: Proxy "i_am_the_receiver"
i_am_the_receiver = a

reckless_handling :: Proxy "reckless_handling"
reckless_handling = a

delivery_delayed :: Proxy "delivery_delayed"
delivery_delayed = a

items_missing :: Proxy "items_missing"
items_missing = a

polite_attitude :: Proxy "polite_attitude"
polite_attitude = a

smooth_experience :: Proxy "smooth_experience"
smooth_experience = a

secure_delivery :: Proxy "secure_delivery"
secure_delivery = a

minimal_calling :: Proxy "minimal_calling"
minimal_calling = a

rude_behaviour :: Proxy "rude_behaviour"
rude_behaviour = a

package_photo_and_otp :: Proxy "package_photo_and_otp"
package_photo_and_otp = a

send_now :: Proxy "send_now"
send_now = a

book_for :: Proxy "book_for"
book_for = a

delivery_details :: Proxy "delivery_details"
delivery_details = a

delivery_guidelines :: Proxy "delivery_guidelines"
delivery_guidelines = a

view_all_guidelines :: Proxy "view_all_guidelines"
view_all_guidelines = a

items_should_fit_in_backpack :: Proxy "items_should_fit_in_backpack"
items_should_fit_in_backpack = a

avoid_sending_high_value_items :: Proxy "avoid_sending_high_value_items"
avoid_sending_high_value_items = a

illegal_items_prohibited :: Proxy "illegal_items_prohibited"
illegal_items_prohibited = a

pickup_instruction :: Proxy "pickup_instruction"
pickup_instruction = a

more_about_me :: Proxy "more_about_me"
more_about_me = a

driving_since :: Proxy "driving_since"
driving_since = a

driver_section_card :: Proxy "driver_section_card"
driver_section_card = a

know_your_driver :: Proxy "know_your_driver"
know_your_driver = a

rated :: Proxy "rated"
rated = a

drivers :: Proxy "drivers"
drivers = a

locations :: Proxy "locations"
locations = a

knows_your_driver :: Proxy "knows_your_driver"
knows_your_driver = a

last_trip :: Proxy "last_trip"
last_trip = a

rupees :: Proxy "rupees"
rupees = a

paid_by_cash :: Proxy "paid_by_cash"
paid_by_cash = a

remove_from_favourite :: Proxy "remove_from_favourite"
remove_from_favourite = a

clean_vehicle :: Proxy "clean_vehicle"
clean_vehicle = a

ac_not_turned_on :: Proxy "ac_not_turned_on"
ac_not_turned_on = a

late_pick_up_arrival :: Proxy "late_pick_up_arrival"
late_pick_up_arrival = a

asked_for_more_fare :: Proxy "asked_for_more_fare"
asked_for_more_fare = a

unhygienic_vehicle :: Proxy "unhygienic_vehicle"
unhygienic_vehicle = a

training :: Proxy "training"
training = a

financial :: Proxy "financial"
financial = a

kids_education :: Proxy "kids_education"
kids_education = a

buy_new_vehicle :: Proxy "buy_new_vehicle"
buy_new_vehicle = a

failed_to_remove_driver :: Proxy "failed_to_remove_driver"
failed_to_remove_driver = a

not_available :: Proxy "not_available"
not_available = a

buy_new_home :: Proxy "buy_new_home"
buy_new_home = a

you_favourited :: Proxy "you_favourited"
you_favourited = a

favorite_your_driver :: Proxy "favorite_your_driver"
favorite_your_driver = a

favourite_driver_preference :: Proxy "favourite_driver_preference"
favourite_driver_preference = a

ride_type_with_favourite_driver :: Proxy "ride_type_with_favourite_driver"
ride_type_with_favourite_driver = a

gotit :: Proxy "gotit"
gotit = a

no_favourite_yet :: Proxy "no_favourite_yet"
no_favourite_yet = a

favourite_appear_here :: Proxy "favourite_appear_here"
favourite_appear_here = a

edit_your_pickup_location_instead :: Proxy "edit_your_pickup_location_instead"
edit_your_pickup_location_instead = a

round_trip_invalid_message :: Proxy "round_trip_invalid_message"
round_trip_invalid_message = a

pickup_time_not_selected :: Proxy "pickup_time_not_selected"
pickup_time_not_selected = a

booking_duration_invalid :: Proxy "booking_duration_invalid"
booking_duration_invalid = a

return :: Proxy "return"
return = a

pickup_input :: Proxy "pickup_input"
pickup_input = a

return_input :: Proxy "return_input"
return_input = a

book_a_round_trip :: Proxy "book_a_round_trip"
book_a_round_trip = a

total_ride_duration :: Proxy "total_ride_duration"
total_ride_duration = a

total_ride_distance :: Proxy "total_ride_distance"
total_ride_distance = a

ride :: Proxy "ride"
ride = a

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

intercity_tc_1 :: Proxy "intercity_tc_1"
intercity_tc_1 = a

intercity_tc_2 :: Proxy "intercity_tc_2"
intercity_tc_2 = a

round_trip_policy_card_1 :: Proxy "round_trip_policy_card_1"
round_trip_policy_card_1 = a

round_trip_policy_card_2 :: Proxy "round_trip_policy_card_2"
round_trip_policy_card_2 = a

round_trip_policy :: Proxy "round_trip_policy"
round_trip_policy = a

distance_fare :: Proxy "distance_fare"
distance_fare = a

driver_allowance_ :: Proxy "driver_allowance_"
driver_allowance_ = a

night_charges :: Proxy "night_charges"
night_charges = a

night_charges_description :: Proxy "night_charges_description"
night_charges_description = a

state_charges_description :: Proxy "state_charges_description"
state_charges_description = a

parking_charges_description :: Proxy "parking_charges_description"
parking_charges_description = a

toll_and_parking_charges_description :: Proxy "toll_and_parking_charges_description"
toll_and_parking_charges_description = a

state_permit_charges :: Proxy "state_permit_charges"
state_permit_charges = a

driver_allowance_description :: Proxy "driver_allowance_description"
driver_allowance_description = a

driver_allowance_required :: Proxy "driver_allowance_required"
driver_allowance_required = a

toll_and_parking_charges :: Proxy "toll_and_parking_charges"
toll_and_parking_charges = a

night_shift_charges :: Proxy "night_shift_charges"
night_shift_charges = a

round_trip_explainer :: Proxy "round_trip_explainer"
round_trip_explainer = a

reserve :: Proxy "reserve"
reserve = a

leave_now :: Proxy "leave_now"
leave_now = a

is_your_driver :: Proxy "is_your_driver"
is_your_driver = a

your_driver_will_be :: Proxy "your_driver_will_be"
your_driver_will_be = a

ride_summary :: Proxy "ride_summary"
ride_summary = a

trip_inclusions :: Proxy "trip_inclusions"
trip_inclusions = a

please_pay_parking_or_other :: Proxy "please_pay_parking_or_other"
please_pay_parking_or_other = a

toll_and_state_permit :: Proxy "toll_and_state_permit"
toll_and_state_permit = a

trip_exclusion :: Proxy "trip_exclusion"
trip_exclusion = a

confirm :: Proxy "confirm"
confirm = a

you_have_an_ride_from_without_to :: Proxy "you_have_an_ride_from_without_to"
you_have_an_ride_from_without_to = a

for_every_extra_hour_you_add :: Proxy "for_every_extra_hour_you_add"
for_every_extra_hour_you_add = a

by_default_one_hour :: Proxy "by_default_one_hour"
by_default_one_hour = a

you_have_an_upcoming_booking :: Proxy "you_have_an_upcoming_booking"
you_have_an_upcoming_booking = a

addon_km_charge :: Proxy "addon_km_charge"
addon_km_charge = a

time_fare :: Proxy "time_fare"
time_fare = a

trip_fare_includes :: Proxy "trip_fare_includes"
trip_fare_includes = a

trip_fare_excludes :: Proxy "trip_fare_excludes"
trip_fare_excludes = a

extras_will_be_charged_at :: Proxy "extras_will_be_charged_at"
extras_will_be_charged_at = a

fixed_charges :: Proxy "fixed_charges"
fixed_charges = a

pickup_date_and_time :: Proxy "pickup_date_and_time"
pickup_date_and_time = a

drop_back_in_at :: Proxy "drop_back_in_at"
drop_back_in_at = a

per_km :: Proxy "per_km"
per_km = a

per_min :: Proxy "per_min"
per_min = a

hour :: Proxy "hour"
hour = a

sec :: Proxy "sec"
sec = a

bike_taxi :: Proxy "bike_taxi"
bike_taxi = a

phone_number_permission :: Proxy "phone_number_permission"
phone_number_permission = a

phone_number_permission_desc :: Proxy "phone_number_permission_desc"
phone_number_permission_desc = a

deny :: Proxy "deny"
deny = a

allow :: Proxy "allow"
allow = a

intercity_bus :: Proxy "intercity_bus"
intercity_bus = a

driver_assigned :: Proxy "driver_assigned"
driver_assigned = a

sender_name :: Proxy "sender_name"
sender_name = a

sender_phone :: Proxy "sender_phone"
sender_phone = a

senders_building_flat :: Proxy "senders_building_flat"
senders_building_flat = a

pickup_instructions :: Proxy "pickup_instructions"
pickup_instructions = a

receiver_name :: Proxy "receiver_name"
receiver_name = a

receiver_phone :: Proxy "receiver_phone"
receiver_phone = a

receivers_building_flat :: Proxy "receivers_building_flat"
receivers_building_flat = a

drop_instructions :: Proxy "drop_instructions"
drop_instructions = a

out_for_delivery :: Proxy "out_for_delivery"
out_for_delivery = a

pickup_in_progress :: Proxy "pickup_in_progress"
pickup_in_progress = a

arrived_at_drop :: Proxy "arrived_at_drop"
arrived_at_drop = a

out_for_pickup :: Proxy "out_for_pickup"
out_for_pickup = a

confirm_pickup :: Proxy "confirm_pickup"
confirm_pickup = a

rate_your_delivery_with :: Proxy "rate_your_delivery_with"
rate_your_delivery_with = a

delivery_completed :: Proxy "delivery_completed"
delivery_completed = a

estimated_arrival_by :: Proxy "estimated_arrival_by"
estimated_arrival_by = a

back :: Proxy "back"
back = a

photo_not_taken_yet :: Proxy "photo_not_taken_yet"
photo_not_taken_yet = a

please_refresh_once_driver_takes_photo :: Proxy "please_refresh_once_driver_takes_photo"
please_refresh_once_driver_takes_photo = a

package_photo_desc :: Proxy "package_photo_desc"
package_photo_desc = a

start_otp :: Proxy "start_otp"
start_otp = a

package_photo :: Proxy "package_photo"
package_photo = a

refresh :: Proxy "refresh"
refresh = a

drop_instruction :: Proxy "drop_instruction"
drop_instruction = a

quick_delivery_with :: Proxy "quick_delivery_with"
quick_delivery_with = a

step :: Proxy "step"
step = a

booking_cannot_proceed_one_party_has_active_booking :: Proxy "booking_cannot_proceed_one_party_has_active_booking"
booking_cannot_proceed_one_party_has_active_booking = a

please_enter_a_valid_mobile_number :: Proxy "please_enter_a_valid_mobile_number"
please_enter_a_valid_mobile_number = a

please_enter_a_valid_address :: Proxy "please_enter_a_valid_address"
please_enter_a_valid_address = a

please_enter_a_valid_name :: Proxy "please_enter_a_valid_name"
please_enter_a_valid_name = a

enter_a_name :: Proxy "enter_a_name"
enter_a_name = a

enter_a_address :: Proxy "enter_a_address"
enter_a_address = a

explore :: Proxy "explore"
explore = a

delivered_in_just :: Proxy "delivered_in_just"
delivered_in_just = a

bengali :: Proxy "bengali"
bengali = a

hindi :: Proxy "hindi"
hindi = a

kannada :: Proxy "kannada"
kannada = a

malayalam :: Proxy "malayalam"
malayalam = a

tamil :: Proxy "tamil"
tamil = a

telugu :: Proxy "telugu"
telugu = a

english :: Proxy "english"
english = a

odisha :: Proxy "odisha"
odisha = a

bhubaneswar :: Proxy "bhubaneswar"
bhubaneswar = a

different_bike :: Proxy "different_bike"
different_bike = a

limit_reached :: Proxy "limit_reached"
limit_reached = a

confirm_contacts :: Proxy "confirm_contacts"
confirm_contacts = a

tickets_for_delhi_metro :: Proxy "tickets_for_delhi_metro"
tickets_for_delhi_metro = a

max_parcel_size :: Proxy "max_parcel_size"
max_parcel_size = a

bus__ :: Proxy "bus__"
bus__ = a

tickets_for_chennai_bus :: Proxy "tickets_for_chennai_bus"
tickets_for_chennai_bus = a

buy_bus_tickets :: Proxy "buy_bus_tickets"
buy_bus_tickets = a

book_and_pay :: Proxy "book_and_pay"
book_and_pay = a

bus_ticket :: Proxy "bus_ticket"
bus_ticket = a

check_spelling_and_try_again :: Proxy "check_spelling_and_try_again"
check_spelling_and_try_again = a

book_bus_ticket :: Proxy "book_bus_ticket"
book_bus_ticket = a

book_a_one_way_instant_bus_ticket :: Proxy "book_a_one_way_instant_bus_ticket"
book_a_one_way_instant_bus_ticket = a

recent_ticket :: Proxy "recent_ticket"
recent_ticket = a

experience_hassle_free_bus_bookings_with :: Proxy "experience_hassle_free_bus_bookings_with"
experience_hassle_free_bus_bookings_with = a

enter_bus_number_or_destination :: Proxy "enter_bus_number_or_destination"
enter_bus_number_or_destination = a

destination_stop :: Proxy "destination_stop"
destination_stop = a

route_bus_number :: Proxy "route_bus_number"
route_bus_number = a

pickup_stop :: Proxy "pickup_stop"
pickup_stop = a

tickets_for_kolkata_bus :: Proxy "tickets_for_kolkata_bus"
tickets_for_kolkata_bus = a

ticket_validity_30_minutes :: Proxy "ticket_validity_30_minutes"
ticket_validity_30_minutes = a

fare_commission_free_wbtc :: Proxy "fare_commission_free_wbtc"
fare_commission_free_wbtc = a

select_route_number :: Proxy "select_route_number"
select_route_number = a

pickup_and_destination_stop :: Proxy "pickup_and_destination_stop"
pickup_and_destination_stop = a

bus_boarded_confirmation :: Proxy "bus_boarded_confirmation"
bus_boarded_confirmation = a

towards_station :: Proxy "towards_station"
towards_station = a

bus_no :: Proxy "bus_no"
bus_no = a

verified :: Proxy "verified"
verified = a

experience_our_pilot_launch_for_bus_ticketing_in_prime_routes :: Proxy "experience_our_pilot_launch_for_bus_ticketing_in_prime_routes"
experience_our_pilot_launch_for_bus_ticketing_in_prime_routes = a

note_your_ticket_is_only_valid_for :: Proxy "note_your_ticket_is_only_valid_for"
note_your_ticket_is_only_valid_for = a

here_is_bus_ticket :: Proxy "here_is_bus_ticket"
here_is_bus_ticket = a

were_you_asked_to_pay_extra_q :: Proxy "were_you_asked_to_pay_extra_q"
were_you_asked_to_pay_extra_q = a

were_you_asked_to_pay_extra_desc :: Proxy "were_you_asked_to_pay_extra_desc"
were_you_asked_to_pay_extra_desc = a

we_are_sorry_to_hear_this_please_click_on_need_help :: Proxy "we_are_sorry_to_hear_this_please_click_on_need_help"
we_are_sorry_to_hear_this_please_click_on_need_help = a

driver_asked_extra_money :: Proxy "driver_asked_extra_money"
driver_asked_extra_money = a

driver_is_asking_for_extra_fare :: Proxy "driver_is_asking_for_extra_fare"
driver_is_asking_for_extra_fare = a
a_c :: Proxy "a_c"
a_c = a

no_oxygen :: Proxy "no_oxygen"
no_oxygen = a

oxygen :: Proxy "oxygen"
oxygen = a

ventilator :: Proxy "ventilator"
ventilator = a

ambulance_ :: Proxy "ambulance_"
ambulance_ = a

uncomfortable_ambulance :: Proxy "uncomfortable_ambulance"
uncomfortable_ambulance = a

clean_ambulance :: Proxy "clean_ambulance"
clean_ambulance = a

ambulance_booking_disclaimer :: Proxy "ambulance_booking_disclaimer"
ambulance_booking_disclaimer = a

different_ambulance :: Proxy "different_ambulance"
different_ambulance = a

no_remaining_tickets :: Proxy "no_remaining_tickets"
no_remaining_tickets = a

ticket_not_booked_refund_initiated :: Proxy "ticket_not_booked_refund_initiated"
ticket_not_booked_refund_initiated = a

parcel_details :: Proxy "parcel_details"
parcel_details = a

parcel_type :: Proxy "parcel_type"
parcel_type = a

parcel_quantity :: Proxy "parcel_quantity"
parcel_quantity = a

unloading_time :: Proxy "unloading_time"
unloading_time = a

loading_time :: Proxy "loading_time"
loading_time = a

helps_smooth_delivery :: Proxy "helps_smooth_delivery"
helps_smooth_delivery = a

free_loading_unloading_time :: Proxy "free_loading_unloading_time"
free_loading_unloading_time = a

max_capacity_warning :: Proxy "max_capacity_warning"
max_capacity_warning = a

select_route :: Proxy "select_route"
select_route = a