-- aadhaar_otp_req
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_aadhaar_otp_req_person_id ON atlas_app.aadhaar_otp_req USING btree (person_id);

-- aadhaar_otp_verify
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_aadhaar_otp_verify_person_id ON atlas_app.aadhaar_otp_verify USING btree (person_id);

-- booking (bpp_booking_id and rider_transaction_id; rider_id/quote_id already indexed in prod)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_bpp_booking_id ON atlas_app.booking USING btree (bpp_booking_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_rider_transaction_id ON atlas_app.booking USING btree (rider_transaction_id);

-- booking_update_request
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_update_request_booking_id ON atlas_app.booking_update_request USING btree (booking_id);

-- bpp_details
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_bpp_details_subscriber_id ON atlas_app.bpp_details USING btree (subscriber_id);

-- comment (issue management)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_comment_issue_report_id ON atlas_app.comment USING btree (issue_report_id);

-- deleted_person
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_deleted_person_static_person_id ON atlas_app.deleted_person USING btree (static_person_id);

-- direct_tax_transaction (reference_id only; invoice_number/counterparty_id/transaction_date already indexed by migration 1500)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_direct_tax_transaction_reference_id ON atlas_app.direct_tax_transaction USING btree (reference_id);

-- finance_invoice (payment_order_id and reference_id; invoice_number/issued_to_id/supplier_id already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_invoice_payment_order_id ON atlas_app.finance_invoice USING btree (payment_order_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_invoice_reference_id ON atlas_app.finance_invoice USING btree (reference_id);

-- finance_ledger_entry (concerned_individual_id, from_account_id, to_account_id; reference_id/settlement_id already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_ledger_entry_concerned_individual_id ON atlas_app.finance_ledger_entry USING btree (concerned_individual_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_ledger_entry_from_account_id ON atlas_app.finance_ledger_entry USING btree (from_account_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_ledger_entry_to_account_id ON atlas_app.finance_ledger_entry USING btree (to_account_id);

-- finance_reconciliation_entry (recon_status only; booking_id/dco_id/summary_id/reconciliation_date already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_reconciliation_entry_recon_status ON atlas_app.finance_reconciliation_entry USING btree (recon_status);

-- finance_reconciliation_summary (reconciliation_type only; reconciliation_date already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_reconciliation_summary_reconciliation_type ON atlas_app.finance_reconciliation_summary USING btree (reconciliation_type);

-- frfs_cancellation_config
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_cancellation_config_vehicle_category ON atlas_app.frfs_cancellation_config USING btree (vehicle_category);

-- frfs_quote_breakup
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_quote_breakup_quote_category_id ON atlas_app.frfs_quote_breakup USING btree (quote_category_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_quote_breakup_quote_id ON atlas_app.frfs_quote_breakup USING btree (quote_id);

-- frfs_search
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_search_rider_id ON atlas_app.frfs_search USING btree (rider_id);

-- frfs_ticket_booking_breakup
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_breakup_quote_category_id ON atlas_app.frfs_ticket_booking_breakup USING btree (quote_category_id);

-- frfs_ticket_booking_feedback
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_feedback_booking_id ON atlas_app.frfs_ticket_booking_feedback USING btree (booking_id);

-- igm_config
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_igm_config_merchant_id ON atlas_app.igm_config USING btree (merchant_id);

-- indirect_tax_transaction (reference_id only; invoice_number/counterparty_id/transaction_date already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_indirect_tax_transaction_reference_id ON atlas_app.indirect_tax_transaction USING btree (reference_id);

-- issue (customer_id only; ticket_id already has idx_ticket_id)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_customer_id ON atlas_app.issue USING btree (customer_id);

-- issue_category
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_category_category ON atlas_app.issue_category USING btree (category);

-- issue_chat (issue_report_id only; person_id/ticket_id already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_chat_issue_report_id ON atlas_app.issue_chat USING btree (issue_report_id);

-- issue_option
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_option_issue_category_id ON atlas_app.issue_option USING btree (issue_category_id);

-- issue_report (category_id only; person_id/ticket_id already have prod indexes; short_id already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_report_category_id ON atlas_app.issue_report USING btree (category_id);

-- issue_translation
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_translation_language ON atlas_app.issue_translation USING btree (language);

-- merchant (subscriber_id only; short_id already has a prod index)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_merchant_subscriber_id ON atlas_app.merchant USING btree (subscriber_id);

-- merchant_onboarding_step
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_merchant_onboarding_step_merchant_onboarding_id ON atlas_app.merchant_onboarding_step USING btree (merchant_onboarding_id);

-- ny_regular_instance_log
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_ny_regular_instance_log_ny_regular_subscription_id ON atlas_app.ny_regular_instance_log USING btree (ny_regular_subscription_id);

-- ny_regular_subscriptions
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_ny_regular_subscriptions_user_id ON atlas_app.ny_regular_subscriptions USING btree (user_id);

-- offer_entity
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_offer_entity_entity_id ON atlas_app.offer_entity USING btree (entity_id);

-- offline_offer
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_offline_offer_reference_id ON atlas_app.offline_offer USING btree (reference_id);

-- parking_transaction
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_parking_transaction_payment_order_id ON atlas_app.parking_transaction USING btree (payment_order_id);

-- partner_invoice_data_log
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_partner_invoice_data_log_booking_id ON atlas_app.partner_invoice_data_log USING btree (booking_id);

-- partner_organization
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_partner_organization_org_id ON atlas_app.partner_organization USING btree (org_id);

-- pass
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pass_pass_type_id ON atlas_app.pass USING btree (pass_type_id);

-- pass_organization
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pass_organization_depot_person_id ON atlas_app.pass_organization USING btree (depot_person_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pass_organization_person_id ON atlas_app.pass_organization USING btree (person_id);

-- pass_type
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pass_type_pass_category_id ON atlas_app.pass_type USING btree (pass_category_id);

-- payment_order (person_id only; short_id/group_id already indexed in prod)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payment_order_person_id ON atlas_app.payment_order USING btree (person_id);

-- payment_transaction_offer_payout
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payment_transaction_offer_payout_network_order_id ON atlas_app.payment_transaction_offer_payout USING btree (network_order_id);

-- person (operator_badge_token only; mobileNumberHash/customerReferralCode/deviceToken/emailHash/referralCode already indexed in prod)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_person_operator_badge_token ON atlas_app.person USING btree (operator_badge_token);

-- person_default_emergency_number (contact_person_id only; mobile_number_hash/person_id are in PK)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_person_default_emergency_number_contact_person_id ON atlas_app.person_default_emergency_number USING btree (contact_person_id);

-- pickup_instructions
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pickup_instructions_person_id ON atlas_app.pickup_instructions USING btree (person_id);

-- purchased_pass_payment (person_id only; order_id/purchased_pass_id already have prod indexes)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_purchased_pass_payment_person_id ON atlas_app.purchased_pass_payment USING btree (person_id);

-- refund_request
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_refund_request_order_id ON atlas_app.refund_request USING btree (order_id);

-- refunds (payment lib table)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_refunds_order_id ON atlas_app.refunds USING btree (order_id);

-- rider_config
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_rider_config_frfs_metrics_api_key ON atlas_app.rider_config USING btree (frfs_metrics_api_key);

-- seat
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_seat_seat_layout_id ON atlas_app.seat USING btree (seat_layout_id);

-- seat_management: existing composite (date, ticket_service_category_id) has date as leading column
-- so ticket_service_category_id alone is not efficiently served by it
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_seat_management_ticket_service_category_id ON atlas_app.seat_management USING btree (ticket_service_category_id);

-- settlement_file_info
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_settlement_file_info_file_name ON atlas_app.settlement_file_info USING btree (file_name);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_settlement_file_info_payment_gateway_name ON atlas_app.settlement_file_info USING btree (payment_gateway_name);

-- stop_information
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_stop_information_ride_id ON atlas_app.stop_information USING btree (ride_id);

-- svp_journey
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_svp_journey_rider_id ON atlas_app.svp_journey USING btree (rider_id);

-- ticket_sub_place
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_ticket_sub_place_ticket_place_id ON atlas_app.ticket_sub_place USING btree (ticket_place_id);

-- translations (language only; message_key already covered by composite prod index message_key+language)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_translations_language ON atlas_app.translations USING btree (language);

-- wallet
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_wallet_account_id ON atlas_app.wallet USING btree (account_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_wallet_person_id ON atlas_app.wallet USING btree (person_id);

-- wallet_history
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_wallet_history_wallet_id ON atlas_app.wallet_history USING btree (wallet_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_wallet_history_wallet_payments_id ON atlas_app.wallet_history USING btree (wallet_payments_id);

-- wallet_payments
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_wallet_payments_order_id ON atlas_app.wallet_payments USING btree (order_id);

-- white_list_org
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_white_list_org_subscriber_id ON atlas_app.white_list_org USING btree (subscriber_id);
