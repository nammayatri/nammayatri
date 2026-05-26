-- aadhaar_otp_req (driver)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_aadhaar_otp_req_driver_id ON atlas_driver_offer_bpp.aadhaar_otp_req USING btree (driver_id);

-- aadhaar_otp_verify (driver)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_aadhaar_otp_verify_driver_id ON atlas_driver_offer_bpp.aadhaar_otp_verify USING btree (driver_id);

-- approval_request
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_approval_request_requestee_id ON atlas_driver_offer_bpp.approval_request USING btree (requestee_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_approval_request_requestor_id ON atlas_driver_offer_bpp.approval_request USING btree (requestor_id);

-- black_list_org
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_black_list_org_subscriber_id ON atlas_driver_offer_bpp.black_list_org USING btree (subscriber_id);

-- booking_update_request (bap_booking_update_request_id only; booking_id already has idx_booking_id_burt)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_update_request_bap_id ON atlas_driver_offer_bpp.booking_update_request USING btree (bap_booking_update_request_id);

-- business_license
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_business_license_document_image_id ON atlas_driver_offer_bpp.business_license USING btree (document_image_id);

-- chat_message
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_chat_message_issue_report_id ON atlas_driver_offer_bpp.chat_message USING btree (issue_report_id);

-- coin_config
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_coin_config_merchant_id ON atlas_driver_offer_bpp.coin_config USING btree (merchant_id);

-- coin_history (audit table name: coin_purchase_history)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_coin_history_driver_id ON atlas_driver_offer_bpp.coin_history USING btree (driver_id);

-- comment (issue management)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_comment_issue_report_id ON atlas_driver_offer_bpp.comment USING btree (issue_report_id);

-- common_driver_onboarding_documents
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_common_driver_onboarding_documents_document_image_id ON atlas_driver_offer_bpp.common_driver_onboarding_documents USING btree (document_image_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_common_driver_onboarding_documents_driver_id ON atlas_driver_offer_bpp.common_driver_onboarding_documents USING btree (driver_id);

-- communication_delivery
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_communication_delivery_communication_id ON atlas_driver_offer_bpp.communication_delivery USING btree (communication_id);

-- digilocker_verification
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_digilocker_verification_driver_id ON atlas_driver_offer_bpp.digilocker_verification USING btree (driver_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_digilocker_verification_state_id ON atlas_driver_offer_bpp.digilocker_verification USING btree (state_id);

-- direct_tax_transaction (reference_id only; invoice_number/counterparty_id/transaction_date already indexed by migration 0767)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_direct_tax_transaction_reference_id ON atlas_driver_offer_bpp.direct_tax_transaction USING btree (reference_id);

-- document_reminder_history
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_document_reminder_history_document_type ON atlas_driver_offer_bpp.document_reminder_history USING btree (document_type);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_document_reminder_history_entity_id ON atlas_driver_offer_bpp.document_reminder_history USING btree (entity_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_document_reminder_history_entity_type ON atlas_driver_offer_bpp.document_reminder_history USING btree (entity_type);

-- driver_bank_account
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_bank_account_account_id ON atlas_driver_offer_bpp.driver_bank_account USING btree (account_id);

-- driver_gstin
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_gstin_driver_id ON atlas_driver_offer_bpp.driver_gstin USING btree (driver_id);

-- driver_operator_association
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_operator_association_driver_id ON atlas_driver_offer_bpp.driver_operator_association USING btree (driver_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_operator_association_operator_id ON atlas_driver_offer_bpp.driver_operator_association USING btree (operator_id);

-- driver_plan (mandate_id only)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_plan_mandate_id ON atlas_driver_offer_bpp.driver_plan USING btree (mandate_id);

-- driver_referral (dynamic_referral_code only; driver_id already indexed in prod)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_referral_dynamic_referral_code ON atlas_driver_offer_bpp.driver_referral USING btree (dynamic_referral_code);

-- driver_ssn
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_ssn_driver_id ON atlas_driver_offer_bpp.driver_ssn USING btree (driver_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_ssn_ssn_hash ON atlas_driver_offer_bpp.driver_ssn USING btree (ssn_hash);

-- driver_udyam
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_udyam_driver_id ON atlas_driver_offer_bpp.driver_udyam USING btree (driver_id);

-- finance_invoice (reference_id only; invoice_number/issued_to_id/payment_order_id/supplier_id already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_invoice_reference_id ON atlas_driver_offer_bpp.finance_invoice USING btree (reference_id);

-- finance_reconciliation_entry (recon_status only; booking_id/dco_id/summary_id/reconciliation_date already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_reconciliation_entry_recon_status ON atlas_driver_offer_bpp.finance_reconciliation_entry USING btree (recon_status);

-- finance_reconciliation_summary (reconciliation_type only; reconciliation_date already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_reconciliation_summary_reconciliation_type ON atlas_driver_offer_bpp.finance_reconciliation_summary USING btree (reconciliation_type);

-- fleet_badge
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_badge_badge_name ON atlas_driver_offer_bpp.fleet_badge USING btree (badge_name);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_badge_badge_type ON atlas_driver_offer_bpp.fleet_badge USING btree (badge_type);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_badge_fleet_owner_id ON atlas_driver_offer_bpp.fleet_badge USING btree (fleet_owner_id);

-- fleet_badge_association
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_badge_association_badge_id ON atlas_driver_offer_bpp.fleet_badge_association USING btree (badge_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_badge_association_badge_type ON atlas_driver_offer_bpp.fleet_badge_association USING btree (badge_type);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_badge_association_driver_id ON atlas_driver_offer_bpp.fleet_badge_association USING btree (driver_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_badge_association_fleet_owner_id ON atlas_driver_offer_bpp.fleet_badge_association USING btree (fleet_owner_id);

-- fleet_driver_association (fleet_owner_id only; driver_id already indexed in prod)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_driver_association_fleet_owner_id ON atlas_driver_offer_bpp.fleet_driver_association USING btree (fleet_owner_id);

-- fleet_operator_association
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_operator_association_fleet_owner_id ON atlas_driver_offer_bpp.fleet_operator_association USING btree (fleet_owner_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_operator_association_operator_id ON atlas_driver_offer_bpp.fleet_operator_association USING btree (operator_id);

-- fleet_rc_association
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_rc_association_rc_id ON atlas_driver_offer_bpp.fleet_rc_association USING btree (rc_id);

-- iffco_tokio_insurance
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_iffco_tokio_insurance_driver_id ON atlas_driver_offer_bpp.iffco_tokio_insurance USING btree (driver_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_iffco_tokio_insurance_invoice_request_number ON atlas_driver_offer_bpp.iffco_tokio_insurance USING btree (invoice_request_number);

-- igm_config
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_igm_config_merchant_id ON atlas_driver_offer_bpp.igm_config USING btree (merchant_id);

-- indirect_tax_transaction (reference_id only; counterparty_id/invoice_number/transaction_date already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_indirect_tax_transaction_reference_id ON atlas_driver_offer_bpp.indirect_tax_transaction USING btree (reference_id);

-- issue_category
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_category_category ON atlas_driver_offer_bpp.issue_category USING btree (category);

-- issue_option
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_option_issue_category_id ON atlas_driver_offer_bpp.issue_option USING btree (issue_category_id);

-- issue_report (person_id and category_id only; ticket_id/short_id already indexed)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_report_person_id ON atlas_driver_offer_bpp.issue_report USING btree (person_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_report_category_id ON atlas_driver_offer_bpp.issue_report USING btree (category_id);

-- issue_translation
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_issue_translation_language ON atlas_driver_offer_bpp.issue_translation USING btree (language);

-- kiosk_location_translation
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_kiosk_location_translation_language ON atlas_driver_offer_bpp.kiosk_location_translation USING btree (language);

-- knowledge_center
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_knowledge_center_sop_type ON atlas_driver_offer_bpp.knowledge_center USING btree (sop_type);

-- lms_certificate
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_lms_certificate_module_completion_id ON atlas_driver_offer_bpp.lms_certificate USING btree (module_completion_id);

-- merchant_overlay (overlay_key only; existing composites have overlay_key as 2nd col, not useful for SK lookup)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_merchant_overlay_overlay_key ON atlas_driver_offer_bpp.merchant_overlay USING btree (overlay_key);

-- message_dictionary
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_message_dictionary_message_key ON atlas_driver_offer_bpp.message_dictionary USING btree (message_key);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_message_dictionary_message_type ON atlas_driver_offer_bpp.message_dictionary USING btree (message_type);

-- morth_verification
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_morth_verification_doc_type ON atlas_driver_offer_bpp.morth_verification USING btree (doc_type);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_morth_verification_document_number_hash ON atlas_driver_offer_bpp.morth_verification USING btree (document_number_hash);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_morth_verification_driver_id ON atlas_driver_offer_bpp.morth_verification USING btree (driver_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_morth_verification_request_id ON atlas_driver_offer_bpp.morth_verification USING btree (request_id);

-- offer (payment lib table)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_offer_offer_code ON atlas_driver_offer_bpp.offer USING btree (offer_code);

-- offline_offer
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_offline_offer_reference_id ON atlas_driver_offer_bpp.offline_offer USING btree (reference_id);

-- payment_order (domain_entity_id only; short_id/person_id already indexed in prod; group_id in migration 0750)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payment_order_domain_entity_id ON atlas_driver_offer_bpp.payment_order USING btree (domain_entity_id);

-- payment_order_offer
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payment_order_offer_payment_order_id ON atlas_driver_offer_bpp.payment_order_offer USING btree (payment_order_id);

-- payment_order_split
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payment_order_split_payment_order_id ON atlas_driver_offer_bpp.payment_order_split USING btree (payment_order_id);

-- person (mobile_number_hash standalone; existing composite 0041 has mobile_country_code as leading col so
-- cannot serve single-field mobile_number_hash lookups. Also add alternate_mobile_number_hash and operator_badge_token)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_person_mobile_number_hash ON atlas_driver_offer_bpp.person USING btree (mobile_number_hash);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_person_alternate_mobile_number_hash ON atlas_driver_offer_bpp.person USING btree (alternate_mobile_number_hash);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_person_operator_badge_token ON atlas_driver_offer_bpp.person USING btree (operator_badge_token);

-- person_default_emergency_number (contact_person_id and person_id; mobile_number_hash is in PK)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_person_default_emergency_number_contact_person_id ON atlas_driver_offer_bpp.person_default_emergency_number USING btree (contact_person_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_person_default_emergency_number_person_id ON atlas_driver_offer_bpp.person_default_emergency_number USING btree (person_id);

-- quote_special_zone
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_quote_special_zone_search_request_id ON atlas_driver_offer_bpp.quote_special_zone USING btree (search_request_id);

-- refunds (payment lib table)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_refunds_order_id ON atlas_driver_offer_bpp.refunds USING btree (order_id);

-- reminder
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_reminder_document_type ON atlas_driver_offer_bpp.reminder USING btree (document_type);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_reminder_driver_id ON atlas_driver_offer_bpp.reminder USING btree (driver_id);

-- search_request_for_driver (request_id only; search_try_id already indexed in prod)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_search_request_for_driver_request_id ON atlas_driver_offer_bpp.search_request_for_driver USING btree (request_id);

-- settlement_file_info
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_settlement_file_info_file_name ON atlas_driver_offer_bpp.settlement_file_info USING btree (file_name);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_settlement_file_info_payment_gateway_name ON atlas_driver_offer_bpp.settlement_file_info USING btree (payment_gateway_name);

-- station
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_station_code ON atlas_driver_offer_bpp.station USING btree (code);

-- stcl_membership (driver_id already indexed in prod; add remaining 4 SKs)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_stcl_membership_account_number_hash ON atlas_driver_offer_bpp.stcl_membership USING btree (account_number_hash);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_stcl_membership_application_id ON atlas_driver_offer_bpp.stcl_membership USING btree (application_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_stcl_membership_mobile_number_hash ON atlas_driver_offer_bpp.stcl_membership USING btree (mobile_number_hash);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_stcl_membership_pan_number_hash ON atlas_driver_offer_bpp.stcl_membership USING btree (pan_number_hash);

-- stop_information (driver version has both ride_id and stop_loc_id as SKs)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_stop_information_ride_id ON atlas_driver_offer_bpp.stop_information USING btree (ride_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_stop_information_stop_loc_id ON atlas_driver_offer_bpp.stop_information USING btree (stop_loc_id);

-- tds_distribution_pdf_file
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_tds_distribution_pdf_file_tds_distribution_record_id ON atlas_driver_offer_bpp.tds_distribution_pdf_file USING btree (tds_distribution_record_id);

-- tds_distribution_record
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_tds_distribution_record_driver_id ON atlas_driver_offer_bpp.tds_distribution_record USING btree (driver_id);

-- trip_alert_request
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_trip_alert_request_driver_id ON atlas_driver_offer_bpp.trip_alert_request USING btree (driver_id);

-- vehicle_fitness_certificate (rc_id only; document_image_id already has idx_vehicle_fitness_certificate_image_id)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_vehicle_fitness_certificate_rc_id ON atlas_driver_offer_bpp.vehicle_fitness_certificate USING btree (rc_id);

-- vehicle_insurance (rc_id only; document_image_id already has idx_vehicle_insurance_document_image_id)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_vehicle_insurance_rc_id ON atlas_driver_offer_bpp.vehicle_insurance USING btree (rc_id);

-- vehicle_noc
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_vehicle_noc_document_image_id ON atlas_driver_offer_bpp.vehicle_noc USING btree (document_image_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_vehicle_noc_rc_id ON atlas_driver_offer_bpp.vehicle_noc USING btree (rc_id);

-- vehicle_permit
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_vehicle_permit_rc_id ON atlas_driver_offer_bpp.vehicle_permit USING btree (rc_id);

-- vehicle_puc
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_vehicle_puc_rc_id ON atlas_driver_offer_bpp.vehicle_puc USING btree (rc_id);

-- volunteer
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_volunteer_place ON atlas_driver_offer_bpp.volunteer USING btree (place);

-- wallet
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_wallet_account_id ON atlas_driver_offer_bpp.wallet USING btree (account_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_wallet_person_id ON atlas_driver_offer_bpp.wallet USING btree (person_id);

-- wallet_history
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_wallet_history_wallet_id ON atlas_driver_offer_bpp.wallet_history USING btree (wallet_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_wallet_history_wallet_payments_id ON atlas_driver_offer_bpp.wallet_history USING btree (wallet_payments_id);

-- wallet_payments
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_wallet_payments_order_id ON atlas_driver_offer_bpp.wallet_payments USING btree (order_id);

-- white_list_org
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_white_list_org_subscriber_id ON atlas_driver_offer_bpp.white_list_org USING btree (subscriber_id);
