CREATE TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ();

ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN active boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN area text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN blacklist_driver_for_rider_seconds integer ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN cancelled_by text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN collection_mode text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN counts_toward_customer_cancellation_stats boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN counts_toward_driver_cancellation_rate boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN customer_commission_and_tax text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN customer_deduction text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN customer_notification_key text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN driver_deduction text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN driver_notification_key text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN exempt_dashboard_bookings boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN fault_rule text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN fault_verdict text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN max_waive_offs_per_period integer ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN payment_instrument text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN trip_category text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN vehicle_service_tier text ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN waive_off_allowed boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN waive_off_period_days integer ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN consume_ride_credit_on_cancellation boolean ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_consequence_matrix ADD COLUMN carry_forward_dues boolean ;