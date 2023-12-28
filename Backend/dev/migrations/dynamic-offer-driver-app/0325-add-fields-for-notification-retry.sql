
alter table atlas_driver_offer_bpp.driver_fee add column notification_retry_count int default 0;
alter table atlas_driver_offer_bpp.transporter_config add column notification_retry_eligible_error_codes text[] default ARRAY['UC1', 'UC2', 'UC5', 'NU'];
alter table atlas_driver_offer_bpp.transporter_config add column notification_retry_count_threshold int default 3;
alter table atlas_driver_offer_bpp.transporter_config add column notification_retry_time_gap bigint default 900;