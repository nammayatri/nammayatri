CREATE INDEX idx_driver_fee_collected_at ON atlas_driver_offer_bpp.driver_fee USING btree (collected_at);
CREATE INDEX idx_driver_fee_end_time ON atlas_driver_offer_bpp.driver_fee USING btree (end_time);
CREATE INDEX idx_driver_fee_pay_by ON atlas_driver_offer_bpp.driver_fee USING btree (pay_by);
CREATE INDEX idx_driver_fee_status ON atlas_driver_offer_bpp.driver_fee USING btree (status);
-- collected_by?