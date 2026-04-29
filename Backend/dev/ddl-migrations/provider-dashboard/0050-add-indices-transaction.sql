CREATE INDEX idx_transaction_requestor_id ON atlas_bpp_dashboard.transaction USING btree (requestor_id);
CREATE INDEX idx_transaction_common_driver_id ON atlas_bpp_dashboard.transaction USING btree (common_driver_id);
CREATE INDEX idx_transaction_common_ride_id ON atlas_bpp_dashboard.transaction USING btree (common_ride_id);