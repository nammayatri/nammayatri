CREATE INDEX idx_hv_verification_req_id ON atlas_driver_offer_bpp.hyperverge_verification USING btree (request_id);

CREATE INDEX idx_hv_verification_driver_id ON atlas_driver_offer_bpp.hyperverge_verification USING btree (driver_id);