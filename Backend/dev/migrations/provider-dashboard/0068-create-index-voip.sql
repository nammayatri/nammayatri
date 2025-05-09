CREATE INDEX idx_voip_call_status_call_id ON atlas_driver_offer_bpp.voip_call_status USING btree (call_id) WHERE call_id IS NOT NULL;
