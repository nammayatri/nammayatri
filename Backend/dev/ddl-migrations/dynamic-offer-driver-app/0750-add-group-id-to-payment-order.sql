ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN group_id character varying(255);

CREATE INDEX idx_payment_order_group_id ON atlas_driver_offer_bpp.payment_order USING btree (group_id);
