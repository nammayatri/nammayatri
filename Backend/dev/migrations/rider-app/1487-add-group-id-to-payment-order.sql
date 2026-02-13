ALTER TABLE atlas_app.payment_order ADD COLUMN group_id character varying(255);

CREATE INDEX idx_payment_order_group_id ON atlas_app.payment_order USING btree (group_id);
