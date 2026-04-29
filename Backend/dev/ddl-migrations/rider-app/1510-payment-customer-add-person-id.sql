

-- Step 4: Drop old primary key and add new one on (person_id, payment_mode)
ALTER TABLE atlas_app.payment_customer DROP CONSTRAINT IF EXISTS payment_customer_pkey;
ALTER TABLE atlas_app.payment_customer ADD PRIMARY KEY (person_id, payment_mode);

-- Step 5: Keep an index on customer_id for reverse lookups
CREATE INDEX IF NOT EXISTS idx_payment_customer_customer_id
ON atlas_app.payment_customer (customer_id);
