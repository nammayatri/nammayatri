-- Add personId column to payment_customer and migrate data from person table
-- Unifies customer ID storage: payment_customer becomes single source of truth

-- Step 2: Populate person_id and default_payment_method_id from person table
UPDATE atlas_app.payment_customer pc
SET person_id = p.id,
    default_payment_method_id = CASE
      WHEN pc.payment_mode = 'LIVE' THEN p.default_payment_method_id
      WHEN pc.payment_mode = 'TEST' THEN p.default_test_payment_method_id
    END
FROM atlas_app.person p
WHERE (pc.payment_mode = 'LIVE' AND p.customer_payment_id = pc.customer_id)
   OR (pc.payment_mode = 'TEST' AND p.customer_test_payment_id = pc.customer_id);

-- Step 3: For any existing person rows with customer IDs but no payment_customer entry, create them
INSERT INTO atlas_app.payment_customer (person_id, customer_id, payment_mode, default_payment_method_id, created_at, updated_at)
SELECT p.id, p.customer_payment_id, 'LIVE', p.default_payment_method_id, now(), now()
FROM atlas_app.person p
WHERE p.customer_payment_id IS NOT NULL
AND NOT EXISTS (
  SELECT 1 FROM atlas_app.payment_customer pc
  WHERE pc.customer_id = p.customer_payment_id AND pc.payment_mode = 'LIVE'
)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.payment_customer (person_id, customer_id, payment_mode, default_payment_method_id, created_at, updated_at)
SELECT p.id, p.customer_test_payment_id, 'TEST', p.default_test_payment_method_id, now(), now()
FROM atlas_app.person p
WHERE p.customer_test_payment_id IS NOT NULL
AND NOT EXISTS (
  SELECT 1 FROM atlas_app.payment_customer pc
  WHERE pc.customer_id = p.customer_test_payment_id AND pc.payment_mode = 'TEST'
)
ON CONFLICT DO NOTHING;

-- Step 4: Drop old primary key and add new one on (person_id, payment_mode)
ALTER TABLE atlas_app.payment_customer DROP CONSTRAINT IF EXISTS payment_customer_pkey;
ALTER TABLE atlas_app.payment_customer ADD PRIMARY KEY (person_id, payment_mode);

-- Step 5: Keep an index on customer_id for reverse lookups
CREATE INDEX IF NOT EXISTS idx_payment_customer_customer_id
ON atlas_app.payment_customer (customer_id);
