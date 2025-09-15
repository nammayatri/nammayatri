INSERT INTO atlas_app.frfs_ticket_category_metadata_config (
    id,
    category,
    code,
    description,
    domain_category_value,
    title,
    tnc,
    vehicle_category,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    'ADULT',
    'ADULT',
    'Standard ticket category for adult passengers',
    'Percentage 0',  -- domain_category_value
    'ADULT',
    'Standard terms and conditions apply',
    'BUS',
    moc.merchant_id,
    moc.id,
    NOW(),
    NOW()
FROM atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.frfs_ticket_category_metadata_config (
    id,
    category,
    code,
    description,
    domain_category_value,
    title,
    tnc,
    vehicle_category,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    'ADULT',
    'ADULT',
    'Standard ticket category for adult passengers',
    'Percentage 0',  -- domain_category_value
    'ADULT',
    'Standard terms and conditions apply',
    'METRO',
    moc.merchant_id,
    moc.id,
    NOW(),
    NOW()
FROM atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.frfs_ticket_category_metadata_config (
    id,
    category,
    code,
    description,
    domain_category_value,
    title,
    tnc,
    vehicle_category,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    'ADULT',
    'ADULT',
    'Standard ticket category for adult passengers',
    'Percentage 0',  -- domain_category_value
    'ADULT',
    'Standard terms and conditions apply',
    'SUBWAY',
    moc.merchant_id,
    moc.id,
    NOW(),
    NOW()
FROM atlas_app.merchant_operating_city moc;

-- Add Index for quote_id  (Production DB)
CREATE INDEX idx_frfs_quote_category_quote_id ON atlas_app.frfs_quote_category USING btree (quote_id);

UPDATE atlas_app.app_dynamic_logic_element SET domain = 'FRFS-TICKET-CATEGORIES' WHERE domain = 'FRFS-DISCOUNTS';

UPDATE atlas_app.app_dynamic_logic_rollout SET domain = 'FRFS-TICKET-CATEGORIES' WHERE domain = 'FRFS-DISCOUNTS';