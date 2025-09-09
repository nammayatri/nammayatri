INSERT INTO atlas_app.frfs_ticket_category_metadata_config (
    id,
    category,
    code,
    description,
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
    'ADULT',
    'Standard terms and conditions apply',
    'BUS',
    moc.merchant_id,
    moc.id,
    NOW(),
    NOW()
FROM atlas_app.merchant_operating_city moc;

CREATE INDEX idx_frfs_quote_category_quote_id_bpp_item_id ON atlas_app.frfs_quote_category  (quote_id,bpp_item_id);