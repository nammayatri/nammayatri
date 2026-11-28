-- Corporate shuttle: the partner companies, fixed for now: Namma Yatri and Juspay, in Bangalore (NAMMA_YATRI). A company is a
-- RegularPass pass organisation with its work-email domains (an employee whose verified business email is on one of them can
-- fill the interest form) and its office (the drop of every to-office trip; every planned route ends there).
-- person_id is the pass dashboards' organisation admin: a placeholder is enough for the shuttle.
-- Idempotent: a company already in the city (by name) is left as it is.
INSERT INTO atlas_app.pass_organization (
    id,
    name,
    pass_enum,
    person_id,
    merchant_id,
    merchant_operating_city_id,
    address,
    email_domains,
    office_lat,
    office_lon,
    created_at,
    updated_at
)
SELECT
    md5('corporate-shuttle-org-' || company.code || '-' || moc.id)::uuid::text,
    company.name,
    'RegularPass',
    md5('corporate-shuttle-org-admin-' || company.code || '-' || moc.id)::uuid::text,
    moc.merchant_id,
    moc.id,
    company.address,
    company.email_domains,
    company.office_lat,
    company.office_lon,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
JOIN atlas_app.merchant m ON m.id = moc.merchant_id
CROSS JOIN (
    VALUES
        ('nammayatri', 'Namma Yatri', 'Namma Yatri office, Koramangala, Bengaluru', '{nammayatri.in}'::text[], 12.9422500, 77.6220637),
        ('juspay', 'Juspay', 'Juspay office, Koramangala, Bengaluru', '{juspay.in}'::text[], 12.937152, 77.621624)
) AS company (code, name, address, email_domains, office_lat, office_lon)
WHERE m.short_id = 'NAMMA_YATRI'
  AND moc.city = 'Bangalore'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_app.pass_organization po
    WHERE po.merchant_operating_city_id = moc.id AND po.name = company.name
  );
