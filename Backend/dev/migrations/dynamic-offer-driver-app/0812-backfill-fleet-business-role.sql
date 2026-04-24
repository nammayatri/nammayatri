-- Not needed for NY prod

UPDATE atlas_driver_offer_bpp.person p
SET role = 'FLEET_BUSINESS', updated_at = now()
FROM atlas_driver_offer_bpp.fleet_owner_information foi
WHERE foi.fleet_owner_person_id = p.id
  AND foi.fleet_type = 'BUSINESS_FLEET'
  AND p.role = 'FLEET_OWNER';
