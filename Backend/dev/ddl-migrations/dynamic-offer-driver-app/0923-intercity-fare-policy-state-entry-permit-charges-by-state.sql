-- State entry permit charge per destination state, e.g. {"TamilNadu": 300, "Telangana": 500}.
-- state_entry_permit_charges stays for older pods but is no longer read.
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN state_entry_permit_charges_by_state json;
