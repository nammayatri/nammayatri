ALTER TABLE atlas_driver_offer_bpp.person
    ADD COLUMN unencrypted_mobile_number character varying(255);

UPDATE atlas_driver_offer_bpp.person
    set unencrypted_mobile_number = '6666666666' where id = 'favorit-suv-000000000000000000000000';

UPDATE atlas_driver_offer_bpp.person
    set unencrypted_mobile_number = '6666666666' where id = 'favorit-sedan-0000000000000000000000';

UPDATE atlas_driver_offer_bpp.person
    set unencrypted_mobile_number = '9999999999' where id = 'favorit-admin-0000000000000000000000';

UPDATE atlas_driver_offer_bpp.person
    set unencrypted_mobile_number = '9999988888' where id = 'nearest-drivers-admin-00000000000000';
