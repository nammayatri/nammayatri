-- Local dev fixes applied after every Sync Data.
-- Overrides production/master values that break local integration tests.

-- Fix geometry state: master DB stores 'Delhi' but Haskell IndianState enum
-- requires 'NationalCapitalTerritory' for Delhi — causes BeamRowReadError on rideSearch.
UPDATE atlas_app.geometry SET state = 'NationalCapitalTerritory' WHERE state = 'Delhi';
UPDATE atlas_driver_offer_bpp.geometry SET state = 'NationalCapitalTerritory' WHERE state = 'Delhi';

