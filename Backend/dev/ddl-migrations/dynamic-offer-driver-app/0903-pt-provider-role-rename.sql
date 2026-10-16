-- BUS_CHECKER/BUS_DISPATCHER were replaced by PT_PROVIDER on this branch. The column is text and
-- parsed via the derived enum, so a row still holding an old value fails to decode and every
-- endpoint for that person breaks -- not just the public-transport ones.
UPDATE atlas_driver_offer_bpp.person
   SET role = 'PT_PROVIDER'
 WHERE role IN ('BUS_CHECKER', 'BUS_DISPATCHER');
