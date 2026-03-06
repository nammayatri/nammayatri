-- Airport parking / entry fee: amount (INR) charged per gate
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN IF NOT EXISTS entry_fee_amount double precision;
