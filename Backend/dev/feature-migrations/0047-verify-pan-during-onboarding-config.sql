-- Add verifyPanDuringOnboarding flag to transporter_config.
-- When true: PAN is verified synchronously via Idfy/HyperVerge at upload time (old behaviour).
-- When false (default): PAN is stored as PENDING and verified after the driver's first ride.
ALTER TABLE atlas_driver_offer_bpp.transporter_config
  ADD COLUMN IF NOT EXISTS verify_pan_during_onboarding boolean DEFAULT false;
