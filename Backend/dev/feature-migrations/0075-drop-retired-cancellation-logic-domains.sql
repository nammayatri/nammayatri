-- Drop the three retired cancellation JsonLogic domains.
--
-- USER-CANCELLATION-DUES, USER-CANCELLATION-DUES-WAIVE-OFF and CANCELLATION-COIN-POLICY were
-- absorbed by cancellation_consequence_matrix (fees, deductions, coin amounts and waive-off are
-- all matrix columns now; JsonLogic keeps only CANCELLATION-FAULT-VERDICT for fault attribution).
-- No code path has evaluated them since that migration, so these rows are dead config.
--
-- This MUST run with or before the deploy that removes the constructors from
-- Lib.Yudhishthira.Types.LogicDomain: Lib.Yudhishthira.Flow.Dashboard reads rollouts per city
-- with findAllByMerchantOpCityId — no domain filter — so any surviving row with a retired
-- domain would fail to parse and break that endpoint.

BEGIN;

DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic_rollout
WHERE domain IN (
  'USER-CANCELLATION-DUES',
  'USER-CANCELLATION-DUES-WAIVE-OFF',
  'CANCELLATION-COIN-POLICY'
);

DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic_element
WHERE domain IN (
  'USER-CANCELLATION-DUES',
  'USER-CANCELLATION-DUES-WAIVE-OFF',
  'CANCELLATION-COIN-POLICY'
);

DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic_always_on
WHERE domain IN (
  'USER-CANCELLATION-DUES',
  'USER-CANCELLATION-DUES-WAIVE-OFF',
  'CANCELLATION-COIN-POLICY'
);

DELETE FROM atlas_driver_offer_bpp.app_dynamic_logic
WHERE domain IN (
  'USER-CANCELLATION-DUES',
  'USER-CANCELLATION-DUES-WAIVE-OFF',
  'CANCELLATION-COIN-POLICY'
);

COMMIT;

-- Expected counts at time of writing (production, driver DB):
--   app_dynamic_logic_element: CANCELLATION-COIN-POLICY 6, USER-CANCELLATION-DUES 22,
--                              USER-CANCELLATION-DUES-WAIVE-OFF 4
--   app_dynamic_logic_rollout: 2, 2, 1 respectively
-- The rider DB (atlas_app) holds no rows for these domains.
