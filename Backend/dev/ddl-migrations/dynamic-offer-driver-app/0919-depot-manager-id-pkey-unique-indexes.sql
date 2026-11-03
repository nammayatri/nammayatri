-- One row per person per depot, as the old composite primary key enforced.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_depot_manager_person_depot_unique
  ON atlas_driver_offer_bpp.depot_manager USING btree (person_id, depot_code)
  WHERE depot_code IS NOT NULL;

-- At most one depot-less checker row per person; PG 14 UNIQUE treats NULLs as distinct.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_depot_manager_person_checker_unique
  ON atlas_driver_offer_bpp.depot_manager USING btree (person_id)
  WHERE depot_code IS NULL;
