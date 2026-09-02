-- Resync every serial sequence in the BPP schema to MAX(column)+1.
--
-- Why: config-sync inserts rows with EXPLICIT ids, which leaves the owned
-- sequences behind; the next DEFAULT-valued insert then draws an already-used
-- id and fails with 23505 (seen on
-- fare_policy_progressive_details_per_extra_km_rate_section: nextval gave 1
-- under seeded ids 1..1394+). config_transfer.py now does this automatically
-- after every sync; run this once by hand to repair a DB synced before that.
--
-- Usage (dev stack must be running):
--   PGPASSWORD=atlas psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user -d atlas_dev -f dev/config-sync/bin/resync-sequences.sql
DO $$
DECLARE r RECORD; fixed INT := 0;
BEGIN
  FOR r IN
    SELECT ns.nspname AS sch, tbl.relname AS tbl, att.attname AS col,
           pg_get_serial_sequence(quote_ident(ns.nspname) || '.' || quote_ident(tbl.relname), att.attname) AS seq
    FROM pg_class tbl
    JOIN pg_namespace ns ON ns.oid = tbl.relnamespace
    JOIN pg_attribute att ON att.attrelid = tbl.oid AND att.attnum > 0 AND NOT att.attisdropped
    WHERE ns.nspname = 'atlas_driver_offer_bpp' AND tbl.relkind = 'r'
      AND pg_get_serial_sequence(quote_ident(ns.nspname) || '.' || quote_ident(tbl.relname), att.attname) IS NOT NULL
  LOOP
    EXECUTE format('SELECT setval(%L, COALESCE((SELECT MAX(%I)::bigint FROM %I.%I), 0) + 1, false)', r.seq, r.col, r.sch, r.tbl);
    fixed := fixed + 1;
  END LOOP;
  RAISE NOTICE 'resynced % sequences', fixed;
END $$;
