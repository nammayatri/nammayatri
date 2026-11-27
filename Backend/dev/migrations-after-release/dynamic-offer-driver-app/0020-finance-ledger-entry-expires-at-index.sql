-- Expiring lots (Lib.Finance.Lot) read an account's credits by expiry. expires_at is a nullable column
-- (generated into migrations-read-only with the rest of the table); only lot entries set it.
-- Built CONCURRENTLY: finance_ledger_entry is a large, write-heavy table. If the build fails it leaves an
-- INVALID index that IF NOT EXISTS keeps: DROP INDEX CONCURRENTLY IF EXISTS finance_ledger_entry_to_account_expires_at; re-run.
CREATE INDEX CONCURRENTLY IF NOT EXISTS finance_ledger_entry_to_account_expires_at
  ON atlas_driver_offer_bpp.finance_ledger_entry USING btree (to_account_id, expires_at);
