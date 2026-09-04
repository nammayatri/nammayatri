-- Marks a payment_order whose row we created without opening a session on the gateway, because
-- another system opens the gateway order under the same order short id. Such a row has no client
-- auth token, no SDK payload and no gateway order id of ours, so anything that would hand the rider
-- an SDK payload, expire the auth token, or update the order on the gateway must skip it. The
-- webhook and orderStatus paths are unaffected: they reconcile purely on the short id.
--
-- NULL is a normal, gateway-created order, which is what every existing row is.
--
-- Apply this in every environment BEFORE deploying the code that writes it.
ALTER TABLE atlas_app.payment_order ADD COLUMN IF NOT EXISTS is_external_order boolean;
