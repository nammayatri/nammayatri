-- Adds isWhatsappRequest to search_request and booking, mirroring the existing
-- isDashboardRequest column on both tables. Set to TRUE by the WhatsApp bot
-- (WhatsappBot/Adapter/Backend.hs's runSearch) at search time, copied onto
-- booking at confirm time (SharedLogic/Confirm.hs), so WhatsApp-originated
-- traffic can be measured directly from these two tables.
ALTER TABLE atlas_app.search_request ADD COLUMN IF NOT EXISTS is_whatsapp_request boolean;
ALTER TABLE atlas_app.booking ADD COLUMN IF NOT EXISTS is_whatsapp_request boolean;
