-- No WhatsApp for FRFS bus/driver reassignment (push notification already covers it).
-- sendWhatsAppTemplateIfOptedIn no-ops when the row is missing, so deleting it is sufficient.
-- Row seeded in 0046-frfs-trip-details-updated-notification.sql. Idempotent.

DELETE FROM atlas_app.merchant_message
WHERE message_key = 'WHATSAPP_FRFS_TRIP_DETAILS_UPDATED';
