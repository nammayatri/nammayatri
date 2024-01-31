
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.refunds
(
    id VARCHAR(36) PRIMARY KEY,
    short_id text NOT NULL,
    order_id text NOT NULL,
    refund_amount double precision NOT NULL,
    id_assigned_by_service_provider text,
    merchant_id text NOT NULL,
    status text NOT NULL,
    error_message text,
    error_code text,
    initiated_by text,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,

);
