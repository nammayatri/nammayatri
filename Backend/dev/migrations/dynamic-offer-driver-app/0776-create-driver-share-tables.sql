-- Social Media Integration: share event tracking and template management

CREATE TABLE atlas_driver_offer_bpp.driver_share_event (
    id                         CHARACTER VARYING(36) PRIMARY KEY,
    driver_id                  CHARACTER VARYING(36) NOT NULL,
    share_type                 CHARACTER VARYING(30) NOT NULL,
    share_platform             CHARACTER VARYING(30),
    content_id                 CHARACTER VARYING(36),
    share_card_url             TEXT,
    deep_link_url              TEXT,
    merchant_operating_city_id CHARACTER VARYING(36) NOT NULL,
    merchant_id                CHARACTER VARYING(36) NOT NULL,
    created_at                 TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_driver_share_event_driver ON atlas_driver_offer_bpp.driver_share_event(driver_id, created_at);
CREATE INDEX idx_driver_share_event_type ON atlas_driver_offer_bpp.driver_share_event(share_type, created_at);
CREATE INDEX idx_driver_share_event_merchant ON atlas_driver_offer_bpp.driver_share_event(merchant_operating_city_id);

CREATE TABLE atlas_driver_offer_bpp.share_template (
    id                         CHARACTER VARYING(36) PRIMARY KEY,
    template_type              CHARACTER VARYING(30) NOT NULL,
    name                       TEXT NOT NULL,
    background_image_url       TEXT NOT NULL,
    text_overlay_config        TEXT NOT NULL,
    is_active                  BOOLEAN NOT NULL DEFAULT TRUE,
    merchant_id                CHARACTER VARYING(36) NOT NULL,
    merchant_operating_city_id CHARACTER VARYING(36),
    created_at                 TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at                 TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_share_template_type_merchant ON atlas_driver_offer_bpp.share_template(template_type, merchant_id, is_active);
