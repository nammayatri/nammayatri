-- Additional Info Request table for operator request-additional-info workflow
CREATE TABLE atlas_driver_offer_bpp.additional_info_request (
    id                          CHARACTER(36) PRIMARY KEY,
    operation_hub_request_id    CHARACTER(36) NOT NULL REFERENCES atlas_driver_offer_bpp.operation_hub_requests(id),
    requested_by                CHARACTER(36) NOT NULL,
    requested_from              CHARACTER(36) NOT NULL,
    requested_document_types    TEXT[] NOT NULL,
    message                     TEXT NOT NULL,
    status                      CHARACTER VARYING(255) NOT NULL DEFAULT 'PENDING',
    response_remarks            TEXT,
    response_document_ids       TEXT[],
    merchant_id                 CHARACTER(36) NOT NULL,
    merchant_operating_city_id  CHARACTER(36) NOT NULL,
    created_at                  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at                  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

CREATE INDEX idx_additional_info_request_hub_req ON atlas_driver_offer_bpp.additional_info_request(operation_hub_request_id);
CREATE INDEX idx_additional_info_request_status ON atlas_driver_offer_bpp.additional_info_request(status);

-- Note: operation_hub_requests.request_status is VARCHAR(255), so AWAITING_INFO is already accepted.
-- No ALTER needed for the status column.
