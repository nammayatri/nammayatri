-- R4: Resolution fields
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN resolution_short_desc text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN resolution_long_desc text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN resolution_action_triggered text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN resolution_refund_amount text;
-- R1: Description
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN description_short text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN description_long text;
-- R1: Order details sub-fields
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN order_state text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN order_provider_id text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN order_merchant_order_id text;
-- R1: Order item (single, mobility)
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN order_item_id text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN order_item_quantity double precision;
-- R1: Order fulfillment (single, mobility)
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN order_fulfillment_id text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN order_fulfillment_state text;
-- R1: Category/sub-category ONDC enum text
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN igm_category text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN igm_sub_category text;
-- R1: Source
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN source_type text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN source_np_id text;
-- R1: Context preservation
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN context_transaction_id text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN context_domain text;
-- R1: Expected times
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN expected_response_time text;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN expected_resolution_time text;
-- R6: Rating on close
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN issue_rating text;

-- Action history table (complainant + respondent actions)
CREATE TABLE atlas_driver_offer_bpp.igm_issue_action (
    id character varying(36) NOT NULL PRIMARY KEY,
    igm_issue_id character varying(36) NOT NULL,
    action_type text NOT NULL,
    action text NOT NULL,
    short_desc text,
    updated_at timestamp with time zone NOT NULL,
    updated_by_org_name text,
    updated_by_contact_phone text,
    updated_by_contact_email text,
    updated_by_person_name text,
    cascaded_level int,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
