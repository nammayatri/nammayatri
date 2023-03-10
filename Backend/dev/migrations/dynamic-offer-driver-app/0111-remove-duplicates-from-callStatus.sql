DELETE FROM
 atlas_driver_offer_bpp.call_status dup_sid
USING  atlas_driver_offer_bpp.call_status dist_sid
WHERE dup_sid.created_at > dist_sid.created_at
AND dup_sid.exotel_call_sid = dist_sid.exotel_call_sid;

ALTER TABLE atlas_driver_offer_bpp.call_status ADD CONSTRAINT unique_call_sid UNIQUE (exotel_call_sid);
