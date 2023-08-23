-- UPDATE atlas_driver_offer_bpp.driver_plan as dp
-- SET mandate_setup_date = (SELECT mandate_setup_date - interval '330 minutes'
-- FROM atlas_driver_offer_bpp.driver_plan as dplan
-- WHERE dplan.driver_id = dp.driver_id AND not null mandate_setup_date  AND dplan.plan_type = 'AUTOPAY' )

UPDATE atlas_driver_offer_bpp.driver_plan
SET mandate_setup_date = mandate_setup_date - interval '330 minutes' where not null mandate_setup_date AND plan_type = 'AUTOPAY'