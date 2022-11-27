-- CONN_STRING="host=localhost port=5434 user=postgres dbname=atlas_dev password=root"
--  psql "$CONN_STRING" -f $MIGDIR/$migfile

truncate table atlas_driver_offer_bpp.booking cascade;
truncate table atlas_driver_offer_bpp.driver_quote;
truncate table atlas_driver_offer_bpp.search_request cascade;
