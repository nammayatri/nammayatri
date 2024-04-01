# Driver app
psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "update atlas_driver_offer_bpp.booking set status = 'CANCELLED' where status not in ('CANCELLED', 'COMPLETED');"
psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "update atlas_driver_offer_bpp.ride set status = 'CANCELLED' where status not in ('CANCELLED', 'COMPLETED');"

psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "update atlas_driver_offer_bpp.driver_information set on_ride = false where on_ride = true;"

# Customer app
psql -h localhost -p 5434 -U atlas_app_user atlas_dev -c "update atlas_app.booking set status = 'CANCELLED' where status not in ('CANCELLED', 'COMPLETED');"
psql -h localhost -p 5434 -U atlas_app_user atlas_dev -c "update atlas_app.ride set status = 'CANCELLED' where status not in ('CANCELLED', 'COMPLETED');"