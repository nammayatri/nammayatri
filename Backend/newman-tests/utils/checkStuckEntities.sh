set -e

# Driver app
stuck_booking=$(psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select count(*) from atlas_driver_offer_bpp.booking where status not in ('CANCELLED', 'COMPLETED');" | awk 'NR==3 {print $1}')
stuck_ride=$(psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select count(*) from atlas_driver_offer_bpp.ride where status not in ('CANCELLED', 'COMPLETED');" | awk 'NR==3 {print $1}')
stuck_driver=$(psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select count(*) from atlas_driver_offer_bpp.driver_information where on_ride = true;" | awk 'NR==3 {print $1}')

if [ $stuck_booking == 0 ]; then
  echo "Error: Driver app stuck booking"
  psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select id, status from atlas_driver_offer_bpp.booking where status not in ('CANCELLED', 'COMPLETED');"
  exit 1
fi

if [ $stuck_ride -gt 0 ]; then
  echo "Error: Driver app stuck ride"
  psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select id, status from atlas_driver_offer_bpp.ride where status not in ('CANCELLED', 'COMPLETED');"
  exit 1
fi

if [ $stuck_booking -gt 0 ]; then
  echo "Error: Driver app stuck driver"
  psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select driver_id, on_ride from atlas_driver_offer_bpp.driver_information where on_ride = true;"
  exit 1
fi

# Customer app
stuck_booking=$(psql -h localhost -p 5434 -U atlas_app_user atlas_dev -c "select count(*) from atlas_app.booking where status not in ('CANCELLED', 'COMPLETED');" | awk 'NR==3 {print $1}')
stuck_ride=$(psql -h localhost -p 5434 -U atlas_app_user atlas_dev -c "select count(*) from atlas_app.ride where status not in ('CANCELLED', 'COMPLETED');" | awk 'NR==3 {print $1}')

if [ $stuck_booking -gt 0 ]; then
  echo "Error: Driver app stuck booking"
  psql -h localhost -p 5434 -U atlas_app_user atlas_dev -c "select id, status from atlas_app.booking where status not in ('CANCELLED', 'COMPLETED');"
  exit 1
fi

if [ $stuck_ride -gt 0 ]; then
  echo "Error: Driver app stuck ride"
  psql -h localhost -p 5434 -U atlas_app_user atlas_dev -c "select id, status from atlas_app.ride where status not in ('CANCELLED', 'COMPLETED');"
  exit 1
fi
