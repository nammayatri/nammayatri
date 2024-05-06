set -e

# Driver app
dobpp_stuck_booking=$(psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select count(*) from atlas_driver_offer_bpp.booking where status not in ('CANCELLED', 'COMPLETED');" | awk 'NR==3 {print $1}')
dobpp_stuck_ride=$(psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select count(*) from atlas_driver_offer_bpp.ride where status not in ('CANCELLED', 'COMPLETED');" | awk 'NR==3 {print $1}')
dobpp_stuck_driver=$(psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select count(*) from atlas_driver_offer_bpp.driver_information where on_ride = true;" | awk 'NR==3 {print $1}')

if [ $dobpp_stuck_booking -gt 0 ]; then
  echo "Error: Driver app stuck booking"
  psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select id, status from atlas_driver_offer_bpp.booking where status not in ('CANCELLED', 'COMPLETED');"
fi

if [ $dobpp_stuck_ride -gt 0 ]; then
  echo "Error: Driver app stuck ride"
  psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select id, status from atlas_driver_offer_bpp.ride where status not in ('CANCELLED', 'COMPLETED');"
fi

if [ $dobpp_stuck_driver -gt 0 ]; then
  echo "Error: Driver app stuck driver"
  psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user atlas_dev -c "select driver_id, on_ride from atlas_driver_offer_bpp.driver_information where on_ride = true;"
fi

# Customer app
# wait for customer app to finish, so added sleep for sync
sleep 1

app_stuck_booking=$(psql -h localhost -p 5434 -U atlas_app_user atlas_dev -c "select count(*) from atlas_app.booking where status not in ('CANCELLED', 'COMPLETED');" | awk 'NR==3 {print $1}')
app_stuck_ride=$(psql -h localhost -p 5434 -U atlas_app_user atlas_dev -c "select count(*) from atlas_app.ride where status not in ('CANCELLED', 'COMPLETED');" | awk 'NR==3 {print $1}')

if [ $app_stuck_booking -gt 0 ]; then
  echo "Error: Customer app stuck booking"
  psql -h localhost -p 5434 -U atlas_app_user atlas_dev -c "select id, status from atlas_app.booking where status not in ('CANCELLED', 'COMPLETED');"
fi

if [ $app_stuck_ride -gt 0 ]; then
  echo "Error: Customer app stuck ride"
  psql -h localhost -p 5434 -U atlas_app_user atlas_dev -c "select id, status from atlas_app.ride where status not in ('CANCELLED', 'COMPLETED');"
fi

if [ $dobpp_stuck_booking -gt 0 -o $dobpp_stuck_ride -gt 0 -o $dobpp_stuck_driver -gt 0 -o $app_stuck_booking -gt 0 -o $app_stuck_ride -gt 0 ]; then
  exit 1
fi