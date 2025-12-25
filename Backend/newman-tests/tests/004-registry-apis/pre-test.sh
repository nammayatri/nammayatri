#Registry
echo "Removing test subscriber"
psql -h localhost -p 5434 -U atlas_registry_user atlas_dev -c "delete from atlas_registry.subscriber where unique_key_id = '69' and subscriber_id = 'abcd.test.subscriber';"
