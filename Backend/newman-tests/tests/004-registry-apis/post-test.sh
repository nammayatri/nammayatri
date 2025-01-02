set -e

test_subscriber=$(psql -h localhost -p 5434 -U atlas_registry_user atlas_dev -c "select count(*) from atlas_registry.subscriber where unique_key_id = '69' and subscriber_id = 'abcd.test.subscriber';" | awk 'NR==3 {print $1}')
NC='\033[0m'
RED='\033[0;31m'

if [ $test_subscriber -gt 0 ]; then
  echo -e "${RED}Error: Test subscriber not removed${NC}"
  psql -h localhost -p 5434 -U atlas_registry_user atlas_dev -c "select subscriber_id, unique_key_id, type, domain from atlas_registry.subscriber where unique_key_id = '69' and subscriber_id = 'abcd.test.subscriber';"
  exit 1
fi