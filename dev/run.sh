#!/bin/bash
apps () {
cat << EOF
allocation-service
rider-app
beckn-gateway
static-offer-driver-app
driver-tracking-healthcheck
mock-fcm
mock-registry
mock-sms
mock-idfy
public-transport-rider-platform
public-transport-search-consumer
search-result-aggregator
transporter-scheduler
scheduler-example-app
scheduler-example-scheduler
dynamic-offer-driver-app
rider-dashboard
provider-dashboard
image-api-helper
driver-offer-allocator
kafka-consumers
EOF
}
export CONSUMER_TYPE="AVAILABILITY_TIME"
allApps=`apps`

map () { while read -r line; do $1 "$line"; done; }

to_log_filename () { echo "$1.log"; }

to_run_cmd () { echo "stack exec $1-exe 2>&1 | tee $(to_log_filename $1)"; }

rm -f $(apps | map to_log_filename)
apps | map to_run_cmd | parallel -j $(apps | wc -l)
