#!/bin/bash

apps () {
cat << EOF
app-backend
beckn-gateway
beckn-transport-btm
beckn-transport
mock-sms
mock-registry
EOF
}

map () { while read -r line; do $1 "$line"; done; }

to_log_filename () { echo "$1.log"; }

to_run_cmd () { echo "stack exec $1-exe | tee $(to_log_filename $1)"; }

rm -f $(apps | map to_log_filename)
apps | map to_run_cmd | parallel
