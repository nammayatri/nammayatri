# Check for anomalies

sleep 0.2 # Waiting for drainer to drain to DB
script_dir=$(dirname "$0")
$script_dir/../../utils/checkStuckEntities.sh
$script_dir/../../utils/clearBlockedDriverListForRiders.sh