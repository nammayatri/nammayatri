# Check for stuck entities

script_dir=$(dirname "$0")
$script_dir/../../utils/checkStuckEntities.sh
$script_dir/../../utils/clearBlockedDriverListForRiders.sh