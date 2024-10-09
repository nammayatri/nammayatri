#!/bin/bash
set +e

NC='\033[0m';
YELLOW='\033[1;33m';

key_pattern="*Block-Listed-Drivers-Key:RiderId-*"
IFS=$'\n'

echo -e "\n${YELLOW}Clearing block listed drivers for rider key, pattern:\"$key_pattern\"${NC}\n";

echo -e "\nChecking in Cluster redis...\n";

# Get the keys from cluster redis
read -d '' -ra cluster_result <<< "$(redis-cli --cluster call --cluster-only-masters localhost:30001 keys "$key_pattern")"
count=0
for key in "${cluster_result[@]}"; do
    if [[ $count -eq 0 ]]; then
        count+=1
        continue
    fi
    suffix=$(echo "$key" | cut -d ' ' -f 2-);
    # echo "Key... :>$suffix";
    if [[ -n "$suffix" ]]; then
        echo -e "${YELLOW}Deleting key:\"$key\"${NC}";
        redis-cli --cluster call --cluster-only-masters localhost:30001 del "$key";
    fi
    count+=1
done

echo -e "\nChecking in Standalone redis...\n";

# Get the keys from standalone redis
read -d '' -ra standalone_result <<< "$(redis-cli -h localhost -p 6379 keys "$key_pattern")"
for key in "${standalone_result[@]}"; do
    suffix=$(echo "$key" | cut -d ' ' -f 2-);
    suffix="${suffix%\"}"
    suffix="${suffix#\"}"
    # echo "Key... :>$suffix";
    if [[ "$suffix" != "(empty array)" ]]; then
        echo -e "${YELLOW}Deleting key:\"$key\"${NC}";
        redis-cli -h localhost -p 6379 del "$key";
    fi
done
