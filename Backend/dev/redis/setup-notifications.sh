#!/bin/bash

# setup fcm tokens for all servers
cat ./dev/redis/test-notifications.redis | redis-cli
