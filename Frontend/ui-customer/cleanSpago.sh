#!/bin/bash
echo " ---------- cleaning project --------------"
rm -rf node_modules .pulp-cache output .spago
echo " ---------- cleaning done --------------"

echo " ---------- started installing --------------"
npm install
npm run fast:start
echo " ---------- cleaning done --------------"