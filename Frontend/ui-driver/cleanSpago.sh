#!/bin/bash
echo " ---------- cleaning project --------------"
rm -rf node_modules .pulp-cache output .spago
echo " ---------- cleaning done --------------"

echo " ---------- started installing --------------"
npm install
npm run start:android:master
echo " ---------- cleaning done --------------"