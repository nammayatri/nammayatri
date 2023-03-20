#!/bin/bash
echo " ---------- Start assests Driver :- --------------"
echo "{\"images\":{" > assests.json
find app/src/driver/$1/res/drawable | grep ".png" | cut -d "/" -f 7 | sed 's/.png//' | awk '{print "\"" $1 "\" : true," }' >> assests.json
find app/src/main/res/drawable | grep ".png" | cut -d "/" -f 6 | sed 's/.png//' | awk '{print "\"" $1 "\" : true," }' >> assests.json
find app/src/driver/$1/res/drawable | grep ".xml" | cut -d "/" -f 7 | sed 's/.xml//' | awk '{print "\"" $1 "\" : true," }' >> assests.json
find app/src/main/res/drawable | grep ".xml" | cut -d "/" -f 6 | sed 's/.xml//' | awk '{print "\"" $1 "\" : true," }' >> assests.json
sed '$ s/.$//' assests.json > juspay_assets.json
echo "}}" >> juspay_assets.json
cat juspay_assets.json | json_pp | tee ./app/src/driver/$1/assets/juspay/juspay_assets.json

rm juspay_assets.json assests.json

echo " ---------- End assests Driver :- --------------"