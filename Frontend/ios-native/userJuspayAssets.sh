#!/bin/bash
echo " ---------- Start assests Customer :- --------------"
echo "{\"images\":{" > assests.json 
find ./Namma\ Yatri/Assets/Images | grep ".png" | cut -d "/" -f 5 | sed 's/.png//' | awk '{print "\"" $1 "\" : true," }' >> assests.json
find ./Namma\ Yatri/Assets/Images | grep ".xml" | cut -d "/" -f 5 | sed 's/.png//' | awk '{print "\"" $1 "\" : true," }' >> assests.json
sed '$ s/.$//' assests.json > juspay_assets.json
echo "}}" >> juspay_assets.json
cat juspay_assets.json | json_pp | tee ./Namma\ Yatri/Assets/juspay_assets.json
rm juspay_assets.json assests.json
echo " ---------- End assests Customer :- --------------"