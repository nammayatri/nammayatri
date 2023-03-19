#!/bin/bash
echo " ---------- Start assests Driver :- --------------"
echo "{\"images\":{" > assests.json 
find android-native/app/src/driver/res/drawable | grep ".png" | cut -d "/" -f 7 | sed 's/.png//' | awk '{print "\"" $1 "\" : true," }' >> assests.json
find android-native/app/src/main/res/drawable | grep ".png" | cut -d "/" -f 7 | sed 's/.png//' | awk '{print "\"" $1 "\" : true," }' >> assests.json
find android-native/app/src/driver/res/drawable | grep ".xml" | cut -d "/" -f 7 | sed 's/.xml//' | awk '{print "\"" $1 "\" : true," }' >> assests.json
find android-native/app/src/main/res/drawable | grep ".xml" | cut -d "/" -f 7 | sed 's/.xml//' | awk '{print "\"" $1 "\" : true," }' >> assests.json
sed '$ s/.$//' assests.json > juspay_assets.json
echo "},\"fonts\":{" >> juspay_assets.json
find android-native/app/src/main/assets/fonts | grep ".ttf" | cut -d "/" -f 7 | sed 's/.ttf//' | awk '{print "\"" $1 "\" : true," }' >> juspay_assets.json
sed '$ s/.$//' juspay_assets.json > juspay_assets_fonts.json
echo "}}" >> juspay_assets_fonts.json
cat juspay_assets_fonts.json | json_pp |  tee ./android-native/app/src/driver/assets/juspay/juspay_assets.json

rm juspay_assets_fonts.json juspay_assets.json assests.json

echo " ---------- End assests Driver :- --------------"