echo " ---------- cleaning project --------------"
rm -rf node_modules .pulp-cache output .spago bower_components
echo " ---------- cleaning done --------------"

echo " ---------- started installing --------------"
npm install
bower install
npm run start
echo " ---------- cleaning done --------------"