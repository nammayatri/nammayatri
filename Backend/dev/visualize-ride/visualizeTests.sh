#!/bin/bash
./visualize-ride.sh shortCurvyRoutesnap /tmp/default short-snap.gpx gpx
./visualize-ride.sh shortCurvyRouteosrm /tmp/default short-osrm.gpx gpx
./visualize-ride.sh farIsolatedPointsnap /tmp/default far-snap.gpx gpx
./visualize-ride.sh farIsolatedPointosrm /tmp/default far-osrm.gpx gpx

./visualize-ride.sh shortCurvyRoutesnap /tmp/default short-snap.csv csv
./visualize-ride.sh shortCurvyRouteosrm /tmp/default short-osrm.csv csv
./visualize-ride.sh farIsolatedPointsnap /tmp/default far-snap.csv csv
./visualize-ride.sh farIsolatedPointosrm /tmp/default far-osrm.csv csv