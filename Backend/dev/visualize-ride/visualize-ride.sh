#!/bin/bash
RIDE_ID=$1
INPUT_FILE=$2
OUTPUT_FILE=$3
FILE_TYPE=$4
TEMP_FILE=temp

# usage:
# 1) obtain file that contains logs of location updates in the accepted format (see the code below)
# 2) define the ride id you are interested in
# 3) ./visualize-ride.sh <rideId> <input log file> <output gpx file (optional)>
# 4) use the gpx visualizer (one of the good ones is https://www.gpsvisualizer.com/)
# 5) there will be two tracks: the first one containing points that were accepted from the driver, tagged "before",
#    and the second one containing interpolated points, tagged "after" (we use snap-to-road Google API for this).
#    Also the points that were first in the batches for the calls to snap-to-road are highlighted,
#    and you can see how separate segments were interpolated.
#
#    hint: xmllint is the part of libxml2-utils package

if [ "o$OUTPUT_FILE" = "o" ] ;
  then OUTPUT_FILE=output.gpx
fi
grep "locupd-rideId-$RIDE_ID" $INPUT_FILE | grep 'points interp' | sed 's/.*points interpolation: input=\(\[.*\]\); output=\(\[.*\]\)/(\1,\2)/' | stack exec -- route-extractor-exe $FILE_TYPE > $TEMP_FILE
if [[ $OUTPUT_FILE == *.gpx ]] ;
  then xmllint --format $TEMP_FILE > $OUTPUT_FILE
  else cat $TEMP_FILE > $OUTPUT_FILE
fi

rm $TEMP_FILE

