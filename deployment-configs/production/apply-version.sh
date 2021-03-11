#!/usr/bin/bash
if [[ -z $1 || -z $2 ]]; then
    echo "usage: $0 <image-version> <deployment-file>";
fi

sed "s/\$BUILD_VERSION/$1/g" $2 