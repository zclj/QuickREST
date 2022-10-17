#!/bin/bash

url=$1
host=$2
port=$3
property=$4

java -jar quick-rest.jar start -p $port -H $host --url $url -b $property -t 100
