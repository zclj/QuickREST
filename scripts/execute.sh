#!/bin/bash

file=$1
host=$2
port=$3

java -jar quick-rest.jar test -p $port -H $host --file $file
