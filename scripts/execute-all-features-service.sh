#!/bin/bash

sh execute.sh ./out/response-equality.edn localhost 50100
sh execute.sh ./out/response-inequality.edn localhost 50100
sh execute.sh ./out/state-mutation.edn localhost 50100
sh execute.sh ./out/state-identity.edn localhost 50100
sh execute.sh ./out/fuzz.edn localhost 50100
