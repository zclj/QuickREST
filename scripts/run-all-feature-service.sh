#!/bin/bash

sh run.sh http://localhost:50100/swagger.json localhost 50100 response-equality
sh run.sh http://localhost:50100/swagger.json localhost 50100 response-inequality
sh run.sh http://localhost:50100/swagger.json localhost 50100 state-mutation
sh run.sh http://localhost:50100/swagger.json localhost 50100 state-identity
sh run.sh http://localhost:50100/swagger.json localhost 50100 fuzz
