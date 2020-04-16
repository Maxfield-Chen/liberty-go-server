#!/bin/bash
curl -X POST -d '{"userEmail": "max@gmail.com","userName": "max","userPassword": "test"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8888/users/register/ -vvv
