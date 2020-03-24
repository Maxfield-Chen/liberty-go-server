#!/bin/bash
curl -X POST -d '{"userEmail": "max@gmail.com","userName": "maxfield","userPassword": "testtest"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:9999/users/register/
