#!/bin/bash
curl -X POST -d '{"_pg_black_player": 0, "_pg_white_player": 0, "_pg_black_focus": "influence", "_pg_white_focus": "territory"}' -H 'Accept: application/json' -H 'Content-type: application/json' --cookie "JWT-Cookie=$1;XSRF-TOKEN=$2;" -H "X-XSRF-TOKEN: $2"  http://localhost:8888/play/proposeGame -vvv
