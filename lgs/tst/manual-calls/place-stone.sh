#!/bin/bash
curl -X PUT -d '[1, 1]' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:9999/play/1/placeStone
