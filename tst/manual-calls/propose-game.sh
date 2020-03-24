#!/bin/bash
curl -X POST -d '[1,2]' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:9999/play/proposeGame
