#!/bin/bash

# /root/.local/bin/main "$@" || echo "run error code: $?"

SERVER_URL=$1
PLAYER_KEY=$2
API_KEY=46a9ed49d3414202898c86d9297976d9

echo "ServerUrl: $SERVER_URL; PlayerKey: $PLAYER_KEY"

RESPONSE_FILE=$( mktemp -t tmp.XXXXXXXXXX )
HTTP_CODE=$( curl -s -o "$RESPONSE_FILE" -w '%{http_code}' -X POST "$SERVER_URL/aliens/send?apiKey=$API_KEY" -H "accept: */*" -H "Content-Type: text/plain" -d "1101000"  )
EXIT_CODE=$?

if [[ ${EXIT_CODE} -ne 0 ]] ; then
    echo "run error code: $EXIT_CODE"
    exit 0
fi

if [[ ${HTTP_CODE} -ne 200 ]] ; then
    echo "Unexpected server response:"
    echo "HTTP code: $HTTP_CODE"
    echo "Response body: $( cat "$RESPONSE_FILE" )"
    exit 0
fi

echo "Server response: $( cat "$RESPONSE_FILE" )"
