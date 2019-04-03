#!/bin/bash
cat metadata.json | jq 'select( .filename | contains("article") ) | .type' | jq '[inputs] | del(.[] | nulls) | unique'

cat metadata.json | jq '."permission-file"' | jq '[inputs] | unique | del(.[] | nulls)' | jq '[.[] | if length > 0 then . else empty end]'

cat metadata.json | jq '.authors' | jq '[inputs] | add | unique'
