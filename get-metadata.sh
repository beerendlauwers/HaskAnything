#!/bin/bash

echo "\$meta-json\$" > /tmp/metadata.pandoc-tpl
rm metadata.json
touch metadata.json
find ./app/content -name "*.markdown" -o -name "*.md" | parallel --max-args 1 'pandoc --template=/tmp/metadata.pandoc-tpl -M filename={} {} >> metadata.json'
