#!/bin/bash
rm -f templates.js && for f in *.dust; do dustc -n=$(echo $f | rev | cut -c 6- | rev) $f>>templates.js; done;
