#!/bin/sh
sed -re 's#rtsConfig\.js#./codeJamRtsConfig#' < rts.js > myrts.js
/home/janus/lamdu/.stack-work/install/x86_64-linux/lts-7.9/8.0.1/bin/janus | python3 -c 'import sys, json; print(json.loads(sys.stdin.read()))' | sed -re 's#rts\.js#./myrts#' > myprog.js; node -p 'require("./myprog")'
