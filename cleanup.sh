#!/usr/bin/env bash

find . -name "*~" -delete
rm -f resources/public/css/main.css
rm -f resources/public/css/main.css.map
rm -f resources/public/js/main.js
rm -f resources/public/js/main-devcards.js
rm -rf resources/public/js/out
rm -rf .cpcache
rm -rf .sass-cache
