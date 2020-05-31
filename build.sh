#!/usr/bin/env bash

echo "cleaning up..."
rm -rf resources/public/js/main.js
rm -rf resources/public/js/out
rm -rf .cpcache

echo "compiling clojurescript..."
clj -A:prod
rm -rf resources/public/js/out

echo "compiling sass..."
sass sass/main.sass:resources/public/css/main.css
echo "running autoprefixer..."
postcss resources/public/css/main.css -u autoprefixer -o resources/public/css/prefixed-main.css

echo "making release..."
rm -rf release
cp -R resources/public release/
