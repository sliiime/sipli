#!/bin/bash

DIR="./Seiras_Kostis_1115201800174"
ZIP="./$DIR"".zip"

if [ ! -d "$DIR" ]; then
  mkdir "$DIR"
fi

if [ -f "$ZIP" ]; then
  rm -f "$ZIP" 
fi

cp -r "src" "build.sh" "test" "README.txt" "$DIR"
zip -r "$DIR"".zip" "$DIR"
rm -rf "$DIR"
