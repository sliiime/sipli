#!/bin/bash

if [ ! -d "./build" ]; then 
  mkdir "./build"
fi

ghc -isrc --make -o ./sipli ./src/sipli.hs -odir build -hidir build
