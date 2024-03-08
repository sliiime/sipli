#!/bin/bash

if [ ! -d ./build]; then 
  mkdir ./build
fi

ghc --make -o ./build/sipli sipli.hs -odir build -hidir build
