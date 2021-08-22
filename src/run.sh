#!/bin/sh

ghc --make -o ./build/Interpreter -outputdir ./build/intermediaries Main.hs
echo $1 | ./build/Interpreter
