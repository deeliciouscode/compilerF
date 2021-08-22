#!/bin/sh

ghc --make -o ./build/Interpreter.bry -outputdir ./build/intermediaries Main.hs
echo $1 | ./build/Interpreter.bry
