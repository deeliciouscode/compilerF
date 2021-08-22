#!/bin/bash

if [ -d "build/intermediaries" ]; then
    echo ""
else
    mkdir -p "build/intermediaries";
    echo "/build/intermediaries was created"
fi

ghc --make -o ./build/Interpreter.bry -outputdir ./build/intermediaries Main.hs
echo $1 | ./build/Interpreter.bry
