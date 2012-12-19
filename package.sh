#!/bin/sh

./compiler/pbjc -c $1 > backend/src/plt/pbj/PBJRunner.java

cd backend # into backend
./build.sh # compile backend.

cp -r bin/* ../distro

cd .. # back to root

cd distro
jar cmvf MANIFEST.MF PBJ.jar com plt

mv PBJ.jar ../



