#!/bin/bash

case "$1" in
    ""|"-h"|"--help"|"h"|"help" )
	echo "First argument must be project root"
	exit 1
	;;
    * )
	;;
esac

pushd "$1"

INSTALLER="racket-7.7-x86_64-linux-cs.sh"
wget "https://mirror.racket-lang.org/installers/7.7/$INSTALLER"
chmod u+x ./$INSTALLER
./$INSTALLER

git clone https://github.com/LLazarek/rscript.git
./racket/bin/raco pkg install ./rscript

./racket/bin/racket blame-evaluation-gt/util/setup.rkt

popd
