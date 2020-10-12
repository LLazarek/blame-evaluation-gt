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

printf "Installing Racket if necessary\n\n\n"
if [ -d "./racket" ]; then
    echo "$(pwd)/racket already exists; skipping installing racket"
else
    INSTALLER="racket-7.7-x86_64-linux-cs.sh"
    wget "https://mirror.racket-lang.org/installers/7.7/$INSTALLER"
    chmod u+x ./$INSTALLER
    ./$INSTALLER <<EOF
no
4

EOF
fi

printf "Installing setup script dependencies if necessary\n\n\n"
if [ ! -d "./ruinit" ]; then
    git clone https://github.com/LLazarek/ruinit.git
fi
if [ ! -d "./rscript" ]; then
    git clone https://github.com/LLazarek/rscript.git
fi
# Can't do them at the same time, it breaks?
./racket/bin/raco pkg install --skip-installed -D ./ruinit
./racket/bin/raco pkg install --skip-installed -D ./rscript

printf "Running setup script\n\n\n"
./racket/bin/racket blame-evaluation-gt/util/setup.rkt

popd
