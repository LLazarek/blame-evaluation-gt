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
VERSION="8.6"
if [ -d "./racket" ]; then
    echo "$(pwd)/racket already exists; skipping installing racket"
else
    INSTALLER="racket-${VERSION}-x86_64-linux-cs.sh"
    wget "https://mirror.racket-lang.org/installers/${VERSION}/$INSTALLER"
    chmod u+x ./$INSTALLER
    ./$INSTALLER <<EOF
no
4

EOF
fi

./racket/bin/raco pkg install --auto blame-evaluation-gt/bex/

printf "Running setup script\n\n\n"
./racket/bin/racket -l bex/util/setup

popd
