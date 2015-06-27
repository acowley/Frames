#!/bin/sh
# Based on https://gist.github.com/Fuuzetsu/8276421
# Usage: sh hackagedocs.sh 0.7.1 UserName Password
PKG_NAME=Frames

cabal configure && cabal build && cabal haddock --hyperlink-source --html-location='http://hackage.haskell.org/package/$pkg/docs' \
                                    --contents-location='http://hackage.haskell.org/package/$pkg'
S=$?
if [ "${S}" -eq "0" ]; then
    cd "dist/doc/html"
    DDIR="${PKG_NAME}-${1}-docs"
    cp -r "${PKG_NAME}" "${DDIR}" && tar -c -v -z --format ustar -f "${DDIR}.tar.gz" "${DDIR}"
    CS=$?
    if [ "${CS}" -eq "0" ]; then
        echo "Uploading to Hackage…"
        curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@${DDIR}.tar.gz" "http://${2}:${3}@hackage.haskell.org/package/${PKG_NAME}-${1}/docs"
        exit $?
    else
        echo "Error when packaging the documentation"
        exit $CS
    fi
else
    echo "Error when trying to build the package."
    exit $S
fi

