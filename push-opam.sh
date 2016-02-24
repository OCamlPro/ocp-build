#!/bin/sh

OPAMREPO=$HOME/WindowsSharedFolder/opam-repository-with-archives
. ./autoconf/Makefile.config
echo VERSION=$VERSION

(cd $OPAMREPO && git checkout master && git pull ocaml master)


if [ -e ${OPAMREPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION} ]; then
    echo "Error: directory for ${VERSION} already exists"
    echo ${OPAMREPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}
    exit 2
fi

mkdir $OPAMREPO/packages/${PACKAGE}/${PACKAGE}.$VERSION || exit 2
cp opam descr findlib $OPAMREPO/packages/${PACKAGE}/${PACKAGE}.$VERSION/ || exit 2

URL=http://github.com/OCamlPro/${PACKAGE}/archive/${VERSION}.tar.gz
rm -f ${VERSION}.tar.gz
wget ${URL} || exit 2
MD5SUM=$(md5sum ${VERSION}.tar.gz | cut -b 1-32 ) 
echo 'archive: "'${URL}'"' > $OPAMREPO/packages/${PACKAGE}/${PACKAGE}.$VERSION/url
echo 'checksum: "'${MD5SUM}'"' >> $OPAMREPO/packages/${PACKAGE}/${PACKAGE}.$VERSION/url

echo Generated url file:
cat $OPAMREPO/packages/${PACKAGE}/${PACKAGE}.$VERSION/url

(cd ${OPAMREPO} &&
        (git branch -D ${PACKAGE}.${VERSION} || true) &&
        git checkout -b ${PACKAGE}.${VERSION} &&
        git add packages/${PACKAGE}/${PACKAGE}.${VERSION} &&
        git commit -m "Add ${PACKAGE}.${VERSION}" packages/${PACKAGE}/${PACKAGE}.${VERSION} &&
        git push -f origin ${PACKAGE}.${VERSION})
