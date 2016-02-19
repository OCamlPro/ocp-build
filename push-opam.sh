#!/bin/sh

OPAMREPO=$HOME/WindowsSharedFolder/opam-repository-with-archives
(cd $OPAMREPO && git checkout master && git pull ocaml master)

. ./autoconf/Makefile.config
echo VERSION=$VERSION

mkdir $OPAMREPO/package/ocp-build/ocp-build.$VERSION
cp opam descr findlib $OPAMREPO/package/ocp-build/ocp-build.$VERSION/

URL=http://github.com/OCamlPro/ocp-build/archive/${VERSION}.tar.gz
wget ${URL}
MD5SUM=$(md5sum ${VERSION}.tar.gz)
echo 'archive: "'${URL}'"' > $OPAMREPO/package/ocp-build/ocp-build.$VERSION/url
echo 'checksum: "'${MD5SUM}'"' >> $OPAMREPO/package/ocp-build/ocp-build.$VERSION/url

echo Generated url file:
cat $OPAMREPO/package/ocp-build/ocp-build.$VERSION/url

(cd ${OPAMREPO} &&
        git checkout -b ocp-build.${VERSION} &&
        git add package/ocp-build/ocp-build.${VERSION} &&
        git commit -m "Add ocp-build.${VERSION}"
 package/ocp-build/ocp-build.${VERSION} &&
     git push)
