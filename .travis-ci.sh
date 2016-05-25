#!/bin/sh

export OPAMYES=1 OPAMVERBOSE=1
eval `opam config env`

echo Architecture
uname -a
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

PREFIX=$HOME/.opam/$OCAML_VERSION

./configure --prefix ${PREFIX} --with-metadir=${PREFIX}/lib
make
make install

if [ "${OCAML_VERSION}" == "4.02.1" ] ; then

   git clone https://github.com/OCamlPro/typerex-lint &&
   (cd typerex-lint;
    ./configure &&
     make &&
     make install) &&
   rm -rf typerex-lint

   ocp-lint -path .
   
fi

