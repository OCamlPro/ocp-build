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

opam remove ocp-build
opam pin remove -y typerex-lint

rm -f my-package.install_log
opam pin -y add ocp-build . 2>&1 | tee -a  my-package.install_log
opam install -y ocp-build 2>&1 | tee -a my-package.install_log

curl -X POST --data  @my-package.install_log "http://github.lefessant.net:18080/travis?issue=${TRAVIS_PULL_REQUEST}&token=${TRANSIT_TOKEN}"

if [ $? -eq 0 ];then
   echo "Installation OK"
else
   exit 2
fi

rm -f my-package.remove_log
opam remove my-package 2>&1 | tee -a my-package.remove_log

curl -X POST --data  @my-package.remove_log "http://github.lefessant.net:18080/travis?issue=${TRAVIS_PULL_REQUEST}&token=${TRANSIT_TOKEN}"

if [ $? -eq 0 ];then
   echo "Removal OK"
else
   exit 2
fi

if [ "${OCAML_VERSION}" != "4.02.3" ] ; then
   echo No lint
else

  PROJECT=${TRAVIS_REPO_SLUG}

   cd ../..
   git clone https://github.com/OCamlPro/typerex-lint.git OCamlPro/typerex-lint
   opam pin add -y typerex-lint OCamlPro/typerex-lint

   ocp-lint --init
   ocp-lint --path ${PROJECT} > lint.stdout 2> lint.stderr
   cat lint.stdout lint.stderr > lint.log
   
   curl -X POST --data  @lint.log "http://github.lefessant.net:18080/travis?issue=${TRAVIS_PULL_REQUEST}&token=${TRANSIT_TOKEN}"
fi
