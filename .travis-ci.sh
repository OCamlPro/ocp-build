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

opam pin add ocp-build .  >  my-package.install.log 2>&1
opam install ocp-build >> my-package.install.log 2>&1

if [ $? -eq 0 ];then
   echo "Installation OK"
else
   echo "Install on ${OCAML_VERSION} failed. End of log (30 lines):" > my-package.install.tail
   echo '```' >> my-package.install.tail
   tail -n 30 my-package.install.log >> my-package.install.tail
   echo '```' >> my-package.install.tail
  curl -X POST --data  @my-package.install.tail "http://github.lefessant.net:18080/travis?issue=${TRAVIS_PULL_REQUEST}&token=${TRANSIT_TOKEN}"
   exit 2
fi

opam remove my-package > my-package.remove.log 2>&1

if [ $? -eq 0 ];then
   echo "Removal OK"
else
    echo "Removal on ${OCAML_VERSION} failed. End of log (30 lines):" > my-package.remove.tail
   echo '```' > my-package.remove.tail
   tail -n 30 my-package.remove.log >> my-package.remove.tail
   echo '```' >> my-package.remove.tail
   curl -X POST --data  @my-package.remove.tail "http://github.lefessant.net:18080/travis?issue=${TRAVIS_PULL_REQUEST}&token=${TRANSIT_TOKEN}"

   exit 2
fi

exit 0

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
