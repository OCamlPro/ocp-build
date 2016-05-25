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

opam pin add -y ocp-build .

  echo '${TRAVIS_BUILD_DIR} = ' ${TRAVIS_BUILD_DIR}
  echo '${TRAVIS_PULL_REQUEST} = ' ${TRAVIS_PULL_REQUEST}
  echo '${TRAVIS_REPO_SLUG} = ' ${TRAVIS_REPO_SLUG}
  echo '${TRAVIS_TAG} = ' ${TRAVIS_TAG}
  echo '${TRAVIS_BRANCH} = ' ${TRAVIS_BRANCH}
  echo '${TRAVIS_COMMIT} = ' ${TRAVIS_COMMIT}
  echo '${TRAVIS_COMMIT_RANGE} = ' ${TRAVIS_COMMIT_RANGE}

if [ "${OCAML_VERSION}" != "4.02.3" ] ; then
   echo No lint
else

   ocaml ./travis2github.ml

   echo -n '{ "body" : "' > travis-comment.txt
   echo -n "Hello from Travis job #${TRAVIS_BUILD_ID}" >> travis-comment.txt
   echo '" }' >> travis-comment.txt
   
   curl --user "ocp-lint-bot:${GITHUB_TOKEN}" -X POST --data  @travis-comment.txt "https://api.github.com/repos/${TRAVIS_REPO_SLUG}/issues/${TRAVIS_PULL_REQUEST}/comments"

   cd ../..
   git clone https://github.com/OCamlPro/typerex-lint.git OCamlPro/typerex-lint
   opam pin add -y typerex-lint OCamlPro/typerex-lint

   ocp-lint --path OCamlPro/ocp-build
   
fi

