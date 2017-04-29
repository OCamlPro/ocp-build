#!/bin/sh -e

#############################################################################
#
#          This file is managed by ocp-autoconf.
#
#  Remove it from `manage_files` in 'ocp-autoconf.config' if you want to
#  modify it manually.
#
#############################################################################

. ./autoconf/Makefile.config
VERSION=${PACKAGE_VERSION}
PACKAGE=${PACKAGE_NAME}

case "$1" in
    "") break;;
    -r)
        echo Reverting changes to opam-repo
        cd ${OPAM_REPO} && \
                git checkout master &&
                git branch -D ${PACKAGE}.${VERSION} &&
                git push ${OPAM_REPO_FORK_REMOTE} :${PACKAGE}.${VERSION};
        echo You can now restart.
        exit 0
        ;;
esac

if test -f ocp-autoconf.d/descr ; then :; else
  echo Missing required file 'descr'
  exit 2
fi



(cd ${OPAM_REPO} && git checkout master && git pull ${OPAM_REPO_OFFICIAL_REMOTE} master)


if [ ! -e ${OPAM_REPO}/packages ]; then
    echo "Error: directory ${OPAM_REPO}/packages does not exist";
    exit 2
fi

if [ -e ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION} ]; then
    echo "Error: directory for ${VERSION} already exists"
    echo ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}
    exit 2
fi

CMD="mkdir -p ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}"
echo $CMD
$CMD

CMD="cp opam ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}/"
echo $CMD
$CMD

CMD="cp ocp-autoconf.d/descr ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}/"
echo $CMD
$CMD || echo OK

if test -f ocp-autoconf.d/findlib; then
  CMD="cp ocp-autoconf.d/findlib ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}/"
  echo $CMD
  $CMD || echo OK
fi

URL=${DOWNLOAD_URL_PREFIX}${VERSION}.tar.gz
TMPFILE=/tmp/push-ocaml.tmp
rm -f ${TMPFILE}

CMD="wget -O ${TMPFILE} ${URL}"
echo $CMD
$CMD

echo Computing checksum on ${TMPFILE}
md5sum ${TMPFILE}
MD5SUM=$(md5sum ${TMPFILE} | cut -b 1-32)
echo 'archive: "'${URL}'"' > ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}/url
echo 'checksum: "'${MD5SUM}'"' >> ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}/url

echo Generated url file:
cat ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}/url

(cd ${OPAM_REPO} &&
        git checkout -b ${PACKAGE}.${VERSION} &&
        git add packages/${PACKAGE}/${PACKAGE}.${VERSION} &&
        git commit -m "Add ${PACKAGE}.${VERSION}" packages/${PACKAGE}/${PACKAGE}.${VERSION} &&
        git push ${OPAM_REPO_FORK_REMOTE} ${PACKAGE}.${VERSION}) || echo "Error: cleanup with git branch -D ${PACKAGE}.${VERSION}"
