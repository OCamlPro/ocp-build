#!/bin/sh

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

SUBPACKAGES="ocp-pp ocp-autoconf ocplib-compat ocplib-file ocplib-config ocplib-lang"

(cd ${OPAM_REPO} && git checkout master && git pull ${OPAM_REPO_OFFICIAL_REMOTE} master)


if [ -e ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION} ]; then
    echo "Error: directory for ${VERSION} already exists"
    echo ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}
    exit 2
fi

URL=${DOWNLOAD_URL_PREFIX}${VERSION}.tar.gz
TMPFILE=/tmp/push-ocaml.tmp
rm -f ${TMPFILE}
wget -o ${TMPFILE} ${URL} || exit 5
MD5SUM=$(md5sum ${TMPFILE} | cut -b 1-32) 

mkdir -p ${OPAM_REPO}/packages/${PACKAGE}
mkdir ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION} || (echo Error 1; exit 3)
cp opam descr findlib ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}/ || (echo Error 2; exit 4)

echo 'archive: "'${URL}'"' > ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}/url
echo 'checksum: "'${MD5SUM}'"' >> ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}/url

echo Generated url file:
cat ${OPAM_REPO}/packages/${PACKAGE}/${PACKAGE}.${VERSION}/url

for pkg in  ${SUBPACKAGES}; do
  mkdir -p ${OPAM_REPO}/packages/${pkg}
  mkdir ${OPAM_REPO}/packages/${pkg}/${pkg}.${VERSION}
  cp distrib/${pkg}.descr ${OPAM_REPO}/packages/${pkg}/${pkg}.${VERSION}/descr
  sed -e "s|@@VERSION@@|${VERSION}|" distrib/sub-package.opam > ${OPAM_REPO}/packages/${pkg}/${pkg}.${VERSION}/opam
done


(cd ${OPAM_REPO} &&
        git checkout -b ${PACKAGE}.${VERSION} &&
        git add packages/${PACKAGE}/${PACKAGE}.${VERSION} &&
        for pkg in  ${SUBPACKAGES}; do git add packages/${pkg}/${pkg}.${VERSION}/descr packages/${pkg}/${pkg}.${VERSION}/opam; done &&
        git commit -m "Add ${PACKAGE}.${VERSION}" packages &&
        git push ${OPAM_REPO_FORK_REMOTE} ${PACKAGE}.${VERSION})
