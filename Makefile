
MAKE_CONFIG := autoconf/Makefile.config

include $(MAKE_CONFIG)

OBUILD_DSTDIR=_obuild

# If you add a library ocplib-xxx:
# 1/ Update this list with xxx_SRCDIR=
STRcompat_SRCDIR=libs/ocplib-compat
compat_SRCDIR=$(STRcompat_SRCDIR)/string-compat
debug_SRCDIR=libs/ocplib-debug
lang_SRCDIR=libs/ocplib-lang
minunix_SRCDIR=libs/ocplib-unix
unix_SRCDIR=libs/ocplib-maxunix
file_SRCDIR=libs/ocplib-file
system_SRCDIR=libs/ocplib-system
ezcmd_SRCDIR=libs/ezcmd
config_SRCDIR=libs/ocplib-config
OCP_BUILD_SRCDIR=tools/ocp-build
OCP_BUILD_DSTDIR=$(OBUILD_DSTDIR)/ocp-build

OCPLIB_NAMES=debug lang unix file system config compat

CMDLINER_DIR := $(shell ocamlfind query cmdliner)
EXTERNAL_INCLUDES=    -I ${CMDLINER_DIR}
EXTERNAL_LIBS=${CMDLINER_DIR}/cmdliner.cmxa

INCLUDES=$(foreach lib, $(OCPLIB_NAMES), -I $($(lib)_SRCDIR)) \
    -I $(OCP_BUILD_SRCDIR) \
    -I libs/ezcmd \
    -I $(OCP_BUILD_SRCDIR)/misc \
    -I $(OCP_BUILD_SRCDIR)/lang-ocp \
    -I $(OCP_BUILD_SRCDIR)/lang-ocp2 \
    -I $(OCP_BUILD_SRCDIR)/engine \
    -I $(OCP_BUILD_SRCDIR)/actions \
    -I $(OCP_BUILD_SRCDIR)/meta \
    -I $(OCP_BUILD_SRCDIR)/ocaml \

OCPLIB_LIBS=$(foreach lib, $(OCPLIB_NAMES), ocplib-$(lib)) \
  ezcmd ocplib-file-compat

OCP_BUILD_BOOTER=boot/ocp-build.asm

STRING_COMPAT=\
	$(compat_SRCDIR)/ocpCompat.ml

EZCMD=$(ezcmd_SRCDIR)/ezcmd.ml

OCPLIB_DEBUG= $(debug_SRCDIR)/ocpDebug.ml

OCPLIB_LANG= $(lang_SRCDIR)/ocpList.ml $(lang_SRCDIR)/ocpString.ml	\
    $(lang_SRCDIR)/ocpArray.ml \
    $(lang_SRCDIR)/ocpDigest.ml $(lang_SRCDIR)/ocpToposort.ml	\
    $(lang_SRCDIR)/ocamllexer.ml $(lang_SRCDIR)/ocpGenlex.ml		\
    $(lang_SRCDIR)/ocpSubst.ml $(lang_SRCDIR)/ocpReuse.ml 

OCPLIB_UNIX= $(unix_SRCDIR)/minUnix.ml $(unix_SRCDIR)/onlyUnix.ml	\
    $(unix_SRCDIR)/onlyWin32.ml

OCPLIB_FILE= $(file_SRCDIR)/fileSig.ml $(file_SRCDIR)/fileOS.ml	\
    $(file_SRCDIR)/fileChannel.ml $(file_SRCDIR)/fileString.ml	\
    $(file_SRCDIR)/fileLines.ml $(file_SRCDIR)/fileGen.ml	\
    $(file_SRCDIR)/fileDir.ml 

OCPLIB_SYSTEM=

OCPLIB_CONFIG= \
    $(config_SRCDIR)/simpleConfigTypes.ml \
    $(config_SRCDIR)/simpleConfigOCaml.ml \
    $(config_SRCDIR)/simpleConfig.ml

BUILD_MISC= $(OCP_BUILD_SRCDIR)/misc/logger.ml	\
    $(OCP_BUILD_SRCDIR)/misc/buildMisc.ml	\
    $(OCP_BUILD_SRCDIR)/misc/buildWarnings.ml	\
    $(OCP_BUILD_SRCDIR)/misc/buildMtime.ml	\
    $(OCP_BUILD_SRCDIR)/misc/buildScanner.ml	\
    $(OCP_BUILD_SRCDIR)/misc/buildSubst.ml	\
    $(OCP_BUILD_SRCDIR)/misc/buildFind.ml	\
    $(OCP_BUILD_SRCDIR)/misc/buildTerm.ml	\
    $(OCP_BUILD_SRCDIR)/misc/versioning.ml	\
    $(OCP_BUILD_SRCDIR)/misc/ocamldot.ml	\
    $(OCP_BUILD_SRCDIR)/buildValue.ml


BUILD_PROJECT= \
    $(OCP_BUILD_SRCDIR)/buildOCPTypes.ml	\
    \
    $(OCP_BUILD_SRCDIR)/lang-ocp/buildOCPTree.ml			\
    $(OCP_BUILD_SRCDIR)/lang-ocp/buildOCPParser.ml		\
    $(OCP_BUILD_SRCDIR)/lang-ocp/buildOCPParse.ml		\
    $(OCP_BUILD_SRCDIR)/lang-ocp/buildOCPPrims.ml		\
    $(OCP_BUILD_SRCDIR)/lang-ocp/buildOCPInterp.ml		\
    \
    $(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Tree.ml		\
    $(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Lexer.ml     	\
    $(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Parser.ml		\
    $(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Parse.ml		\
    $(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Prims.ml		\
    $(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Interp.ml		\
    \
    $(OCP_BUILD_SRCDIR)/buildOCPPrinter.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCP.ml

BUILD_ENGINE= $(OCP_BUILD_SRCDIR)/engine/buildEngineTypes.ml	\
    $(OCP_BUILD_SRCDIR)/engine/buildEngineGlobals.ml		\
    $(OCP_BUILD_SRCDIR)/engine/buildEngineRules.ml		\
    $(OCP_BUILD_SRCDIR)/engine/buildEngineContext.ml		\
    $(OCP_BUILD_SRCDIR)/engine/buildEngineDisplay.ml		\
    $(OCP_BUILD_SRCDIR)/engine/buildEngineReport.ml 		\
    $(OCP_BUILD_SRCDIR)/engine/buildEngine.ml

BUILD_OCAML_OBJS= $(OCP_BUILD_SRCDIR)/ocaml/buildObjectInspector.ml

BUILD_LIB= $(OCP_BUILD_SRCDIR)/buildVersion.ml	\
    $(OCP_BUILD_SRCDIR)/buildTypes.ml		\
    $(OCP_BUILD_SRCDIR)/buildOptions.ml		\
    $(OCP_BUILD_SRCDIR)/buildGlobals.ml		\
    $(OCP_BUILD_SRCDIR)/buildConfig.ml		\
    $(OCP_BUILD_SRCDIR)/buildUninstall.ml		

BUILD_OCAMLFIND= $(OCP_BUILD_SRCDIR)/meta/metaTypes.ml	\
    $(OCP_BUILD_SRCDIR)/meta/metaLexer.ml		\
    $(OCP_BUILD_SRCDIR)/meta/metaParser.ml		\
    $(OCP_BUILD_SRCDIR)/meta/metaFile.ml		\
    $(OCP_BUILD_SRCDIR)/meta/metaConfig.ml

BUILD_OCAML= $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlConfig.ml	\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlTypes.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlMisc.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlVariables.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlGlobals.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamldep.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlSyntaxes.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlInstall.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlDotReport.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlVerifyPackages.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlOCP2.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlRules.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlMeta.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlTest.ml		\
    $(OCP_BUILD_SRCDIR)/ocaml/buildOasis.ml \
    $(OCP_BUILD_SRCDIR)/ocaml/buildOCamlPlugin.ml

BUILD_MAIN= $(OCP_BUILD_SRCDIR)/actions/buildArgs.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActions.ml		\
    $(OCP_BUILD_SRCDIR)/actions/buildActionsWarnings.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActionConfigure.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActionInit.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActionCheck.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActionPrefs.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActionMake.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActionInstall.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActionClean.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActionTests.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActionUninstall.ml	\
    $(OCP_BUILD_SRCDIR)/actions/buildActionQuery.ml	\
    $(OCP_BUILD_SRCDIR)/buildMain.ml

OCP_BUILD_MLS= $(STRING_COMPAT) $(EZCMD) $(OCPLIB_DEBUG)	\
  $(OCPLIB_LANG) $(OCPLIB_UNIX) $(OCPLIB_FILE) $(OCPLIB_SYSTEM)		\
  $(OCPLIB_CONFIG) $(BUILD_MISC) $(BUILD_PROJECT) $(BUILD_ENGINE)	\
  $(BUILD_OCAML_OBJS) $(BUILD_LIB) $(BUILD_OCAMLFIND) $(BUILD_OCAML)	\
  $(BUILD_MAIN)

OCP_BUILD_MLLS= \
   $(lang_SRCDIR)/ocamllexer.mll \
   $(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Lexer.mll     	\
   $(OCP_BUILD_SRCDIR)/meta/metaLexer.mll  \

OCP_BUILD_MLYS= $(OCP_BUILD_SRCDIR)/lang-ocp/buildOCPParser.mly

OCP_BUILD_CS= $(minunix_SRCDIR)/minUnix_c.c			\
 $(minunix_SRCDIR)/onlyWin32_c.c $(minunix_SRCDIR)/onlyUnix_c.c

OCP_BUILD_CMXS= $(OCP_BUILD_MLS:.ml=.cmx)
OCP_BUILD_CMOS= $(OCP_BUILD_MLS:.ml=.cmo)
OCP_BUILD_MLIS= $(OCP_BUILD_MLS:.ml=.mli)
OCP_BUILD_CMIS= $(OCP_BUILD_MLS:.ml=.cmi)
OCP_BUILD_STUBS= $(OCP_BUILD_CS:.c=.o)
OCP_BUILD_TMPS= $(OCP_BUILD_MLYS:.mly=.mli) $(OCP_BUILD_MLYS:.mly=.ml) \
	$(OCP_BUILD_MLLS:.mll=.ml) $(OCP_BUILD_ML4S:.ml4=.ml) \
	$(OCP_BUILD_SRCDIR)/buildVersion.ml \
	$(compat_SRCDIR)/ocpCompat.ml

OCP_BUILD_OS= $(OCP_BUILD_STUBS) $(OCP_BUILD_CMXS:.cmx=.o)

all: build-ocps
	@echo Libraries will be installed in ${ocamldir}
	@echo META files will be installed in ${metadir}

_obuild: Makefile

build-ocps: $(OCP_BUILD_BOOTER) _obuild
	$(OCP_BUILD_BOOTER)

depend: $(OCP_BUILD_MLS) $(OCP_BUILD_TMPS)
	$(OCAMLDEP) $(INCLUDES) $(OCP_BUILD_MLS) $(OCP_BUILD_MLIS) > .depend

$(OCP_BUILD_BOOTER): $(MAKE_CONFIG)
	$(MAKE) create-booter
	$(MAKE) partialclean

create-booter: $(OCP_BUILD_MLS) $(OCP_BUILD_CMXS) $(OCP_BUILD_STUBS)
	$(OCAMLOPT) -o $(OCP_BUILD_BOOTER) unix.cmxa str.cmxa $(EXTERNAL_LIBS) $(OCP_BUILD_CMXS) $(OCP_BUILD_STUBS)

byte: ocp-build.byte
ocp-build.byte: $(OCP_BUILD_MLS) $(OCP_BUILD_CMOS) $(OCP_BUILD_STUBS)
	$(OCAMLC) -custom -o ocp-build.byte unix.cma $(OCP_BUILD_CMOS) $(OCP_BUILD_STUBS)

partialclean:
	rm -f $(OCP_BUILD_TMPS) $(OCP_BUILD_CMIS) $(OCP_BUILD_CMOS) $(OCP_BUILD_CMXS) $(OCP_BUILD_OS)

clean: partialclean
	rm -f $(OCP_BUILD_BOOTER) ocp-build.byte
	rm -rf _obuild

distclean: clean ocp-distclean
	rm -f $(compat_SRCDIR)/ocpCompat.ml

#  "buildVersion.ml" (ocp2ml ; env_strings = [ "datadir" ])
$(OCP_BUILD_SRCDIR)/buildVersion.ml: Makefile $(MAKE_CONFIG)
	echo "let version=\"$(PACKAGE_VERSION)\"" > $(OCP_BUILD_SRCDIR)/buildVersion.ml

$(OCP_BUILD_SRCDIR)/lang-ocp/buildOCPParser.cmi: $(OCP_BUILD_SRCDIR)/lang-ocp/buildOCPParser.mli
	$(OCAMLC) -c -o $(OCP_BUILD_SRCDIR)/lang-ocp/buildOCPParser.cmi $(EXTERNAL_INCLUDES) $(INCLUDES) $(OCP_BUILD_SRCDIR)/lang-ocp/buildOCPParser.mli

$(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Parser.cmi: $(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Parser.mli
	$(OCAMLC) -c -o $(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Parser.cmi $(EXTERNAL_INCLUDES) $(INCLUDES) $(OCP_BUILD_SRCDIR)/lang-ocp2/buildOCP2Parser.mli

doc:
	cd docs/user-manual; $(MAKE)

install: install-ocp-build
	if test -f $(OBUILD_DSTDIR)/ocp-pp/ocp-pp.asm; then $(MAKE) install-ocp-pp; else :; fi

OCPBUILD_INSTALL=./_obuild/ocp-build/ocp-build.asm install		\
  --install-lib $(ocamldir) --install-meta $(metadir)                    \
  --install-bin $(bindir)

DST_SITE_OCP2 := ${ocamldir}/site-ocp2/ocp-build
SRC_SITE_OCP2 := ${OCP_BUILD_SRCDIR}/lang-ocp2/site-ocp2/ocp-build

install-ocp-build:
	mkdir -p ${ocamldir}/ocp-build
	cp -f boot/camlp4.ocp boot/ocaml.ocp ${ocamldir}/ocp-build
	mkdir -p ${DST_SITE_OCP2}
	cp -f ${SRC_SITE_OCP2}/*.ocp2 ${DST_SITE_OCP2}
	echo "generated = true" > ${ocamldir}/installed.ocp
	$(OCPBUILD_INSTALL) ocp-build
	$(OCPBUILD_INSTALL) $(OCPLIB_LIBS)

install-ocp-pp:
	$(OCPBUILD_INSTALL) ocp-pp ocp-autoconf

configure: autoconf/configure.ac autoconf/m4/*.m4
	cd autoconf/; \
		aclocal -I m4; \
		autoconf; \
		./configure $(CONFIGURE_ARGS)

$(compat_SRCDIR)/ocpCompat.ml: $(STRcompat_SRCDIR)/$(HAS_BYTES)/ocpCompat.ml
	cp -f $(STRcompat_SRCDIR)/$(HAS_BYTES)/ocpCompat.ml \
		$(compat_SRCDIR)/ocpCompat.ml

###########################################################################
#
#
#                           For OPAM
#
#
###########################################################################

## We need this tool installed to opamize ocp-build

OCP_OPAMER=ocp-opamer

push-tag:
	git push -f origin ocp-build.$(PACKAGE_VERSION)

tag:
	git tag ocp-build.$(PACKAGE_VERSION)
	$(MAKE) push-tag

force_tag:
	git tag -f ocp-build.$(PACKAGE_VERSION)
	$(MAKE) push-tag

opamize:
	$(MAKE) opamize-ocp-build
opamize-ocp-build:
	$(OCP_OPAMER) \
	 	-descr opam/ocp-build.descr \
		-opam opam/ocp-build.opam  \
		ocp-build $(PACKAGE_VERSION) \
		https://github.com/OCamlPro/ocp-build/tarball/ocp-build.$(PACKAGE_VERSION)

release:
	rm -rf /tmp/ocp-build.$(PACKAGE_VERSION)
	mkdir -p /tmp/ocp-build.$(PACKAGE_VERSION)
	cp -r . /tmp/ocp-build.$(PACKAGE_VERSION)
	(cd /tmp/ocp-build.$(PACKAGE_VERSION); make distclean)
	(cd /tmp; tar zcf /tmp/ocp-build.$(PACKAGE_VERSION).tar.gz ocp-build.$(PACKAGE_VERSION))
	scp /tmp/ocp-build.$(PACKAGE_VERSION).tar.gz webmaster@kimsufi2011:/home/www.typerex.com/www/pub/ocp-build/
	echo archive: \"http://www.typerex.org/pub/ocp-build/ocp-build.$(PACKAGE_VERSION).tar.gz\" > $(HOME)/BUILD/opam-cache-repo/packages/ocp-build/ocp-build.$(PACKAGE_VERSION)/url
	echo checksum: \"`md5sum /tmp/ocp-build.$(PACKAGE_VERSION).tar.gz | awk '{print $$1}'`\" >> $(HOME)/BUILD/opam-cache-repo/packages/ocp-build/ocp-build.$(PACKAGE_VERSION)/url

publish-opam:
	cd $(HOME)/.opam/opamer/opam-repository; git checkout master && git pull ocaml master && git checkout -b ocp-build.$(PACKAGE_VERSION)
	rm -rf $(HOME)/.opam/opamer/opam-repository/packages/ocp-build/ocp-build.$(PACKAGE_VERSION)
	cp -r $(HOME)/BUILD/opam-cache-repo/packages/ocp-build/ocp-build.$(PACKAGE_VERSION) $(HOME)/.opam/opamer/opam-repository/packages/ocp-build/ocp-build.$(PACKAGE_VERSION)
	cd $(HOME)/.opam/opamer/opam-repository; git add packages/ocp-build/ocp-build.$(PACKAGE_VERSION)


bootstrap:
	cp -f _obuild/ocp-build/ocp-build.asm $(OCP_BUILD_BOOTER)
	$(OCP_BUILD_BOOTER) clean
	$(OCP_BUILD_BOOTER)

include .depend

# for Makefile.rules to use our bootstrap ocp-build
OCP_BUILD:=$(OCP_BUILD_BOOTER)
include autoconf/Makefile.rules

.SUFFIXES: .ml .mll .mli .mly .c .o .cmo .cmi .cmx

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) $<

.ml.cmx:
	$(OCAMLOPT) -c -o $*.cmx $(EXTERNAL_INCLUDES) $(INCLUDES) $<

.mli.cmi:
	$(OCAMLC) -c -o $*.cmi $(EXTERNAL_INCLUDES) $(INCLUDES) $<

.ml.cmo:
	$(OCAMLC) -c -o $*.cmo $(EXTERNAL_INCLUDES) $(INCLUDES) $<

.c.o:
	$(OCAMLC) -c $(INCLUDES) $<
	mv `basename $*.o` $*.o



