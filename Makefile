
MAKE_CONFIG := autoconf/Makefile.config

include $(MAKE_CONFIG)

STRCOMPAT_SRCDIR=ocplib-compat/string-compat
DEBUG_SRCDIR=ocplib-debug
LANG_SRCDIR=ocplib-lang
UNIX_SRCDIR=ocplib-unix
SYSTEM_SRCDIR=ocplib-system
OCP_BUILD_SRCDIR=ocp-build
OCP_BUILD_DSTDIR=$(OBUILD_DSTDIR)/ocp-build

OCP_BUILD_BOOTER=boot/ocp-build.asm

STRING_COMPAT=$(STRCOMPAT_SRCDIR)/stringCompat.ml

OCPLIB_DEBUG= $(DEBUG_SRCDIR)/debugVerbosity.ml	\
    $(DEBUG_SRCDIR)/debugTyperex.ml

OCPLIB_LANG= $(LANG_SRCDIR)/ocpPervasives.ml				\
    $(LANG_SRCDIR)/ocpList.ml $(LANG_SRCDIR)/ocpString.ml		\
    $(LANG_SRCDIR)/ocpStream.ml $(LANG_SRCDIR)/ocpGenlex.ml		\
    $(LANG_SRCDIR)/ocpHashtbl.ml $(LANG_SRCDIR)/ocpDigest.ml		\
    $(LANG_SRCDIR)/option.ml $(LANG_SRCDIR)/intMap.ml			\
    $(LANG_SRCDIR)/intSet.ml $(LANG_SRCDIR)/stringMap.ml		\
    $(LANG_SRCDIR)/stringSet.ml $(LANG_SRCDIR)/toposort.ml		\
    $(LANG_SRCDIR)/linearToposort.ml $(LANG_SRCDIR)/ocamllexer.ml	\
    $(LANG_SRCDIR)/trie.ml $(LANG_SRCDIR)/ocpLang.ml			\
    $(LANG_SRCDIR)/stringSubst.ml $(LANG_SRCDIR)/manpage.ml		\
    $(LANG_SRCDIR)/stringTemplate.ml

OCPLIB_UNIX= $(UNIX_SRCDIR)/minUnix.ml $(UNIX_SRCDIR)/onlyUnix.ml	\
    $(UNIX_SRCDIR)/onlyWin32.ml

OCPLIB_SYSTEM= $(SYSTEM_SRCDIR)/reentrantBuffers.ml			\
    $(SYSTEM_SRCDIR)/file.ml $(SYSTEM_SRCDIR)/fileLines.ml		\
    $(SYSTEM_SRCDIR)/fileLabels.ml $(SYSTEM_SRCDIR)/date.ml		\
    $(SYSTEM_SRCDIR)/ocpUnix.ml $(SYSTEM_SRCDIR)/ocpFilename.ml	\
    $(SYSTEM_SRCDIR)/debug.ml $(SYSTEM_SRCDIR)/configParser.ml	\
    $(SYSTEM_SRCDIR)/simpleConfig.ml					\
    $(SYSTEM_SRCDIR)/fileTemplate.ml

BUILD_MISC= $(OCP_BUILD_SRCDIR)/logger.ml				\
    $(OCP_BUILD_SRCDIR)/buildMisc.ml					\
    $(OCP_BUILD_SRCDIR)/buildMtime.ml					\
    $(OCP_BUILD_SRCDIR)/buildScanner.ml					\
    $(OCP_BUILD_SRCDIR)/buildSubst.ml					\
    $(OCP_BUILD_SRCDIR)/buildFind.ml $(OCP_BUILD_SRCDIR)/buildTerm.ml	\
    $(OCP_BUILD_SRCDIR)/ocamldot.ml $(OCP_BUILD_SRCDIR)/buildValue.ml   \
    $(OCP_BUILD_SRCDIR)/versioning.ml

BUILD_PROJECT= $(OCP_BUILD_SRCDIR)/buildOCPTypes.ml	\
    $(OCP_BUILD_SRCDIR)/buildOCPTree.ml			\
    $(OCP_BUILD_SRCDIR)/buildOCPParser.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCPParse.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCPPrinter.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCPInterp.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCP.ml

BUILD_ENGINE= $(OCP_BUILD_SRCDIR)/buildEngineTypes.ml	\
    $(OCP_BUILD_SRCDIR)/buildEngineGlobals.ml		\
    $(OCP_BUILD_SRCDIR)/buildEngineRules.ml		\
    $(OCP_BUILD_SRCDIR)/buildEngineContext.ml		\
    $(OCP_BUILD_SRCDIR)/buildEngineDisplay.ml		\
    $(OCP_BUILD_SRCDIR)/buildEngine.ml

BUILD_OCAML_OBJS= $(OCP_BUILD_SRCDIR)/buildObjectInspector.ml

BUILD_LIB= $(OCP_BUILD_SRCDIR)/buildVersion.ml	\
    $(OCP_BUILD_SRCDIR)/buildTypes.ml		\
    $(OCP_BUILD_SRCDIR)/buildOptions.ml		\
    $(OCP_BUILD_SRCDIR)/buildGlobals.ml		\
    $(OCP_BUILD_SRCDIR)/buildConfig.ml		\
    $(OCP_BUILD_SRCDIR)/buildUninstall.ml		\
    $(OCP_BUILD_SRCDIR)/buildAutogen.ml

BUILD_OCAMLFIND= $(OCP_BUILD_SRCDIR)/metaTypes.ml			\
    $(OCP_BUILD_SRCDIR)/metaLexer.ml $(OCP_BUILD_SRCDIR)/metaFile.ml	\
    $(OCP_BUILD_SRCDIR)/metaParser.ml					\
    $(OCP_BUILD_SRCDIR)/metaConfig.ml

BUILD_OCAML= $(OCP_BUILD_SRCDIR)/buildOCamlConfig.ml	\
    $(OCP_BUILD_SRCDIR)/buildOCamlTypes.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCamlGlobals.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCamlMisc.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCamlVariables.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCamldep.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCamlSyntaxes.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCamlRules.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCamlInstall.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCamlMeta.ml		\
    $(OCP_BUILD_SRCDIR)/buildOCamlTest.ml		\
    $(OCP_BUILD_SRCDIR)/buildOasis.ml

BUILD_MAIN= $(OCP_BUILD_SRCDIR)/buildArgs.ml	\
    $(OCP_BUILD_SRCDIR)/buildActions.ml		\
    $(OCP_BUILD_SRCDIR)/buildActionRoot.ml	\
    $(OCP_BUILD_SRCDIR)/buildActionPrefs.ml	\
    $(OCP_BUILD_SRCDIR)/buildActionConfigure.ml	\
    $(OCP_BUILD_SRCDIR)/buildActionBuild.ml	\
    $(OCP_BUILD_SRCDIR)/buildActionInstall.ml	\
    $(OCP_BUILD_SRCDIR)/buildActionClean.ml	\
    $(OCP_BUILD_SRCDIR)/buildActionTests.ml	\
    $(OCP_BUILD_SRCDIR)/buildActionUninstall.ml	\
    $(OCP_BUILD_SRCDIR)/buildActionQuery.ml	\
    $(OCP_BUILD_SRCDIR)/buildActionHelp.ml	\
    $(OCP_BUILD_SRCDIR)/buildMain.ml

OCP_BUILD_MLS= $(STRING_COMPAT) $(OCPLIB_DEBUG) $(OCPLIB_LANG)		\
  $(OCPLIB_UNIX) $(OCPLIB_SYSTEM) $(BUILD_MISC) $(BUILD_PROJECT)	\
  $(BUILD_ENGINE) $(BUILD_OCAML_OBJS) $(BUILD_LIB) $(BUILD_OCAMLFIND)	\
  $(BUILD_OCAML) $(BUILD_MAIN)

OCP_BUILD_MLLS= \
   $(LANG_SRCDIR)/ocamllexer.mll $(OCP_BUILD_SRCDIR)/metaLexer.mll 

OCP_BUILD_MLYS= $(OCP_BUILD_SRCDIR)/buildOCPParser.mly
OCP_BUILD_ML4S= $(SYSTEM_SRCDIR)/simpleConfig.ml4

OCP_BUILD_CS= $(UNIX_SRCDIR)/minUnix_c.c			\
 $(UNIX_SRCDIR)/onlyWin32_c.c $(UNIX_SRCDIR)/onlyUnix_c.c

OCP_BUILD_CMXS= $(OCP_BUILD_MLS:.ml=.cmx)
OCP_BUILD_CMOS= $(OCP_BUILD_MLS:.ml=.cmo)
OCP_BUILD_MLIS= $(OCP_BUILD_MLS:.ml=.mli)
OCP_BUILD_CMIS= $(OCP_BUILD_MLS:.ml=.cmi)
OCP_BUILD_STUBS= $(OCP_BUILD_CS:.c=.o)
OCP_BUILD_TMPS= $(OCP_BUILD_MLYS:.mly=.mli) $(OCP_BUILD_MLYS:.mly=.ml) \
	$(OCP_BUILD_MLLS:.mll=.ml) $(OCP_BUILD_ML4S:.ml4=.ml) \
	$(OCP_BUILD_SRCDIR)/buildVersion.ml \
	$(STRCOMPAT_SRCDIR)/stringCompat.ml

OCP_BUILD_OS= $(OCP_BUILD_STUBS) $(OCP_BUILD_CMXS:.cmx=.o)

INCLUDES= -I $(STRCOMPAT_SRCDIR) -I $(DEBUG_SRCDIR) -I $(LANG_SRCDIR)	\
   -I $(SYSTEM_SRCDIR) -I $(UNIX_SRCDIR) -I $(OCP_BUILD_SRCDIR)

all: build-ocps

build-ocps: $(OCP_BUILD_BOOTER)
	$(OCP_BUILD_BOOTER) init
	$(OCP_BUILD_BOOTER)

depend: $(OCP_BUILD_MLS) $(OCP_BUILD_TMPS)
	$(OCAMLDEP) $(INCLUDES) $(OCP_BUILD_MLS) $(OCP_BUILD_MLIS) > .depend

$(OCP_BUILD_BOOTER): $(MAKE_CONFIG)
	$(MAKE) create-booter
	$(MAKE) partialclean

create-booter: $(OCP_BUILD_MLS) $(OCP_BUILD_CMXS) $(OCP_BUILD_STUBS)
	$(OCAMLOPT) -o $(OCP_BUILD_BOOTER) unix.cmxa $(OCP_BUILD_CMXS) $(OCP_BUILD_STUBS)

byte: ocp-build.byte
ocp-build.byte: $(OCP_BUILD_MLS) $(OCP_BUILD_CMOS) $(OCP_BUILD_STUBS)
	$(OCAMLC) -custom -o ocp-build.byte unix.cma $(OCP_BUILD_CMOS) $(OCP_BUILD_STUBS)

partialclean:
	rm -f $(OCP_BUILD_TMPS) $(OCP_BUILD_CMIS) $(OCP_BUILD_CMOS) $(OCP_BUILD_CMXS) $(OCP_BUILD_OS)

clean: partialclean
	rm -f $(OCP_BUILD_BOOTER) ocp-build.byte
	rm -rf _obuild

distclean: clean
	rm -f autoconf/config.log
	rm -f autoconf/config.status
	rm -f $(MAKE_CONFIG)
	rm -rf autoconf/autom4te.cache
	rm -f $(STRCOMPAT_SRCDIR)/stringCompat.ml

#  "buildVersion.ml" (ocp2ml ; env_strings = [ "datadir" ])
$(OCP_BUILD_SRCDIR)/buildVersion.ml: Makefile $(MAKE_CONFIG)
	echo "let version=\"$(VERSION)\"" > $(OCP_BUILD_SRCDIR)/buildVersion.ml

$(SYSTEM_SRCDIR)/simpleConfig.ml: $(SYSTEM_SRCDIR)/simpleConfig.ml4
	$(CAMLP4O) -impl $< > $*.ml

$(OCP_BUILD_SRCDIR)/buildOCPParser.cmi: $(OCP_BUILD_SRCDIR)/buildOCPParser.mli
	$(OCAMLC) -c -o $(OCP_BUILD_SRCDIR)/buildOCPParser.cmi $(INCLUDES) $(OCP_BUILD_SRCDIR)/buildOCPParser.mli

doc:
	cd docs/user-manual; $(MAKE)

install: install-ocp-build
	if test -f $(OBUILD_DSTDIR)/ocp-pp/ocp-pp.asm; then $(MAKE) install-ocp-pp; else :; fi

install-ocp-build:
	cp $(OCP_BUILD_DSTDIR)/ocp-build.asm ${BINDIR}/ocp-build
	mkdir -p ${LIBDIR}/ocp-build
	cp -f boot/camlp4.ocp boot/ocaml.ocp ${LIBDIR}/ocp-build
	cp -f build.ocp ${LIBDIR}/installed.ocp
	echo "generated = true" >> ${LIBDIR}/installed.ocp
	for lib in debug lang unix system compat subcmd; do \
		mkdir -p ${LIBDIR}/ocplib-$$lib; \
		cp -f ocplib-$$lib/build.ocp \
			 ${LIBDIR}/ocplib-$$lib/; \
		cp -f $(OBUILD_DSTDIR)/ocplib-$$lib/*.cmi \
		      $(OBUILD_DSTDIR)/ocplib-$$lib/*.cma \
		      $(OBUILD_DSTDIR)/ocplib-$$lib/*.cmx \
		      $(OBUILD_DSTDIR)/ocplib-$$lib/*.cmxa \
		      $(OBUILD_DSTDIR)/ocplib-$$lib/*.cmxs \
		      $(OBUILD_DSTDIR)/ocplib-$$lib/*.a \
				 ${LIBDIR}/ocplib-$$lib/; \
	done

install-ocp-pp:
	cp -f $(OBUILD_DSTDIR)/ocp-pp/ocp-pp.asm $(BINDIR)/ocp-pp

configure: autoconf/configure.ac autoconf/m4/*.m4
	cd autoconf/; \
		aclocal -I m4; \
		autoconf; \
		./configure $(CONFIGURE_ARGS)

$(STRCOMPAT_SRCDIR)/stringCompat.ml: ocplib-compat/$(HAS_BYTES)/stringCompat.ml
	cp -f ocplib-compat/$(HAS_BYTES)/stringCompat.ml \
		$(STRCOMPAT_SRCDIR)/stringCompat.ml

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
	git push -f origin ocp-build.$(VERSION)

tag:
	git tag ocp-build.$(VERSION)
	$(MAKE) push-tag

force_tag:
	git tag -f ocp-build.$(VERSION)
	$(MAKE) push-tag

opamize:
	$(MAKE) opamize-ocp-build
opamize-ocp-build:
	$(OCP_OPAMER) \
	 	-descr opam/ocp-build.descr \
		-opam opam/ocp-build.opam  \
		ocp-build $(VERSION) \
		https://github.com/OCamlPro/ocp-build/tarball/ocp-build.$(VERSION)

release:
	rm -rf /tmp/ocp-build.$(VERSION)
	mkdir -p /tmp/ocp-build.$(VERSION)
	cp -r . /tmp/ocp-build.$(VERSION)
	(cd /tmp/ocp-build.$(VERSION); make distclean)
	(cd /tmp; tar zcf /tmp/ocp-build.$(VERSION).tar.gz ocp-build.$(VERSION))
	scp /tmp/ocp-build.$(VERSION).tar.gz webmaster@kimsufi2011:/home/www.typerex.com/www/pub/ocp-build/
	echo archive: \"http://www.typerex.org/pub/ocp-build/ocp-build.$(VERSION).tar.gz\" > $(HOME)/BUILD/opam-cache-repo/packages/ocp-build/ocp-build.$(VERSION)/url
	echo checksum: \"`md5sum /tmp/ocp-build.$(VERSION).tar.gz | awk '{print $$1}'`\" >> $(HOME)/BUILD/opam-cache-repo/packages/ocp-build/ocp-build.$(VERSION)/url

publish-opam:
	cd $(HOME)/.opam/opamer/opam-repository; git checkout master && git pull ocaml master && git checkout -b ocp-build.$(VERSION)
	rm -rf $(HOME)/.opam/opamer/opam-repository/packages/ocp-build/ocp-build.$(VERSION)
	cp -r $(HOME)/BUILD/opam-cache-repo/packages/ocp-build/ocp-build.$(VERSION) $(HOME)/.opam/opamer/opam-repository/packages/ocp-build/ocp-build.$(VERSION)
	cd $(HOME)/.opam/opamer/opam-repository; git add packages/ocp-build/ocp-build.$(VERSION)




include .depend

.SUFFIXES: .ml .mll .mli .mly .c .o .cmo .cmi .cmx

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) $<

.ml.cmx:
	$(OCAMLOPT) -c -o $*.cmx $(INCLUDES) $<

.mli.cmi:
	$(OCAMLC) -c -o $*.cmi $(INCLUDES) $<

.ml.cmo:
	$(OCAMLC) -c -o $*.cmo $(INCLUDES) $<

.c.o:
	$(OCAMLC) -c $(INCLUDES) $<
	mv `basename $*.o` $*.o

