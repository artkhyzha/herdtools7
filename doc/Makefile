# @todo Fix this error ('make' needs to be called twice to build the doc):
# ...
# compiling /Users/vincent/Dropbox/Hacking/Active/RDaS/main/herdtools7/doc/diy.tex...
# /Users/vincent/Dropbox/Hacking/Active/RDaS/main/herdtools7/doc/SB+CLUSTER.pstex_t:21: Missing $ inserted.
# /Users/vincent/Dropbox/Hacking/Active/RDaS/main/herdtools7/doc/SB+CLUSTER.pstex_t:21: leading text: }}}}
# /Users/vincent/Dropbox/Hacking/Active/RDaS/main/herdtools7/doc/SB+CLUSTER.pstex_t:21: Extra }, or forgotten $.
# ...

INSTALLDIR=$$HOME/public_html/diy/tst7
HERD=herd7 -exit true
SPLITDOT=splitdot7
all: docps docpdf dochtml
luc: dochtml installhtml

install: x86.tar
	/bin/rm -rf $(INSTALLDIR)/doc && mkdir -p $(INSTALLDIR)/doc
	$(MAKE) $(MFLAGS) installhtml
	$(MAKE) $(MFLAGS) installpdf
	cp x86.tar $(INSTALLDIR)/doc

clean::
	/bin/rm -f *~
SBL=SB+L-00.dot  SB+L-01.dot  SB+L-02.dot  \
 SB+L-03.dot  SB+L-04.dot  SB+L-05.dot

SBOK=SB+OK-00.dot  SB+OK-01.dot  SB+OK-02.dot  \
 SB+OK-03.dot  SB+OK-04.dot  SB+OK-05.dot

GENDOTS=SB-00.dot SB-01.dot SB-02.dot SB-03.dot CoRWR.dot \
 SB+SC.dot \
 $(SBL) $(SBOK) \
 SB+rfi-pos.dot  SB+rfi-pos+BIS.dot SB+rfi-pos+TER.dot \
 SB+CLUSTER.dot SB+FREE.dot  SB+COLUMNS.dot SB+SQUISHED.dot \
 SB+TSO.dot \
 MP+lwsync+addr.dot LB+lwsync+addr.dot \
 MP+lwsync+addr-po.dot MP+lwsync+addr-po001.dot \
 MP+X000.dot MP+X001.dot \
 WRC.dot SB+mfences.dot SB+mfences+GHB.dot \
 SB+mfences+CLUSTER.dot  SB+mfences+FREE.dot \
 IRIW+lwsyncs+APOIL.dot IRIW+syncs+APOIL.dot \
 6.SB.dot \
 RW.dot WW.dot WR.dot W+RR.dot W+RW.dot \


LB.dot: tst-arm/LB.litmus
	$(HERD) -conf doc.cfg -showinitrf true -show prop -model minimal.cat -o .  $< > /dev/null

SB.dot: SB.litmus
	$(HERD) -conf doc.cfg -showfinalrf true -showinitrf true -show all -model minimal.cat -o . SB.litmus > /dev/null

SB+L.dot: SB.litmus lamport.cat
	$(HERD) -suffix +L -conf doc.cfg -model lamport.cat -showinitwrites true  SB.litmus -skipchecks RfCons -oneinit false -show prop -o . -doshow S,rf-S -initpos -0.5,1  -xscale 1.75 -edgemerge true > /dev/null

SB+OK.dot: SB.litmus lamport.cat sb11.txt
	$(HERD) -suffix +OK -conf doc.cfg -model lamport.cat -showinitwrites true  SB.litmus -oneinit false -show prop -o . -doshow S,rf-S -initpos -0.5,1  -xscale 1.75 -edgemerge true -conds sb11.txt > /dev/null

6.SB.dot: 6.SB.litmus
	$(HERD) -conf doc.cfg -showinitrf true -showthread true -show prop -model minimal.cat -o . $< > /dev/null


SB+mfences.dot: SB+mfences.litmus
	$(HERD) -conf doc.cfg -showinitrf true -show prop -model minimal.cat -o . $< > /dev/null

SB+mfences+CLUSTER.dot: SB+mfences.litmus
	$(HERD) -suffix +CLUSTER -skipcheck tso -conf doc.cfg -showinitrf true -show prop -model ./tso-02.cat -graph cluster -showthread true -o . $< > /dev/null

SB+mfences+FREE.dot: SB+mfences.litmus
	$(HERD) -suffix +FREE -skipcheck tso -conf doc.cfg -showinitrf true -show prop -model ./tso-02.cat -graph free -pad 0.2 -showthread true -o . $< > /dev/null


SB+mfences+GHB.dot: SB+mfences.litmus
	$(HERD) -suffix +GHB -conf doc.cfg -showinitrf true -show prop -model ./tso-02.cat -skipcheck tso  -o . $< > /dev/null

SB+CLUSTER.dot: SB.litmus
	$(HERD) -suffix +CLUSTER -model tso-02.cat -show prop -o .  $< >/dev/null

THREEOPT=-showlegend false -squished true -margin 0.2
SB+SQUISHED.dot: SB.litmus
	$(HERD) -suffix +SQUISHED -model tso-02.cat $(THREEOPT) -show prop -graph cluster -o . $< >/dev/null

SB+FREE.dot: SB.litmus
	$(HERD) -suffix +FREE -model tso-02.cat  $(THREEOPT) -graph free -show prop -o . $< >/dev/null

SB+COLUMNS.dot: SB.litmus
	$(HERD) -suffix +COLUMNS -model tso-02.cat $(THREEOPT) -extrachars 1.0 -show prop -graph columns  -o . $< >/dev/null

SB+TSO.dot: SB.litmus
	$(HERD) -suffix +TSO -model tso-02.cat -show prop -conf doc.cfg -o . $< >/dev/null

SB+SC.dot: SB.litmus sc.cat
	$(HERD) -suffix +SC -conf doc.cfg -scale 1.5 -showfinalrf true -showinitrf true -show prop -model ./sc.cat -through invalid -o . SB.litmus > /dev/null

SB+rfi-pos.dot: SB+rfi-pos.litmus
	$(HERD) -conf doc.cfg -yscale 0.75 -showinitrf true -show prop -model minimal.cat -o . SB+rfi-pos.litmus > /dev/null

SB+rfi-pos+BIS.dot: SB+rfi-pos.litmus
	$(HERD) -suffix +BIS -conf doc.cfg -xscale 1.5  -showinitrf true -show prop -model tso-01.cat -o . $< > /dev/null

SB+rfi-pos+TER.dot: SB+rfi-pos.litmus
	$(HERD) -suffix +TER -conf doc.cfg -xscale 1.5 -showinitrf true -show prop -model tso-00.cat -through invalid -o . $< > /dev/null

CoRWR.dot: CoRWR.litmus tso-01.cat
	$(HERD) -conf doc.cfg -unshow ghb -initpos -1.0,0.3333 -oneinit false -showinitwrites true -show prop -model tso-01.cat -o . CoRWR.litmus -extrachars 1  > /dev/null

MOREAPOIL=-fontsize 10 -edgeattr prop,color,blueviolet
IRIW+lwsyncs+APOIL.dot: IRIW+lwsyncs.litmus
	$(HERD)  -suffix +APOIL -conf apoil.cfg $(MOREAPOIL) -doshow prop -show prop -o . $< > /dev/null

IRIW+syncs+APOIL.dot: IRIW+syncs.litmus
	$(HERD)  -suffix +APOIL -through invalid -conf apoil.cfg  $(MOREAPOIL) -doshow prop -show prop -o . $< > /dev/null

SB-00.dot SB-01.dot SB-02.dot SB-03.dot: SB.dot
	$(SPLITDOT) SB.dot >/dev/null

$(SBL): SB+L.dot
	$(SPLITDOT) SB+L.dot >/dev/null

$(SBOK): SB+OK.dot
	$(SPLITDOT) SB+OK.dot >/dev/null

MORECO=-showlegend true -shortlegend true -model minimal.cat

RW.dot: tst-co/basic/RW.litmus
	$(HERD)  -conf doc.cfg  $(MORECO) -show prop -o . $< >/dev/null

WW.dot: tst-co/basic/WW.litmus
	$(HERD)  -conf doc.cfg  $(MORECO) -show prop -o . $< >/dev/null

WR.dot: tst-co/basic/WR.litmus
	$(HERD)  -conf doc.cfg  $(MORECO) -show prop -o . $< >/dev/null

W+RR.dot: tst-co/basic/W+RR.litmus
	$(HERD)  -conf doc.cfg  $(MORECO) -show prop -o . $< >/dev/null

W+RW.dot: tst-co/basic/W+RW.litmus
	$(HERD)  -conf doc.cfg  $(MORECO) -show prop -o . $< >/dev/null


# WW.dot WR.dot W+RR.dot W+RW.dot

clean::
	/bin/rm -f LB.dot SB.dot SB+L.dot SB+OK.dot $(GENDOTS)

%.dot: %.litmus
	$(HERD) -model minimal.cat -conf doc.cfg -showinitrf true -showlegend true -shortlegend true  -show prop -o . $< > /dev/null

DOTS=classic.dot classic-sc.dot ws-sc.dot iriw-small-caps.dot iriw-internal.dot \
 IRIW.dot 2+2WLocal+DOT.dot LB.dot LB-kinds.dot \
 DetourR.dot DetourW.dot DetourRW.dot DetourWW.dot \
 $(GENDOTS)

X86=x86-safe.conf x86-rfi.conf x86-podwr.conf
TST=ARM-LB.log SB.litmus SB+onces.litmus SB.log SB-PPC.litmus SB-PPC.log  SB-PPC2.log\
  STFW-PPC.litmus 6.SB.litmus 6.SB+Prefetch.litmus WRC.litmus \
  SB-labelled.litmus classic.conf x86-2+2W.litmus \
  ppc-iriw-lwsync.litmus IRIW+lwsyncs.litmus IRIW+syncs.litmus\
  2+2W.litmus 2+2WObs.litmus 2+2WObsFenced.litmus 2+2WObsLoop.litmus \
  2+2WLocal.litmus 2+2WLocalFenced.litmus 2+2WLocalLoop.litmus \
  W.00 W5.litmus \
  MP.litmus MP+lwsync+addr.litmus MP+lwsyncs.litmus \
  MP+lwsync+addr-po.litmus MP+lwsync+addr-po001.litmus \
  LB+lwsyncs.litmus LB+lwsync+addr.litmus \
  MP+X000.litmus MP+X001.litmus \
  X.conf \
  iriw.litmus iriw-internal.litmus $(X86) \
  CoRWR.litmus SB+rfi-pos.litmus SB+mfences.litmus \
  R.litmus MP-TTD+DMB.ST+DMB.LD.litmus  MP-TTD+DMB.ST+DSB-ISB.litmus

STY=macros.sty common.tex syntaxdef.sty
HVA=macros.hva common.tex syntaxdef.hva
SRC=diy.tex litmus.tex gen.tex herd.tex jingle.tex examples.tex version.tex languages.tex
GENTEX=BASIC.tex MPPOSS.tex
GENSRC=$(GENTEX:.tex=-src)
GENIMG=$(GENTEX:.tex=-img)
PNG=$(DOTS:.dot=.png)
ALLPNG=$(PNG) m1l-time.png
PSTEX=$(DOTS:.dot=.pstex_t)
CFG=hpcx.cfg saumur.cfg apoil.cfg trimslice.cfg dragon.cfg
NOGENCAT=tso-00.cat tso-01.cat  tso-02.cat lamport.cat sc.cat
GENCAT=herd.cat x86tso.cat minimal.cat uniproc.cat cos-no-opt.cat  cross.cat stdlib.cat
CAT=$(NOGENCAT) $(GENCAT)
TXT=rename.txt cond.txt trimslice.sh dragon.sh kvm-log.txt
DONT=x86.sc x86.explo x86-log.txt
LITMUSDIR=../litmus/libdir
HERDDIR=../herd/libdir
MAPPLY=mapply7 -j 16
#Generated latex
clean::
	/bin/rm -rf $(GENTEX) $(GENSRC) $(GENIMG)

BASIC.tex:tst-co/basic/@all BASIC-img/index.tex
	sh tools/gen.sh BASIC tst-co/basic/@all > $@

BASIC-img:
	mkdir -p $@

BASIC-img/index.tex: BASIC-img tst-co/basic/@all
	$(MAPPLY) -com tools/img3.sh -comargs tools/UTCP.sh,BASIC-img tst-co/basic/@all > $@


MPPOSS.tex:tst-co/MPPOSS/@all MPPOSS-img/index.tex
	sh tools/gen.sh MPPOSS tst-co/MPPOSS/@all > $@

MPPOSS-img:
	mkdir -p $@

MPPOSS-img/index.tex: MPPOSS-img tst-co/MPPOSS/@all
	$(MAPPLY) -com tools/img3.sh -comargs tools/UTCP.sh,MPPOSS-img tst-co/MPPOSS/@all > $@



#tar of x86 configuration
x86.tar: x86.txt $(X86)
	rm -rf x86
	mkdir -p x86/safe && cp x86-safe.conf x86/safe
	mkdir x86/rfi && cp x86-rfi.conf x86/rfi
	mkdir x86/podwr && cp x86-podwr.conf x86/podwr
	cp x86.txt x86/README.txt
	tar cf x86.tar x86
	rm -rf x86

clean::
	/bin/rm -f x86.tar
#tar of x86/ppc allowed tests
tst-x86.tar:
	tar -cf tst-x86.tar tst-x86

tst-ppc.tar:
	tar -cf tst-ppc.tar tst-ppc

tst-arm.tar:
	tar -cf tst-arm.tar tst-arm

tst-co.tar:
	/bin/rm -rf /tmp/tst-co && \
	cp -r tst-co/src /tmp/tst-co  && \
	(cd /tmp && tar -cf - tst-co ) > $@

clean::
	/bin/rm -f tst-ppc.tar tst-x86.tar tst-arm.tar tst-co.tar

#version
version.tex: ../herdtools7.opam
	/bin/echo '\def\version{'$$(cat ../VERSION.txt)'}' > $@

clean::
	/bin/rm -f version.tex

#HTML
HEVEABIN=/usr/local/bin
HEVEA=$(HEVEABIN)/hevea
HEVEAOPTS=-fix -exec xxdate.exe -O
HACHA=$(HEVEABIN)/hacha
HACHAOPTS=-tocter

dochtml: html/index.html

html:
	mkdir -p html

html/diy.html: $(SRC) $(HVA) $(TST) $(CFG) $(CAT) $(TXT) $(DONT) $(ALLPNG) $(GENTEX) | html
	cp -r $(GENSRC) $(GENIMG) html
	 $(HEVEA) $(HEVEAOPTS) -o $@ diy.tex && \
        ( cp jade.bib html && cd html && bibhva diy && rm jade.bib ) && \
        $(HEVEA) $(HEVEAOPTS) -I html -o $@ diy.tex && \
	cp $(TST) $(ALLPNG) $(CFG) $(CAT) $(TXT) $(DONT) html
hva:
	$(HEVEA) $(HEVEAOPTS) -o html/diy.html diy.tex

TSTX86=html/tst-x86.tar html/tst-x86 html/x86-64.cfg
TSTPPC=html/tst-ppc.tar html/tst-ppc html/power7.cfg
TSTARM=html/tst-arm.tar html/tst-arm html/tegra3.cfg
TSTCO=html/tst-co.tar html/tst-co
CATS=html/uniproccat.txt


html/index.html: html/diy.html $(TSTX86) $(TSTPPC) $(TSTARM) $(TSTCO) $(CATS) | html
	$(HACHA) $(HACHAOPTS) -o html/index.html html/diy.html

html/tst-x86.tar: tst-x86.tar | html
	cp tst-x86.tar html/tst-x86.tar

html/tst-x86: tst-x86.tar | html
	( cd html && tar xf ../tst-x86.tar )

html/x86-64.cfg: x86-64.cfg | html
	cp x86-64.cfg $@

html/tst-ppc.tar: tst-ppc.tar | html
	cp tst-ppc.tar html/tst-ppc.tar

html/tst-ppc: tst-ppc.tar | html
	( cd html && tar xf ../tst-ppc.tar )

html/power7.cfg: power7.cfg | html
	cp power7.cfg $@

html/tst-arm.tar: tst-arm.tar | html
	cp tst-arm.tar html/tst-arm.tar

html/tst-arm: tst-arm.tar | html
	( cd html && tar xf ../tst-arm.tar )

html/tegra3.cfg: tegra3.cfg | html
	cp tegra3.cfg $@

html/tst-co.tar: tst-co.tar | html
	cp tst-co.tar html/tst-co.tar

html/tst-co: tst-co.tar | html
	( cd html && tar xf ../tst-co.tar )

html/uniproccat.txt: $(HERDDIR)/uniproccat.cat | html
	cp $(HERDDIR)/uniproccat.cat $@

cleanhtml:
	/bin/rm -f html/diy.html \
	 html/*.htoc html/*.haux html/*.hbbl html/*.blg

installhtml: cleanhtml
	cp -r html/* $(INSTALLDIR)/doc

clean::
	/bin/rm -rf html

#PS/PDF
LATEX ?= $(if $(shell which rubber),\
		rubber -f,\
		$(if $(shell which latexmk),\
			latexmk -dvi -ps- -pdf-,\
			$(error "Could not find either rubber or latexmk in PATH")))
DVIPS=dvips
PS2PDF=ps2pdf

docps: pdf/diy.ps
docpdf: pdf/diy.pdf

pdf/diy.dvi:  $(SRC) $(STY) $(TST) $(PSTEX) $(CAT)
	@mkdir -p pdf
	cp jade.bib pdf && cd pdf && \
        TEXINPUTS=..:$$TEXINPUTS $(LATEX) ../diy.tex

%.ps: %.dvi
	$(DVIPS) -o $*.ps $*.dvi

%.pdf: %.ps
	$(PS2PDF) $*.ps $*.pdf

installpdf:
	cp pdf/diy.pdf pdf/diy.ps $(INSTALLDIR)/doc

clean::
	/bin/rm -rf pdf

#copy from sources
hpcx.cfg: $(LITMUSDIR)/hpcx.cfg
	cp $< $@

trimslice.cfg: $(LITMUSDIR)/trimslice.cfg
	cp $< $@

dragon.cfg: $(LITMUSDIR)/dragon.cfg
	cp $< $@

saumur.cfg: $(LITMUSDIR)/saumur.cfg
	cp $< $@

%.cfg: $(HERDDIR)/%.cfg
	cp $< $@

%.cat: $(HERDDIR)/%.cat
	cp $< $@

clean::
	/bin/rm -f $(CFG) $(GENCAT)

#images
6.SB.png: 6.SB.dot
	neato -Tpng 6.SB.dot > $@

IRIW.png: IRIW.dot
	neato -Tpng IRIW.dot > $@

LB.png: LB.dot
	neato -Tpng LB.dot > $@

LB+lwsync+addr.png: LB+lwsync+addr.dot
	neato -Tpng LB+lwsync+addr.dot > $@

MP+lwsync+addr.png: MP+lwsync+addr.dot
	neato -Tpng MP+lwsync+addr.dot > $@

MP+lwsync+addr-po.png: MP+lwsync+addr-po.dot
	neato -Tpng MP+lwsync+addr-po.dot > $@

MP+lwsync+addr-po001.png: MP+lwsync+addr-po001.dot
	neato -Tpng MP+lwsync+addr-po001.dot > $@

MP+X000.png: MP+X000.dot
	neato -Tpng MP+X000.dot > $@

MP+X001.png: MP+X001.dot
	neato -Tpng MP+X001.dot > $@

WRC.png: WRC.dot
	neato -Tpng WRC.dot > $@

CoRWR.png: CoRWR.dot
	neato -Tpng CoRWR.dot > $@

SB+SC.png: SB+SC.dot
	neato -Tpng SB+SC.dot > $@

SB+COLUMNS.png: SB+COLUMNS.dot
	neato -Tpng SB+COLUMNS.dot > $@

SB+TSO.png: SB+TSO.dot
	neato -Tpng SB+TSO.dot > $@

SB+mfences.png: SB+mfences.dot
	neato -Tpng $< > $@

SB+mfences+GHB.png: SB+mfences+GHB.dot
	neato -Tpng $< > $@

SB-00.png: SB-00.dot
	neato -Tpng SB-00.dot > $@

SB-01.png: SB-01.dot
	neato -Tpng SB-01.dot > $@

SB-02.png: SB-02.dot
	neato -Tpng SB-02.dot > $@

SB-03.png: SB-03.dot
	neato -Tpng SB-03.dot > $@

SB+L-00.png: SB+L-00.dot
	neato -Tpng $< > $@
SB+L-01.png: SB+L-01.dot
	neato -Tpng $< > $@
SB+L-02.png: SB+L-02.dot
	neato -Tpng $< > $@
SB+L-03.png: SB+L-03.dot
	neato -Tpng $< > $@
SB+L-04.png: SB+L-04.dot
	neato -Tpng $< > $@
SB+L-05.png: SB+L-05.dot
	neato -Tpng $< > $@

SB+OK-00.png: SB+OK-00.dot
	neato -Tpng $< > $@
SB+OK-01.png: SB+OK-01.dot
	neato -Tpng $< > $@
SB+OK-02.png: SB+OK-02.dot
	neato -Tpng $< > $@
SB+OK-03.png: SB+OK-03.dot
	neato -Tpng $< > $@
SB+OK-04.png: SB+OK-04.dot
	neato -Tpng $< > $@
SB+OK-05.png: SB+OK-05.dot
	neato -Tpng $< > $@

RW.png: RW.dot
	neato -Tpng $< > $@

WW.png: WW.dot
	neato -Tpng $< > $@

WR.png: WR.dot
	neato -Tpng $< > $@

W+RW.png: W+RW.dot
	neato -Tpng $< > $@

W+RR.png: W+RR.dot
	neato -Tpng $< > $@

SB+rfi-pos.png: SB+rfi-pos.dot
	neato -Tpng  SB+rfi-pos.dot > $@

SB+rfi-pos+BIS.png: SB+rfi-pos+BIS.dot
	neato -Tpng  SB+rfi-pos+BIS.dot > $@

SB+rfi-pos+TER.png: SB+rfi-pos+TER.dot
	neato -Tpng  SB+rfi-pos+TER.dot > $@

LB-kinds.png: LB-kinds.dot
	dot -Gsize=8,8 -Tpng LB-kinds.dot > $@

DetourR.png: DetourR.dot
	neato -Tpng  $< > $@

DetourW.png: DetourW.dot
	neato -Tpng  $< > $@

DetourRW.png: DetourRW.dot
	neato -Tpng  $< > $@

DetourWW.png: DetourWW.dot
	neato -Tpng  $< > $@

%.png: %.dot
	dot -Tpng $< > $@

clean::
	/bin/rm -f $(PNG)

SZ=12

IRIW.pstex_t: IRIW.dot
	sh mkpstex.sh IRIW $(SZ) neato

LB.pstex_t: LB.dot
	sh mkpstex.sh LB $(SZ) neato

LB+lwsync+addr.pstex_t: LB+lwsync+addr.dot
	sh mkpstex.sh LB+lwsync+addr $(SZ) neato

MP+lwsync+addr.pstex_t: MP+lwsync+addr.dot
	sh mkpstex.sh MP+lwsync+addr $(SZ) neato

MP+lwsync+addr-po.pstex_t: MP+lwsync+addr-po.dot
	sh mkpstex.sh MP+lwsync+addr-po $(SZ) neato

MP+lwsync+addr-po001.pstex_t: MP+lwsync+addr-po001.dot
	sh mkpstex.sh MP+lwsync+addr-po001 $(SZ) neato

MP+X000.pstex_t: MP+X000.dot
	sh mkpstex.sh MP+X000 $(SZ) neato

MP+X001.pstex_t: MP+X001.dot
	sh mkpstex.sh MP+X001 $(SZ) neato

WRC.pstex_t: WRC.dot
	sh mkpstex.sh WRC $(SZ) neato

CoRWR.pstex_t: CoRWR.dot
	sh mkpstex.sh CoRWR $(SZ) neato

SB-00.pstex_t: SB-00.dot
	sh mkpstex.sh SB-00 $(SZ) neato

SB-01.pstex_t: SB-01.dot
	sh mkpstex.sh SB-01 $(SZ) neato

SB-02.pstex_t: SB-02.dot
	sh mkpstex.sh SB-02 $(SZ) neato

SB-03.pstex_t: SB-03.dot
	sh mkpstex.sh SB-03 $(SZ) neato

6.SB.pstex_t: 6.SB.dot
	sh mkpstex.sh 6.SB $(SZ) neato

DetourR.pstex_t: DetourR.dot
	sh mkpstex.sh DetourR $(SZ) neato

DetourW.pstex_t: DetourW.dot
	sh mkpstex.sh DetourW $(SZ) neato


DetourRW.pstex_t: DetourRW.dot
	sh mkpstex.sh DetourRW $(SZ) neato

DetourWW.pstex_t: DetourWW.dot
	sh mkpstex.sh DetourWW $(SZ) neato


%.pstex_t: %.dot
	sh mkpstex.sh $* $(SZ) dot


clean::
	/bin/rm -f $(PSTEX)
	/bin/rm -f $(DOTS:.dot=.fig)
	/bin/rm -f $(DOTS:.dot=.pstex)

gendots: $(GENDOTS)
