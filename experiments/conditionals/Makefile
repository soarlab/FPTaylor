FPTAYLOR_HOME=../..
ROSA_HOME=~/Work/Solvers/rosa
GAPPA_HOME=~/Work/Solvers/gappa-1.0.0/src

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
ROSA=$(ROSA_HOME)/rosa --real --z3Timeout=1000
GAPPA=$(GAPPA_HOME)/gappa


taylor:
	$(FPTAYLOR) -c config.cfg triangle1.txt

rosa-dir:
	mkdir -p generated

rosa-test-then : rosa-dir
	$(ROSA) --specGen \
		--functions=test2_then \
		Rosa/test.scala

rosa-test1 : rosa-dir
	$(ROSA) --specGen \
		--functions=test1 \
		Rosa/test.scala

rosa-test2 : rosa-dir
	$(ROSA) --specGen \
		--functions=test2 \
		Rosa/test.scala

rosa-test3 : rosa-dir
	$(ROSA) --specGen \
		--functions=test3 \
		Rosa/test.scala

rosa-tri1: rosa-dir
	$(ROSA) --specGen \
		--functions=triangle1 \
		Rosa/TriangleProgression.scala

rosa-tri-high: rosa-dir
	$(ROSA) --specGen \
		--functions=triangle10:triangle11:triangle12 \
		Rosa/TriangleProgression.scala

rosa-tri9: rosa-dir
	$(ROSA) --specGen \
		--functions=triangle9 \
		Rosa/TriangleProgression.scala

rosa-tri8: rosa-dir
	$(ROSA) --specGen \
		--functions=triangle8 \
		Rosa/TriangleProgression.scala

rosa-tri: rosa-dir
	$(ROSA) --specGen \
		--functions=_triangle1:_triangle2:_triangle3:_triangle12 \
		Rosa/TriangleProgression.scala

rosa-tri-c: rosa-dir
	$(ROSA) --specGen \
		--functions=triangle1_c \
		Rosa/TriangleProgression.scala

gappa_nonlin1:
	$(GAPPA) gappa_nonlin1.g
	$(GAPPA) gappa_nonlin1_hints.g

gappa_nonlin2:
	$(GAPPA) gappa_nonlin2.g
	$(GAPPA) gappa_nonlin2_hints.g

gappa_sum3:
	$(GAPPA) gappa_sum3.g
	$(GAPPA) gappa_sum3_hints.g

rosa_sum3:
	mkdir -p generated
	$(ROSA) --functions=sum3 rosa.scala

rosa_nonlin1:
	mkdir -p generated
	$(ROSA) --functions=nonlin1 rosa.scala

rosa_nonlin2:
	mkdir -p generated
	$(ROSA) --functions=nonlin2 rosa.scala

clean:
	rm -rf generated tmp log
	rm -f *~ *.log
