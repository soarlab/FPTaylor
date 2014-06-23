FPTAYLOR_HOME=../..
ROSA_HOME=~/Work/Solvers/rosa
GAPPA_HOME=~/Work/Solvers/gappa-1.0.0/src

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
ROSA=$(ROSA_HOME)/rosa --real --z3Timeout=5000
GAPPA=$(GAPPA_HOME)/gappa


taylor:
	$(FPTAYLOR) -c config.cfg micro.txt

taylor2:
	$(FPTAYLOR) -c config2.cfg micro.txt

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
