FPTAYLOR_HOME=../..
ROSA_HOME=~/Work/Solvers/rosa
GAPPA_HOME=~/Work/Solvers/gappa-1.1.2/src

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
ROSA=$(ROSA_HOME)/rosa --real --z3Timeout=1000 --specGen --precision=single
GAPPA=$(GAPPA_HOME)/gappa -Blatex > gappa.tex

gappa:
	$(GAPPA) Gappa/add1.g

rosa:
	mkdir -p generated
	$(ROSA) --functions=sqrt_sub:sqrt_add sqrt.scala

clean:
	rm -rf *~ *.aux gappa.log

