FPTAYLOR_HOME=../..
ROSA_HOME=~/Work/Solvers/rosa
GAPPA_HOME=~/Work/Solvers/gappa-1.0.0/src

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor -c FPTaylor/config.cfg
FPTAYLOR2=$(FPTAYLOR_HOME)/fptaylor -c FPTaylor/config2.cfg
ROSA=$(ROSA_HOME)/rosa --real --z3Timeout=1000
GAPPA=$(GAPPA_HOME)/gappa


taylor:
	$(FPTAYLOR) FPTaylor/carbonGas.txt
	$(FPTAYLOR) FPTaylor/doppler1.txt
	$(FPTAYLOR) FPTaylor/doppler2.txt
	$(FPTAYLOR) FPTaylor/doppler3.txt
	$(FPTAYLOR) FPTaylor/func.txt
	$(FPTAYLOR) FPTaylor/jet.txt
	$(FPTAYLOR) FPTaylor/turbine.txt

taylor2:
	$(FPTAYLOR2) FPTaylor/carbonGas.txt
# Simple branch and bound methods cannot find exact maximum:
# use approximations
	$(FPTAYLOR2) -c FPTaylor/approx.cfg FPTaylor/doppler1.txt
	$(FPTAYLOR2) -c FPTaylor/approx.cfg FPTaylor/doppler2.txt
	$(FPTAYLOR2) -c FPTaylor/approx.cfg FPTaylor/doppler3.txt
	$(FPTAYLOR2) FPTaylor/func.txt
	$(FPTAYLOR2) FPTaylor/jet.txt
	$(FPTAYLOR2) FPTaylor/turbine.txt


gappa:
	$(GAPPA) Gappa/carbonGas.g
	$(GAPPA) Gappa/doppler1.g
	$(GAPPA) Gappa/doppler2.g
	$(GAPPA) Gappa/doppler3.g
	$(GAPPA) Gappa/func.g
	$(GAPPA) Gappa/jet.g
	$(GAPPA) Gappa/turbine.g

rosa-gas:
	mkdir -p generated
	$(ROSA) --functions=carbonGas Rosa/RosaBenchmarks.scala

rosa-doppler:
	mkdir -p generated
	$(ROSA) --functions=doppler1:doppler2:doppler3 Rosa/RosaBenchmarks.scala

rosa-func:
	mkdir -p generated
	$(ROSA) --functions=sine:sqroot:sineOrder3 Rosa/RosaBenchmarks.scala

rosa-jet:
	mkdir -p generated
	$(ROSA) --functions=jetEngine Rosa/RosaBenchmarks.scala

rosa-turbine:
	mkdir -p generated
	$(ROSA) --functions=turbine1:turbine2:turbine3 Rosa/RosaBenchmarks.scala

clean:
	rm -rf tmp generated log *.log *~



