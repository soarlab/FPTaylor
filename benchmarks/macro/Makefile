FPTAYLOR_HOME=../..
ROSA_HOME=~/Work/Solvers/rosa
GAPPA_HOME=~/Work/Solvers/gappa-1.0.0/src

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
FPTAYLOR_A=$(FPTAYLOR) -c FPTaylor/config-a.cfg
FPTAYLOR_B=$(FPTAYLOR) -c FPTaylor/config-b.cfg
FPTAYLOR_C=$(FPTAYLOR) -c FPTaylor/config-c.cfg
ROSA=$(ROSA_HOME)/rosa --real --z3Timeout=1000
GAPPA=$(GAPPA_HOME)/gappa


taylor-a:
	$(FPTAYLOR_A) FPTaylor/carbonGas.txt
	$(FPTAYLOR_A) FPTaylor/doppler1.txt
	$(FPTAYLOR_A) FPTaylor/doppler2.txt
	$(FPTAYLOR_A) FPTaylor/doppler3.txt
	$(FPTAYLOR_A) FPTaylor/func.txt
	$(FPTAYLOR_A) FPTaylor/turbine.txt
	$(FPTAYLOR_A) FPTaylor/jet.txt

taylor-b:
	$(FPTAYLOR_B) FPTaylor/carbonGas.txt
	$(FPTAYLOR_B) FPTaylor/doppler1.txt
	$(FPTAYLOR_B) FPTaylor/doppler2.txt
	$(FPTAYLOR_B) FPTaylor/doppler3.txt
	$(FPTAYLOR_B) FPTaylor/func.txt
	$(FPTAYLOR_B) FPTaylor/turbine.txt
	$(FPTAYLOR_B) FPTaylor/jet.txt

taylor-c:
	$(FPTAYLOR_C) FPTaylor/carbonGas.txt
# A simple branch and bound method cannot find exact maximum;
# use approximations
	$(FPTAYLOR_C) -c FPTaylor/approx.cfg FPTaylor/doppler1.txt
	$(FPTAYLOR_C) -c FPTaylor/approx.cfg FPTaylor/doppler2.txt
	$(FPTAYLOR_C) -c FPTaylor/approx.cfg FPTaylor/doppler3.txt
	$(FPTAYLOR_C) FPTaylor/func.txt
	$(FPTAYLOR_C) FPTaylor/turbine.txt
	$(FPTAYLOR_C) FPTaylor/jet.txt

gappa:
	$(GAPPA) Gappa/carbonGas.g
	$(GAPPA) Gappa/doppler1.g
	$(GAPPA) Gappa/doppler2.g
	$(GAPPA) Gappa/doppler3.g
	$(GAPPA) Gappa/func.g
	$(GAPPA) Gappa/turbine.g
	$(GAPPA) Gappa/jet.g

gappa-hints:
	$(GAPPA) Gappa/carbonGas-hints.g
	$(GAPPA) Gappa/doppler1-hints.g
	$(GAPPA) Gappa/doppler2-hints.g
	$(GAPPA) Gappa/doppler3-hints.g
	$(GAPPA) Gappa/sine-hints.g
	$(GAPPA) Gappa/sqroot-hints.g
	$(GAPPA) Gappa/sineOrder3-hints.g
	$(GAPPA) Gappa/turbine1-hints.g
	$(GAPPA) Gappa/turbine2-hints.g
	$(GAPPA) Gappa/turbine3-hints.g
	$(GAPPA) Gappa/jet-hints.g

# Simulation does not work: returns the same results as the verification
rosa-sim:
	mkdir -p generated
	$(ROSA) --simulation --functions=sine Rosa/RosaBenchmarks.scala

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



