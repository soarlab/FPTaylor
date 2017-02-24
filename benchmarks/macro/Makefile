FPTAYLOR_HOME=../..
ROSA_HOME=~/Work/Solvers/rosa
GAPPA_HOME=~/Work/Solvers/gappa-1.1.2/src

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
FPTAYLOR_A=$(FPTAYLOR) -c FPTaylor/config-a.cfg
FPTAYLOR_B=$(FPTAYLOR) -c FPTaylor/config-b.cfg
FPTAYLOR_D=$(FPTAYLOR) -c FPTaylor/config-d.cfg
FPTAYLOR_TRANS_A=$(FPTAYLOR) -c trans/config-trans-a.cfg
FPTAYLOR_TRANS_B=$(FPTAYLOR) -c trans/config-trans-b.cfg

ROSA=$(ROSA_HOME)/rosa --real --z3Timeout=1000

GAPPA=$(GAPPA_HOME)/gappa


taylor-a:
	rm -rf log tmp
	$(FPTAYLOR_A) FPTaylor/carbonGas.txt
	$(FPTAYLOR_A) FPTaylor/verhulst.txt
	$(FPTAYLOR_A) FPTaylor/predatorPrey.txt
	$(FPTAYLOR_A) FPTaylor/rigidBody.txt
	$(FPTAYLOR_A) FPTaylor/doppler1.txt
	$(FPTAYLOR_A) FPTaylor/doppler2.txt
	$(FPTAYLOR_A) FPTaylor/doppler3.txt
	$(FPTAYLOR_A) FPTaylor/func.txt
	$(FPTAYLOR_A) FPTaylor/turbine.txt
	$(FPTAYLOR_A) FPTaylor/jet.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-d:
	rm -rf log tmp
	$(FPTAYLOR_D) FPTaylor/carbonGas.txt
	$(FPTAYLOR_D) FPTaylor/verhulst.txt
	$(FPTAYLOR_D) FPTaylor/predatorPrey.txt
	$(FPTAYLOR_D) FPTaylor/rigidBody.txt
	$(FPTAYLOR_D) FPTaylor/doppler1.txt
	$(FPTAYLOR_D) FPTaylor/doppler2.txt
	$(FPTAYLOR_D) FPTaylor/doppler3.txt
	$(FPTAYLOR_D) FPTaylor/func.txt
	$(FPTAYLOR_D) FPTaylor/turbine.txt
	$(FPTAYLOR_D) FPTaylor/jet.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-b:
	rm -rf log tmp
	$(FPTAYLOR_B) FPTaylor/carbonGas.txt
	$(FPTAYLOR_B) FPTaylor/verhulst.txt
	$(FPTAYLOR_B) FPTaylor/predatorPrey.txt
	$(FPTAYLOR_B) FPTaylor/rigidBody.txt
	$(FPTAYLOR_B) FPTaylor/doppler1.txt
	$(FPTAYLOR_B) FPTaylor/doppler2.txt
	$(FPTAYLOR_B) FPTaylor/doppler3.txt
	$(FPTAYLOR_B) FPTaylor/func.txt
	$(FPTAYLOR_B) FPTaylor/turbine.txt
	$(FPTAYLOR_B) FPTaylor/jet.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-trans-a:
	rm -rf log tmp
	$(FPTAYLOR_TRANS_A) trans/logexp.txt
	$(FPTAYLOR_TRANS_A) trans/point_on_sphere.txt
	$(FPTAYLOR_TRANS_A) trans/greatCircleAzimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-trans-b:
	rm -rf log tmp
	$(FPTAYLOR_TRANS_B) trans/logexp.txt
	$(FPTAYLOR_TRANS_B) trans/point_on_sphere.txt
	$(FPTAYLOR_TRANS_B) trans/greatCircleAzimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

show:
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

gappa:
	$(GAPPA) Gappa/carbonGas.g
	$(GAPPA) Gappa/verhulst.g
	$(GAPPA) Gappa/predatorPrey.g
	$(GAPPA) Gappa/rigidBody.g
	$(GAPPA) Gappa/doppler1.g
	$(GAPPA) Gappa/doppler2.g
	$(GAPPA) Gappa/doppler3.g
	$(GAPPA) Gappa/func.g
	$(GAPPA) Gappa/turbine.g
	$(GAPPA) Gappa/jet.g

gappa-hints:
	$(GAPPA) Gappa/carbonGas-hints.g
	$(GAPPA) Gappa/verhulst-hints.g
	$(GAPPA) Gappa/predatorPrey-hints.g
	$(GAPPA) Gappa/rigidBody-hints.g
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
rosa-rigid:
	mkdir -p generated
	$(ROSA) --simulation --functions=rigidBody1:rigidBody2 Rosa/RosaBenchmarks.scala

rosa-vp:
	mkdir -p generated
	$(ROSA) --simulation --functions=verhulst:predatorPrey Rosa/RosaBenchmarks.scala

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



