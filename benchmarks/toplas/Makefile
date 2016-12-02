FPTAYLOR_HOME=../..
ROSA_HOME=~/Work/Solvers/rosa
ROSA_OPT_HOME=~/Work/Solvers/rosa-opt
GAPPA_HOME=~/Work/Solvers/gappa-1.3.0/src
GELPIA_HOME=~/Work/Solvers/gelpia

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
FPTAYLOR_A=$(FPTAYLOR) -c FPTaylor/config-a.cfg
FPTAYLOR_B=$(FPTAYLOR) -c FPTaylor/config-b.cfg
FPTAYLOR_B_PROOF=$(FPTAYLOR) -c FPTaylor/config-b.cfg --proof-record true --proof-dir FPTaylor/proofs
FPTAYLOR_C=$(FPTAYLOR) -c FPTaylor/config-c.cfg
FPTAYLOR_C_TOL=$(FPTAYLOR) -c FPTaylor/config-c-tol.cfg
FPTAYLOR_GELPIA=$(FPTAYLOR) -c FPTaylor/config-c.cfg --opt gelpia --opt-timeout 60000
FPTAYLOR_GELPIA_TOL=$(FPTAYLOR) -c FPTaylor/config-c-tol.cfg --opt gelpia --opt-timeout 60000

ROSA=$(ROSA_HOME)/rosa --real --z3Timeout=1000
ROSA_OPT=$(ROSA_OPT_HOME)/rosa --real --z3Timeout=1000
GAPPA=$(GAPPA_HOME)/gappa

taylor-a:
	rm -rf log tmp
	time $(FPTAYLOR_A) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_A) FPTaylor/doppler1.txt
	time $(FPTAYLOR_A) FPTaylor/doppler2.txt
	time $(FPTAYLOR_A) FPTaylor/doppler3.txt
	time $(FPTAYLOR_A) FPTaylor/jet.txt
	time $(FPTAYLOR_A) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_A) FPTaylor/rigidBody1.txt
	time $(FPTAYLOR_A) FPTaylor/rigidBody2.txt
	time $(FPTAYLOR_A) FPTaylor/sine.txt
	time $(FPTAYLOR_A) FPTaylor/sineOrder3.txt
	time $(FPTAYLOR_A) FPTaylor/sqroot.txt
	time $(FPTAYLOR_A) FPTaylor/t_div_t1.txt
	time $(FPTAYLOR_A) FPTaylor/turbine1.txt
	time $(FPTAYLOR_A) FPTaylor/turbine2.txt
	time $(FPTAYLOR_A) FPTaylor/turbine3.txt
	time $(FPTAYLOR_A) FPTaylor/verhulst.txt
	time $(FPTAYLOR_A) FPTaylor/logexp.txt
	time $(FPTAYLOR_A) FPTaylor/sphere.txt
	time $(FPTAYLOR_A) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-b:
	rm -rf log tmp
	time $(FPTAYLOR_B) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_B) FPTaylor/doppler1.txt
	time $(FPTAYLOR_B) FPTaylor/doppler2.txt
	time $(FPTAYLOR_B) FPTaylor/doppler3.txt
	time $(FPTAYLOR_B) FPTaylor/jet.txt
	time $(FPTAYLOR_B) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_B) FPTaylor/rigidBody1.txt
	time $(FPTAYLOR_B) FPTaylor/rigidBody2.txt
	time $(FPTAYLOR_B) FPTaylor/sine.txt
	time $(FPTAYLOR_B) FPTaylor/sineOrder3.txt
	time $(FPTAYLOR_B) FPTaylor/sqroot.txt
	time $(FPTAYLOR_B) FPTaylor/t_div_t1.txt
	time $(FPTAYLOR_B) FPTaylor/turbine1.txt
	time $(FPTAYLOR_B) FPTaylor/turbine2.txt
	time $(FPTAYLOR_B) FPTaylor/turbine3.txt
	time $(FPTAYLOR_B) FPTaylor/verhulst.txt
	time $(FPTAYLOR_B) FPTaylor/logexp.txt
	time $(FPTAYLOR_B) FPTaylor/sphere.txt
	time $(FPTAYLOR_B) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-b-formal:
	rm -rf log tmp
	time $(FPTAYLOR_B_PROOF) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/doppler1.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/doppler2.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/doppler3.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/jet.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/rigidBody1.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/rigidBody2.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/sine.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/sineOrder3.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/sqroot.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/t_div_t1.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/turbine1.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/turbine2.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/turbine3.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/verhulst.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/logexp.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/sphere.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-c:
	rm -rf log tmp
	time $(FPTAYLOR_C) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_C) FPTaylor/doppler1.txt
	time $(FPTAYLOR_C) FPTaylor/doppler2.txt
	time $(FPTAYLOR_C) FPTaylor/doppler3.txt
	time $(FPTAYLOR_C) FPTaylor/jet.txt
	time $(FPTAYLOR_C) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_C) FPTaylor/rigidBody1.txt
	time $(FPTAYLOR_C) FPTaylor/rigidBody2.txt
	time $(FPTAYLOR_C) FPTaylor/sine.txt
	time $(FPTAYLOR_C) FPTaylor/sineOrder3.txt
	time $(FPTAYLOR_C) FPTaylor/sqroot.txt
	time $(FPTAYLOR_C) FPTaylor/t_div_t1.txt
	time $(FPTAYLOR_C) FPTaylor/turbine1.txt
	time $(FPTAYLOR_C) FPTaylor/turbine2.txt
	time $(FPTAYLOR_C) FPTaylor/turbine3.txt
	time $(FPTAYLOR_C) FPTaylor/verhulst.txt
	time $(FPTAYLOR_C) FPTaylor/logexp.txt
	time $(FPTAYLOR_C) FPTaylor/sphere.txt
	time $(FPTAYLOR_C) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-c-tol:
	rm -rf log tmp
	time $(FPTAYLOR_C_TOL) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/doppler1.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/doppler2.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/doppler3.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/jet.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/rigidBody1.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/rigidBody2.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/sine.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/sineOrder3.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/sqroot.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/t_div_t1.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/turbine1.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/turbine2.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/turbine3.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/verhulst.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/logexp.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/sphere.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-gelpia:
	rm -rf log tmp
	export GELPIA_PATH=$(GELPIA_HOME)
	export GELPIA=$(GELPIA_HOME)/bin/gelpia
	time $(FPTAYLOR_GELPIA) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/doppler1.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/doppler2.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/doppler3.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/jet.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/rigidBody1.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/rigidBody2.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/sine.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/sineOrder3.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/sqroot.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/t_div_t1.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/turbine1.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/turbine2.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/turbine3.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/verhulst.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/logexp.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/sphere.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-gelpia-tol:
	rm -rf log tmp
	export GELPIA_PATH=$(GELPIA_HOME)
	export GELPIA=$(GELPIA_HOME)/bin/gelpia
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/doppler1.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/doppler2.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/doppler3.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/jet.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/rigidBody1.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/rigidBody2.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/sine.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/sineOrder3.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/sqroot.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/t_div_t1.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/turbine1.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/turbine2.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/turbine3.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/verhulst.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/logexp.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/sphere.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-a:
	rm -rf log tmp
	time $(FPTAYLOR_A) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_A) FPTaylor/kepler0.txt
	time $(FPTAYLOR_A) FPTaylor/kepler1.txt
	time $(FPTAYLOR_A) FPTaylor/kepler2.txt
	time $(FPTAYLOR_A) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-b:
	rm -rf log tmp
	time $(FPTAYLOR_B) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_B) FPTaylor/kepler0.txt
	time $(FPTAYLOR_B) FPTaylor/kepler1.txt
	time $(FPTAYLOR_B) FPTaylor/kepler2.txt
	time $(FPTAYLOR_B) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-b-formal:
	rm -rf log tmp
	time $(FPTAYLOR_B_PROOF) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/kepler0.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/kepler1.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/kepler2.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-c:
	rm -rf log tmp
	time $(FPTAYLOR_C) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_C) FPTaylor/kepler0.txt
	time $(FPTAYLOR_C) FPTaylor/kepler1.txt
	time $(FPTAYLOR_C) FPTaylor/kepler2.txt
	time $(FPTAYLOR_C) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-c-tol:
	rm -rf log tmp
	time $(FPTAYLOR_C_TOL) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/kepler0.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/kepler1.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/kepler2.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-gelpia:
	rm -rf log tmp
	export GELPIA_PATH=$(GELPIA_HOME)
	export GELPIA=$(GELPIA_HOME)/bin/gelpia
	time $(FPTAYLOR_GELPIA) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/kepler0.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/kepler1.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/kepler2.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-gelpia-tol:
	rm -rf log tmp
	export GELPIA_PATH=$(GELPIA_HOME)
	export GELPIA=$(GELPIA_HOME)/bin/gelpia
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/kepler0.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/kepler1.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/kepler2.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

gappa:
	time $(GAPPA) Gappa/carbonGas.g
	time $(GAPPA) Gappa/doppler1.g
	time $(GAPPA) Gappa/doppler2.g
	time $(GAPPA) Gappa/doppler3.g
	time $(GAPPA) Gappa/jet.g
	time $(GAPPA) Gappa/predatorPrey.g
	time $(GAPPA) Gappa/rigidBody1.g
	time $(GAPPA) Gappa/rigidBody2.g
	time $(GAPPA) Gappa/sine.g
	time $(GAPPA) Gappa/sineOrder3.g
	time $(GAPPA) Gappa/sqroot.g
	time $(GAPPA) Gappa/t_div_t1.g
	time $(GAPPA) Gappa/turbine1.g
	time $(GAPPA) Gappa/turbine2.g
	time $(GAPPA) Gappa/turbine3.g
	time $(GAPPA) Gappa/verhulst.g

gappa-hints:
	time $(GAPPA) Gappa/carbonGas-hints.g
	time $(GAPPA) Gappa/doppler1-hints.g
	time $(GAPPA) Gappa/doppler2-hints.g
	time $(GAPPA) Gappa/doppler3-hints.g
	time $(GAPPA) Gappa/jet-hints.g
	time $(GAPPA) Gappa/predatorPrey-hints.g
	time $(GAPPA) Gappa/rigidBody1-hints.g
	time $(GAPPA) Gappa/rigidBody2-hints.g
	time $(GAPPA) Gappa/sine-hints.g
	time $(GAPPA) Gappa/sineOrder3-hints.g
	time $(GAPPA) Gappa/sqroot-hints.g
	time $(GAPPA) Gappa/t_div_t1-hints.g
	time $(GAPPA) Gappa/turbine1-hints.g
	time $(GAPPA) Gappa/turbine2-hints.g
	time $(GAPPA) Gappa/turbine3-hints.g
	time $(GAPPA) Gappa/verhulst-hints.g

gappa-simple-hints:
	time $(GAPPA) Gappa/carbonGas-simple-hints.g
	time $(GAPPA) Gappa/doppler1-simple-hints.g
	time $(GAPPA) Gappa/doppler2-simple-hints.g
	time $(GAPPA) Gappa/doppler3-simple-hints.g
	time $(GAPPA) Gappa/jet-simple-hints.g
	time $(GAPPA) Gappa/predatorPrey-simple-hints.g
	time $(GAPPA) Gappa/rigidBody1-simple-hints.g
	time $(GAPPA) Gappa/rigidBody2-simple-hints.g
	time $(GAPPA) Gappa/sine-simple-hints.g
	time $(GAPPA) Gappa/sineOrder3-simple-hints.g
	time $(GAPPA) Gappa/sqroot-simple-hints.g
	time $(GAPPA) Gappa/t_div_t1-simple-hints.g
	time $(GAPPA) Gappa/turbine1-simple-hints.g
	time $(GAPPA) Gappa/turbine2-simple-hints.g
	time $(GAPPA) Gappa/turbine3-simple-hints.g
	time $(GAPPA) Gappa/verhulst-simple-hints.g

extra-gappa:
	time $(GAPPA) Gappa/himmilbeau.g
	time $(GAPPA) Gappa/kepler0.g
	time $(GAPPA) Gappa/kepler1.g
	time $(GAPPA) Gappa/kepler2.g

extra-gappa-hints:
	time $(GAPPA) Gappa/himmilbeau-hints.g
	time $(GAPPA) Gappa/kepler0-hints.g
	time $(GAPPA) Gappa/kepler1-hints.g
	time $(GAPPA) Gappa/kepler2-hints.g

extra-gappa-simple-hints:
	time $(GAPPA) Gappa/himmilbeau-simple-hints.g
	time $(GAPPA) Gappa/kepler0-simple-hints.g
	time $(GAPPA) Gappa/kepler1-simple-hints.g
	time $(GAPPA) Gappa/kepler2-simple-hints.g

rosa:
	time $(ROSA) --functions=carbonGas Rosa/rosa.scala
	time $(ROSA) --functions=doppler1 Rosa/rosa.scala
	time $(ROSA) --functions=doppler2 Rosa/rosa.scala
	time $(ROSA) --functions=doppler3 Rosa/rosa.scala
	time $(ROSA) --functions=jet Rosa/rosa.scala
	time $(ROSA) --functions=predatorPrey Rosa/rosa.scala
	time $(ROSA) --functions=rigidBody1 Rosa/rosa.scala
	time $(ROSA) --functions=rigidBody2 Rosa/rosa.scala
	time $(ROSA) --functions=sine Rosa/rosa.scala
	time $(ROSA) --functions=sineOrder3 Rosa/rosa.scala
	time $(ROSA) --functions=sqroot Rosa/rosa.scala
	time $(ROSA) --functions=t_div_t1 Rosa/rosa.scala
	time $(ROSA) --functions=turbine1 Rosa/rosa.scala
	time $(ROSA) --functions=turbine2 Rosa/rosa.scala
	time $(ROSA) --functions=turbine3 Rosa/rosa.scala
	time $(ROSA) --functions=verhulst Rosa/rosa.scala

extra-rosa:
	time $(ROSA) --functions=himmilbeau Rosa/rosa.scala
	time $(ROSA) --functions=kepler0 Rosa/rosa.scala
	time $(ROSA) --functions=kepler1 Rosa/rosa.scala
	time $(ROSA) --functions=kepler2 Rosa/rosa.scala

rosa-opt:
	time $(ROSA_OPT) --functions=carbonGas Rosa/rosa.scala
	time $(ROSA_OPT) --functions=doppler1 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=doppler2 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=doppler3 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=jet Rosa/rosa.scala
	time $(ROSA_OPT) --functions=predatorPrey Rosa/rosa.scala
	time $(ROSA_OPT) --functions=rigidBody1 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=rigidBody2 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=sine Rosa/rosa.scala
	time $(ROSA_OPT) --functions=sineOrder3 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=sqroot Rosa/rosa.scala
	time $(ROSA_OPT) --functions=t_div_t1 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=turbine1 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=turbine2 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=turbine3 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=verhulst Rosa/rosa.scala

extra-rosa-opt:
	time $(ROSA_OPT) --functions=himmilbeau Rosa/rosa.scala
	time $(ROSA_OPT) --functions=kepler0 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=kepler1 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=kepler2 Rosa/rosa.scala


show:
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

clean:
	rm -rf tmp generated log *.log *~



