FPTAYLOR_HOME=../..
ROSA_HOME=~/Work/Solvers/rosa
GAPPA_HOME=~/Work/Solvers/gappa-1.3.0/src
GELPIA_HOME=~/Work/Solvers/gelpia

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
FPTAYLOR_A=$(FPTAYLOR) -c FPTaylor/config-a.cfg
FPTAYLOR_B=$(FPTAYLOR) -c FPTaylor/config-b.cfg
FPTAYLOR_C=$(FPTAYLOR) -c FPTaylor/config-c.cfg
FPTAYLOR_GELPIA=$(FPTAYLOR) -c FPTaylor/config-c.cfg --opt gelpia --opt-timeout 60000

ROSA=$(ROSA_HOME)/rosa --real --z3Timeout=1000
GAPPA=$(GAPPA_HOME)/gappa

taylor-a:
	rm -rf log tmp
	$(FPTAYLOR_A) FPTaylor/t_div_t1.txt
	$(FPTAYLOR_A) FPTaylor/carbonGas.txt
	$(FPTAYLOR_A) FPTaylor/verhulst.txt
	$(FPTAYLOR_A) FPTaylor/predatorPrey.txt
	$(FPTAYLOR_A) FPTaylor/rigidBody1.txt
	$(FPTAYLOR_A) FPTaylor/rigidBody2.txt
	$(FPTAYLOR_A) FPTaylor/doppler1.txt
	$(FPTAYLOR_A) FPTaylor/doppler2.txt
	$(FPTAYLOR_A) FPTaylor/doppler3.txt
	$(FPTAYLOR_A) FPTaylor/sine.txt
	$(FPTAYLOR_A) FPTaylor/sqroot.txt
	$(FPTAYLOR_A) FPTaylor/sineOrder3.txt
	$(FPTAYLOR_A) FPTaylor/turbine1.txt
	$(FPTAYLOR_A) FPTaylor/turbine2.txt
	$(FPTAYLOR_A) FPTaylor/turbine3.txt
	$(FPTAYLOR_A) FPTaylor/jet.txt
	$(FPTAYLOR_A) FPTaylor/logexp.txt
	$(FPTAYLOR_A) FPTaylor/sphere.txt
	$(FPTAYLOR_A) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-b:
	rm -rf log tmp
	$(FPTAYLOR_B) FPTaylor/t_div_t1.txt
	$(FPTAYLOR_B) FPTaylor/carbonGas.txt
	$(FPTAYLOR_B) FPTaylor/verhulst.txt
	$(FPTAYLOR_B) FPTaylor/predatorPrey.txt
	$(FPTAYLOR_B) FPTaylor/rigidBody1.txt
	$(FPTAYLOR_B) FPTaylor/rigidBody2.txt
	$(FPTAYLOR_B) FPTaylor/doppler1.txt
	$(FPTAYLOR_B) FPTaylor/doppler2.txt
	$(FPTAYLOR_B) FPTaylor/doppler3.txt
	$(FPTAYLOR_B) FPTaylor/sine.txt
	$(FPTAYLOR_B) FPTaylor/sqroot.txt
	$(FPTAYLOR_B) FPTaylor/sineOrder3.txt
	$(FPTAYLOR_B) FPTaylor/turbine1.txt
	$(FPTAYLOR_B) FPTaylor/turbine2.txt
	$(FPTAYLOR_B) FPTaylor/turbine3.txt
	$(FPTAYLOR_B) FPTaylor/jet.txt
	$(FPTAYLOR_B) FPTaylor/logexp.txt
	$(FPTAYLOR_B) FPTaylor/sphere.txt
	$(FPTAYLOR_B) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-c:
	rm -rf log tmp
	$(FPTAYLOR_C) FPTaylor/t_div_t1.txt
	$(FPTAYLOR_C) FPTaylor/carbonGas.txt
	$(FPTAYLOR_C) FPTaylor/verhulst.txt
	$(FPTAYLOR_C) FPTaylor/predatorPrey.txt
	$(FPTAYLOR_C) FPTaylor/rigidBody1.txt
	$(FPTAYLOR_C) FPTaylor/rigidBody2.txt
	$(FPTAYLOR_C) FPTaylor/doppler1.txt
	$(FPTAYLOR_C) FPTaylor/doppler2.txt
	$(FPTAYLOR_C) FPTaylor/doppler3.txt
	$(FPTAYLOR_C) FPTaylor/sine.txt
	$(FPTAYLOR_C) FPTaylor/sqroot.txt
	$(FPTAYLOR_C) FPTaylor/sineOrder3.txt
	$(FPTAYLOR_C) FPTaylor/turbine1.txt
	$(FPTAYLOR_C) FPTaylor/turbine2.txt
	$(FPTAYLOR_C) FPTaylor/turbine3.txt
	$(FPTAYLOR_C) FPTaylor/jet.txt
	$(FPTAYLOR_C) FPTaylor/logexp.txt
	$(FPTAYLOR_C) FPTaylor/sphere.txt
	$(FPTAYLOR_C) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-gelpia:
	rm -rf log tmp
	export GELPIA_PATH=$(GELPIA_HOME)
	export GELPIA=$(GELPIA_HOME)/bin/gelpia
	$(FPTAYLOR_GELPIA) FPTaylor/t_div_t1.txt
	$(FPTAYLOR_GELPIA) FPTaylor/carbonGas.txt
	$(FPTAYLOR_GELPIA) FPTaylor/verhulst.txt
	$(FPTAYLOR_GELPIA) FPTaylor/predatorPrey.txt
	$(FPTAYLOR_GELPIA) FPTaylor/rigidBody1.txt
	$(FPTAYLOR_GELPIA) FPTaylor/rigidBody2.txt
	$(FPTAYLOR_GELPIA) FPTaylor/doppler1.txt
	$(FPTAYLOR_GELPIA) FPTaylor/doppler2.txt
	$(FPTAYLOR_GELPIA) FPTaylor/doppler3.txt
	$(FPTAYLOR_GELPIA) FPTaylor/sine.txt
	$(FPTAYLOR_GELPIA) FPTaylor/sqroot.txt
	$(FPTAYLOR_GELPIA) FPTaylor/sineOrder3.txt
	$(FPTAYLOR_GELPIA) FPTaylor/turbine1.txt
	$(FPTAYLOR_GELPIA) FPTaylor/turbine2.txt
	$(FPTAYLOR_GELPIA) FPTaylor/turbine3.txt
	$(FPTAYLOR_GELPIA) FPTaylor/jet.txt
	$(FPTAYLOR_GELPIA) FPTaylor/logexp.txt
	$(FPTAYLOR_GELPIA) FPTaylor/sphere.txt
	$(FPTAYLOR_GELPIA) FPTaylor/azimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-a:
	rm -rf log tmp
	$(FPTAYLOR_A) FPTaylor/kepler0.txt
	$(FPTAYLOR_A) FPTaylor/kepler1.txt
	$(FPTAYLOR_A) FPTaylor/kepler2.txt
	$(FPTAYLOR_A) FPTaylor/himmilbeau.txt
	$(FPTAYLOR_A) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-b:
	rm -rf log tmp
	$(FPTAYLOR_B) FPTaylor/kepler0.txt
	$(FPTAYLOR_B) FPTaylor/kepler1.txt
	$(FPTAYLOR_B) FPTaylor/kepler2.txt
	$(FPTAYLOR_B) FPTaylor/himmilbeau.txt
	$(FPTAYLOR_B) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-c:
	rm -rf log tmp
	$(FPTAYLOR_C) FPTaylor/kepler0.txt
	$(FPTAYLOR_C) FPTaylor/kepler1.txt
	$(FPTAYLOR_C) FPTaylor/kepler2.txt
	$(FPTAYLOR_C) FPTaylor/himmilbeau.txt
	$(FPTAYLOR_C) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-gelpia:
	rm -rf log tmp
	export GELPIA_PATH=$(GELPIA_HOME)
	export GELPIA=$(GELPIA_HOME)/bin/gelpia
	$(FPTAYLOR_GELPIA) FPTaylor/kepler0.txt
	$(FPTAYLOR_GELPIA) FPTaylor/kepler1.txt
	$(FPTAYLOR_GELPIA) FPTaylor/kepler2.txt
	$(FPTAYLOR_GELPIA) FPTaylor/himmilbeau.txt
	$(FPTAYLOR_GELPIA) FPTaylor/hartman3.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

gappa:
	$(GAPPA) Gappa/t_div_t1.g
	$(GAPPA) Gappa/carbonGas.g
	$(GAPPA) Gappa/verhulst.g
	$(GAPPA) Gappa/predatorPrey.g
	$(GAPPA) Gappa/rigidBody1.g
	$(GAPPA) Gappa/rigidBody2.g
	$(GAPPA) Gappa/doppler1.g
	$(GAPPA) Gappa/doppler2.g
	$(GAPPA) Gappa/doppler3.g
	$(GAPPA) Gappa/sine.g
	$(GAPPA) Gappa/sqroot.g
	$(GAPPA) Gappa/sineOrder3.g
	$(GAPPA) Gappa/turbine1.g
	$(GAPPA) Gappa/turbine2.g
	$(GAPPA) Gappa/turbine3.g
	$(GAPPA) Gappa/jet.g

gappa-hints:
	$(GAPPA) Gappa/t_div_t1-hints.g
	$(GAPPA) Gappa/carbonGas-hints.g
	$(GAPPA) Gappa/verhulst-hints.g
	$(GAPPA) Gappa/predatorPrey-hints.g
	$(GAPPA) Gappa/rigidBody1-hints.g
	$(GAPPA) Gappa/rigidBody2-hints.g
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

gappa-simple-hints:
	$(GAPPA) Gappa/t_div_t1-simple-hints.g
	$(GAPPA) Gappa/carbonGas-simple-hints.g
	$(GAPPA) Gappa/verhulst-simple-hints.g
	$(GAPPA) Gappa/predatorPrey-simple-hints.g
	$(GAPPA) Gappa/rigidBody1-simple-hints.g
	$(GAPPA) Gappa/rigidBody2-simple-hints.g
	$(GAPPA) Gappa/doppler1-simple-hints.g
	$(GAPPA) Gappa/doppler2-simple-hints.g
	$(GAPPA) Gappa/doppler3-simple-hints.g
	$(GAPPA) Gappa/sine-simple-hints.g
	$(GAPPA) Gappa/sqroot-simple-hints.g
	$(GAPPA) Gappa/sineOrder3-simple-hints.g
	$(GAPPA) Gappa/turbine1-simple-hints.g
	$(GAPPA) Gappa/turbine2-simple-hints.g
	$(GAPPA) Gappa/turbine3-simple-hints.g
	$(GAPPA) Gappa/jet-simple-hints.g

extra-gappa:
	$(GAPPA) Gappa/kepler0.g
	$(GAPPA) Gappa/kepler1.g
	$(GAPPA) Gappa/kepler2.g
	$(GAPPA) Gappa/himmilbeau.g

extra-gappa-hints:
	$(GAPPA) Gappa/kepler0-hints.g
	$(GAPPA) Gappa/kepler1-hints.g
	$(GAPPA) Gappa/kepler2-hints.g
	$(GAPPA) Gappa/himmilbeau-hints.g

extra-gappa-simple-hints:
	$(GAPPA) Gappa/kepler0-simple-hints.g
	$(GAPPA) Gappa/kepler1-simple-hints.g
	$(GAPPA) Gappa/kepler2-simple-hints.g
	$(GAPPA) Gappa/himmilbeau-simple-hints.g


show:
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

clean:
	rm -rf tmp generated log *.log *~



