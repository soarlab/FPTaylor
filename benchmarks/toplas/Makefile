FPTAYLOR_HOME=../..
ROSA_HOME=~/TOPLAS/ROSA/rosa-master
ROSA_OPT_HOME=~/TOPLAS/ROSA/rosa-opt
GAPPA_HOME=~/TOPLAS/GAPPA/gappa-1.3.0/src
GELPIA_HOME=~/TOPLAS/FPTAYLOR/FPTaylor/gelpia
FLUCTUAT_HOME=~/TOPLAS/FLUCTUAT/fluctuat_v3.1376

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
FPTAYLOR_A=$(FPTAYLOR) -c FPTaylor/config-a.cfg
FPTAYLOR_B=$(FPTAYLOR) -c FPTaylor/config-b.cfg
FPTAYLOR_B_VARS=$(FPTAYLOR) -c FPTaylor/config-b.cfg --const-approx-real-vars false
FPTAYLOR_B_PROOF=$(FPTAYLOR) -c FPTaylor/config-b.cfg --proof-record true --proof-dir FPTaylor/proofs
FPTAYLOR_C=$(FPTAYLOR) -c FPTaylor/config-c.cfg
FPTAYLOR_C_TOL=$(FPTAYLOR) -c FPTaylor/config-c-tol.cfg
FPTAYLOR_Z3=$(FPTAYLOR) -c FPTaylor/config-z3.cfg
FPTAYLOR_GELPIA=$(FPTAYLOR) -c FPTaylor/config-c.cfg --opt gelpia --opt-timeout 60000
FPTAYLOR_GELPIA_TOL=$(FPTAYLOR) -c FPTaylor/config-c-tol.cfg --opt gelpia --opt-timeout 60000

FPTAYLOR_REL_B=$(FPTAYLOR_B) -abs false -rel true
FPTAYLOR_REL_B_VARS=$(FPTAYLOR_B_VARS) -abs false -rel true
FPTAYLOR_REL_C=$(FPTAYLOR_C) -abs false -rel true
FPTAYLOR_REL_C_TOL=$(FPTAYLOR_C_TOL) -abs false -rel true
FPTAYLOR_REL_Z3=$(FPTAYLOR_Z3) -abs false -rel true

ROSA=$(ROSA_HOME)/rosa --real --z3Timeout=1000
ROSA_OPT=$(ROSA_OPT_HOME)/rosa --real --z3Timeout=1000

GAPPA=$(GAPPA_HOME)/gappa

FLUCTUAT=cd Fluctuat && time ./fluctuat_benchmark.sh ${FLUCTUAT_HOME}
FLUCTUAT_SUBDIV=cd FluctuatSubdiv && time ./fluctuat_benchmark.sh ${FLUCTUAT_HOME}


taylor-a:
	rm -rf log tmp
	# Non-Transcendental alphabetically
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
	# Transcendental alphabetically
	time $(FPTAYLOR_A) FPTaylor/azimuth.txt
	time $(FPTAYLOR_A) FPTaylor/logexp.txt
	time $(FPTAYLOR_A) FPTaylor/sphere.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-b:
	rm -rf log tmp
	# Non-Transcendental alphabetically
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
	# Transcendental alphabetically
	time $(FPTAYLOR_B) FPTaylor/azimuth.txt
	time $(FPTAYLOR_B) FPTaylor/logexp.txt
	time $(FPTAYLOR_B) FPTaylor/sphere.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-b-formal:
	rm -rf log tmp
	# Non-Transcendental alphabetically
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
	# Transcendental alphabetically
	time $(FPTAYLOR_B_PROOF) FPTaylor/azimuth.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/logexp.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/sphere.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-b-vars:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_B_VARS) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/doppler1.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/doppler2.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/doppler3.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/jet.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/rigidBody1.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/rigidBody2.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/sine.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/sineOrder3.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/sqroot.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/t_div_t1.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/turbine1.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/turbine2.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/turbine3.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/verhulst.txt
	# Transcendental alphabetically
	time $(FPTAYLOR_B_VARS) FPTaylor/azimuth.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/logexp.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/sphere.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;
	
taylor-c:
	rm -rf log tmp
	# Non-Transcendental alphabetically
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
	# Transcendental alphabetically
	time $(FPTAYLOR_C) FPTaylor/azimuth.txt
	time $(FPTAYLOR_C) FPTaylor/logexp.txt
	time $(FPTAYLOR_C) FPTaylor/sphere.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-c-tol:
	rm -rf log tmp
	# Non-Transcendental alphabetically
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
	# Transcendental alphabetically
	time $(FPTAYLOR_C_TOL) FPTaylor/azimuth.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/logexp.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/sphere.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-z3:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_Z3) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_Z3) FPTaylor/doppler1.txt
	time $(FPTAYLOR_Z3) FPTaylor/doppler2.txt
	time $(FPTAYLOR_Z3) FPTaylor/doppler3.txt
	time $(FPTAYLOR_Z3) FPTaylor/jet.txt
	time $(FPTAYLOR_Z3) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_Z3) FPTaylor/rigidBody1.txt
	time $(FPTAYLOR_Z3) FPTaylor/rigidBody2.txt
	time $(FPTAYLOR_Z3) FPTaylor/sine.txt
	time $(FPTAYLOR_Z3) FPTaylor/sineOrder3.txt
	time $(FPTAYLOR_Z3) FPTaylor/sqroot.txt
	time $(FPTAYLOR_Z3) FPTaylor/t_div_t1.txt
	time $(FPTAYLOR_Z3) FPTaylor/turbine1.txt
	time $(FPTAYLOR_Z3) FPTaylor/turbine2.txt
	time $(FPTAYLOR_Z3) FPTaylor/turbine3.txt
	time $(FPTAYLOR_Z3) FPTaylor/verhulst.txt
	# extra
	time $(FPTAYLOR_Z3) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_Z3) FPTaylor/kepler0.txt
	time $(FPTAYLOR_Z3) FPTaylor/kepler1.txt
	time $(FPTAYLOR_Z3) FPTaylor/kepler2.txt
	# floudas
	time $(FPTAYLOR_Z3) FPTaylor/floudas1.txt
	time $(FPTAYLOR_Z3) FPTaylor/floudas2.txt
	time $(FPTAYLOR_Z3) FPTaylor/floudas3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-gelpia:
	rm -rf log tmp
	export GELPIA_PATH=$(GELPIA_HOME)
	export GELPIA=$(GELPIA_HOME)/bin/gelpia
	# Non-Transcendental alphabetically
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
	# Transcendental alphabetically
	time $(FPTAYLOR_GELPIA) FPTaylor/azimuth.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/logexp.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/sphere.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-gelpia-tol:
	rm -rf log tmp
	export GELPIA_PATH=$(GELPIA_HOME)
	export GELPIA=$(GELPIA_HOME)/bin/gelpia
	# Non-Transcendental alphabetically
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
	# Transcendental alphabetically
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/azimuth.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/logexp.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/sphere.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

rel-taylor-b:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_REL_B) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_REL_B) FPTaylor/doppler1.txt
	time $(FPTAYLOR_REL_B) FPTaylor/doppler2.txt
	time $(FPTAYLOR_REL_B) FPTaylor/doppler3.txt
	time $(FPTAYLOR_REL_B) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_REL_B) FPTaylor/sqroot.txt
	time $(FPTAYLOR_REL_B) FPTaylor/turbine1.txt
	time $(FPTAYLOR_REL_B) FPTaylor/turbine3.txt
	time $(FPTAYLOR_REL_B) FPTaylor/verhulst.txt
	time $(FPTAYLOR_REL_B) FPTaylor/kepler0.txt
	time $(FPTAYLOR_REL_B) FPTaylor/kepler1.txt
	time $(FPTAYLOR_REL_B) FPTaylor/kepler2.txt

rel-taylor-b-vars:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/doppler1.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/doppler2.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/doppler3.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/sqroot.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/turbine1.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/turbine3.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/verhulst.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/kepler0.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/kepler1.txt
	time $(FPTAYLOR_REL_B_VARS) FPTaylor/kepler2.txt

rel-taylor-c:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_REL_C) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_REL_C) FPTaylor/doppler1.txt
	time $(FPTAYLOR_REL_C) FPTaylor/doppler2.txt
	time $(FPTAYLOR_REL_C) FPTaylor/doppler3.txt
	time $(FPTAYLOR_REL_C) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_REL_C) FPTaylor/sqroot.txt
	time $(FPTAYLOR_REL_C) FPTaylor/turbine1.txt
	time $(FPTAYLOR_REL_C) FPTaylor/turbine3.txt
	time $(FPTAYLOR_REL_C) FPTaylor/verhulst.txt
	time $(FPTAYLOR_REL_C) FPTaylor/kepler0.txt
	time $(FPTAYLOR_REL_C) FPTaylor/kepler1.txt
	time $(FPTAYLOR_REL_C) FPTaylor/kepler2.txt

rel-taylor-c-tol:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/doppler1.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/doppler2.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/doppler3.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/sqroot.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/turbine1.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/turbine3.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/verhulst.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/kepler0.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/kepler1.txt
	time $(FPTAYLOR_REL_C_TOL) FPTaylor/kepler2.txt

rel-taylor-z3:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_REL_Z3) FPTaylor/carbonGas.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/doppler1.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/doppler2.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/doppler3.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/predatorPrey.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/sqroot.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/turbine1.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/turbine3.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/verhulst.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/kepler0.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/kepler1.txt
	time $(FPTAYLOR_REL_Z3) FPTaylor/kepler2.txt

extra-taylor-a:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_A) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_A) FPTaylor/kepler0.txt
	time $(FPTAYLOR_A) FPTaylor/kepler1.txt
	time $(FPTAYLOR_A) FPTaylor/kepler2.txt
	# Transcendental alphabetically
	time $(FPTAYLOR_A) FPTaylor/hartman3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-b:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_B) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_B) FPTaylor/kepler0.txt
	time $(FPTAYLOR_B) FPTaylor/kepler1.txt
	time $(FPTAYLOR_B) FPTaylor/kepler2.txt
	# Transcendental alphabetically
	time $(FPTAYLOR_B) FPTaylor/hartman3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-b-vars:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_B_VARS) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/kepler0.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/kepler1.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/kepler2.txt
	# Transcendental alphabetically
	time $(FPTAYLOR_B_VARS) FPTaylor/hartman3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-b-formal:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_B_PROOF) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/kepler0.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/kepler1.txt
	time $(FPTAYLOR_B_PROOF) FPTaylor/kepler2.txt
	# Transcendental alphabetically
	time $(FPTAYLOR_B_PROOF) FPTaylor/hartman3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-c:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_C) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_C) FPTaylor/kepler0.txt
	time $(FPTAYLOR_C) FPTaylor/kepler1.txt
	time $(FPTAYLOR_C) FPTaylor/kepler2.txt
	# Transcendental alphabetically
	time $(FPTAYLOR_C) FPTaylor/hartman3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-c-tol:
	rm -rf log tmp
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_C_TOL) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/kepler0.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/kepler1.txt
	time $(FPTAYLOR_C_TOL) FPTaylor/kepler2.txt
	# Transcendental alphabetically
	time $(FPTAYLOR_C_TOL) FPTaylor/hartman3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-gelpia:
	rm -rf log tmp
	export GELPIA_PATH=$(GELPIA_HOME)
	export GELPIA=$(GELPIA_HOME)/bin/gelpia
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_GELPIA) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/kepler0.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/kepler1.txt
	time $(FPTAYLOR_GELPIA) FPTaylor/kepler2.txt
	# Transcendental alphabetically
	time $(FPTAYLOR_GELPIA) FPTaylor/hartman3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

extra-taylor-gelpia-tol:
	rm -rf log tmp
	export GELPIA_PATH=$(GELPIA_HOME)
	export GELPIA=$(GELPIA_HOME)/bin/gelpia
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/himmilbeau.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/kepler0.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/kepler1.txt
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/kepler2.txt
	# Non-Transcendental alphabetically
	time $(FPTAYLOR_GELPIA_TOL) FPTaylor/hartman3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

floudas-taylor-b:
	rm -rf log tmp
	time $(FPTAYLOR_B) FPTaylor/floudas1.txt
	time $(FPTAYLOR_B) FPTaylor/floudas2.txt
	time $(FPTAYLOR_B) FPTaylor/floudas3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;	

floudas-taylor-b-vars:
	rm -rf log tmp
	time $(FPTAYLOR_B_VARS) FPTaylor/floudas1.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/floudas2.txt
	time $(FPTAYLOR_B_VARS) FPTaylor/floudas3.txt
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;	

gappa:
	# Non-Transcendental alphabetically
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
	# Non-Transcendental alphabetically
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
	# Non-Transcendental alphabetically
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
	# Non-Transcendental alphabetically
	time $(GAPPA) Gappa/himmilbeau.g
	time $(GAPPA) Gappa/kepler0.g
	time $(GAPPA) Gappa/kepler1.g
	time $(GAPPA) Gappa/kepler2.g

extra-gappa-hints:
	# Non-Transcendental alphabetically
	time $(GAPPA) Gappa/himmilbeau-hints.g
	time $(GAPPA) Gappa/kepler0-hints.g
	time $(GAPPA) Gappa/kepler1-hints.g
	time $(GAPPA) Gappa/kepler2-hints.g

extra-gappa-simple-hints:
	# Non-Transcendental alphabetically
	time $(GAPPA) Gappa/himmilbeau-simple-hints.g
	time $(GAPPA) Gappa/kepler0-simple-hints.g
	time $(GAPPA) Gappa/kepler1-simple-hints.g
	time $(GAPPA) Gappa/kepler2-simple-hints.g

rosa:
	$(RM) -r vcs generated
	mkdir vcs generated
	export LD_LIBRARY_PATH=${ROSA_HOME}/lib
	# Non-Transcendental alphabetically
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
	$(RM) -r vcs generated
	mkdir vcs generated
	export LD_LIBRARY_PATH=${ROSA_HOME}/lib
	# Non-Transcendental alphabetically
	time $(ROSA) --functions=himmilbeau Rosa/rosa.scala
	time $(ROSA) --functions=kepler0 Rosa/rosa.scala
	time $(ROSA) --functions=kepler1 Rosa/rosa.scala
	time $(ROSA) --functions=kepler2 Rosa/rosa.scala

rosa-opt:
	$(RM) -r vcs generated
	mkdir vcs generated
	export LD_LIBRARY_PATH=${ROSA_OPT_HOME}/lib
	# Non-Transcendental alphabetically
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
	$(RM) -r vcs generated
	mkdir vcs generated
	export LD_LIBRARY_PATH=${ROSA_OPT_HOME}/lib
	# Non-Transcendental alphabetically
	time $(ROSA_OPT) --functions=himmilbeau Rosa/rosa.scala
	time $(ROSA_OPT) --functions=kepler0 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=kepler1 Rosa/rosa.scala
	time $(ROSA_OPT) --functions=kepler2 Rosa/rosa.scala

fluctuat:
	# Non-Transcendental alphabetically
	$(FLUCTUAT) carbonGas.c
	$(FLUCTUAT) doppler1.c
	$(FLUCTUAT) doppler2.c
	$(FLUCTUAT) doppler3.c
	$(FLUCTUAT) jet.c
	$(FLUCTUAT) predatorPrey.c
	$(FLUCTUAT) rigidBody1.c
	$(FLUCTUAT) rigidBody2.c
	$(FLUCTUAT) sine.c
	$(FLUCTUAT) sineOrder3.c
	$(FLUCTUAT) sqroot.c
	$(FLUCTUAT) t_div_t1.c
	$(FLUCTUAT) turbine1.c
	$(FLUCTUAT) turbine2.c
	$(FLUCTUAT) turbine3.c
	$(FLUCTUAT) verhulst.c

extra-fluctuat:
	# Non-Transcendental alphabetically
	$(FLUCTUAT) himmilbeau.c
	$(FLUCTUAT) kepler0.c
	$(FLUCTUAT) kepler1.c
	$(FLUCTUAT) kepler2.c

fluctuat-subdiv:
	# Non-Transcendental alphabetically
	$(FLUCTUAT_SUBDIV) carbonGas.c
	$(FLUCTUAT_SUBDIV) doppler1.c
	$(FLUCTUAT_SUBDIV) doppler2.c
	$(FLUCTUAT_SUBDIV) doppler3.c
	$(FLUCTUAT_SUBDIV) jet.c
	$(FLUCTUAT_SUBDIV) predatorPrey.c
	$(FLUCTUAT_SUBDIV) rigidBody1.c
	$(FLUCTUAT_SUBDIV) rigidBody2.c
	$(FLUCTUAT_SUBDIV) sine.c
	$(FLUCTUAT_SUBDIV) sineOrder3.c
	$(FLUCTUAT_SUBDIV) sqroot.c
	$(FLUCTUAT_SUBDIV) t_div_t1.c
	$(FLUCTUAT_SUBDIV) turbine1.c
	$(FLUCTUAT_SUBDIV) turbine2.c
	$(FLUCTUAT_SUBDIV) turbine3.c
	$(FLUCTUAT_SUBDIV) verhulst.c

extra-fluctuat-subdiv:
	# Non_Transcendental alphabetically
	$(FLUCTUAT_SUBDIV) himmilbeau.c
	#$(FLUCTUAT_SUBDIV) kepler0.c
	$(FLUCTUAT_SUBDIV) kepler1.c
	$(FLUCTUAT_SUBDIV) kepler2.c


show:
	#find log -name "*.log" -exec awk '/---/,0' '{}' \;

clean:
	$(RM) -r tmp generated log *.log *~ vcs generated
	$(RM) Fluctuat/*.ii Fluctuat/console.eout Fluctuat/ianlog
	$(RM) -r Fluctuat/info_iterations.out Fluctuat/logfile Fluctuat/output
	$(RM) FluctuatSubdiv/*.ii FluctuatSubdiv/console.eout
	$(RM) FluctuatSubdiv/ianlog FluctuatSubdiv/info_iterations.out
	$(RM) -r FluctuatSubdiv/logfile FluctuatSubdiv/output



