FPTAYLOR_HOME=../..
ROSA_HOME=~/Work/Solvers/rosa
GAPPA_HOME=~/Work/Solvers/gappa-1.1.2/src

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
FPTAYLOR_A=$(FPTAYLOR) -c FPTaylor/config-a.cfg
FPTAYLOR_B=$(FPTAYLOR) -c FPTaylor/config-b.cfg
FPTAYLOR_C=$(FPTAYLOR) -c FPTaylor/config-c.cfg

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

show:
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

clean:
	rm -rf tmp generated log *.log *~



