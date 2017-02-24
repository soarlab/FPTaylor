FPTAYLOR_HOME=../..

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
FPTAYLOR_A=$(FPTAYLOR) -c config.cfg
FPTAYLOR_C=$(FPTAYLOR) -c config-c.cfg
FPTAYLOR_TRANS_A=$(FPTAYLOR) -c config-trans-a.cfg

tests:
	rm -rf log tmp
	$(FPTAYLOR_A) p1.txt
	$(FPTAYLOR_A) p2.txt
	$(FPTAYLOR_A) p3.txt

tests2:
	rm -rf log tmp
	$(FPTAYLOR_A) point_on_sphere.txt
	$(FPTAYLOR_A) hypot32.txt
	$(FPTAYLOR_A) exp1x.txt
	$(FPTAYLOR_A) sqrt_sub.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-a:
	rm -rf log tmp
	$(FPTAYLOR_A) carbonGas.txt
	$(FPTAYLOR_A) verhulst.txt
	$(FPTAYLOR_A) predatorPrey.txt
	$(FPTAYLOR_A) rigidBody.txt
	$(FPTAYLOR_A) doppler1.txt
	$(FPTAYLOR_A) doppler2.txt
	$(FPTAYLOR_A) doppler3.txt
	$(FPTAYLOR_A) func.txt
	$(FPTAYLOR_A) turbine.txt
	$(FPTAYLOR_A) jet.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-c:
	rm -rf log tmp
	$(FPTAYLOR_C) carbonGas.txt
	$(FPTAYLOR_C) verhulst.txt
	$(FPTAYLOR_C) predatorPrey.txt
	$(FPTAYLOR_C) rigidBody.txt
	$(FPTAYLOR_C) doppler1.txt
	$(FPTAYLOR_C) doppler2.txt
	$(FPTAYLOR_C) doppler3.txt
	$(FPTAYLOR_C) func.txt
	$(FPTAYLOR_C) turbine.txt
	$(FPTAYLOR_C) jet.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

trans-a:
	rm -rf log tmp
	$(FPTAYLOR_TRANS_A) logexp.txt
	$(FPTAYLOR_TRANS_A) point_on_sphere.txt
	$(FPTAYLOR_TRANS_A) greatCircleAzimuth.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

clean:
	rm -rf tmp generated log *.log *~ proofs



