FPTAYLOR_HOME=../..

FPTAYLOR=$(FPTAYLOR_HOME)/fptaylor
FPTAYLOR_A=$(FPTAYLOR) -c config-a.cfg
FPTAYLOR_A_NO_SIMPL=$(FPTAYLOR) -c config-a.cfg -c no-simpl.cfg -c iters.cfg
FPTAYLOR_B=$(FPTAYLOR) -c config-b.cfg
FPTAYLOR_C=$(FPTAYLOR) -c config-c.cfg
FPTAYLOR_C_NO_SIMPL=$(FPTAYLOR) -c config-c.cfg -c no-simpl.cfg -c iters.cfg
FPTAYLOR_DEV=$(FPTAYLOR) -c config-dev.cfg
FPTAYLOR_Z3=$(FPTAYLOR) -c config-z3.cfg

taylor-a:
	rm -rf log tmp
	$(FPTAYLOR_A) doppler1.txt
	$(FPTAYLOR_A) doppler2.txt
	$(FPTAYLOR_A) doppler3.txt
	$(FPTAYLOR_A) rigidbody1.txt
	$(FPTAYLOR_A) rigidbody2.txt
	$(FPTAYLOR_A) verhulst.txt
	$(FPTAYLOR_A) carbonGas.txt
	$(FPTAYLOR_A) predPrey.txt
	$(FPTAYLOR_A) kepler0.txt
	$(FPTAYLOR_A) kepler1.txt
	$(FPTAYLOR_A) kepler2.txt
	$(FPTAYLOR_A) sineTaylor.txt
	$(FPTAYLOR_A) sineOrder3.txt
	$(FPTAYLOR_A) sqroot.txt
	$(FPTAYLOR_A) floudas1.txt
	$(FPTAYLOR_A) floudas2.txt
	$(FPTAYLOR_A) floudas3.txt
	$(FPTAYLOR_A) logexp.txt
	$(FPTAYLOR_A) sphere.txt
	$(FPTAYLOR_A) hartman3.txt
# Very slow
#	$(FPTAYLOR_A) -c no-simpl.cfg hartman6.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-a-fp-no-simpl:
	rm -rf log tmp
	$(FPTAYLOR_A_NO_SIMPL) doppler1_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) doppler2_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) doppler3_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) rigidbody1_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) rigidbody2_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) verhulst_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) carbonGas_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) predPrey_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) kepler0_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) kepler1_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) kepler2_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) sineTaylor_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) sineOrder3_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) sqroot_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) floudas1_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) floudas2_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) floudas3_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) logexp_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) sphere_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) hartman3_fp.txt
	$(FPTAYLOR_A_NO_SIMPL) -c iters.cfg hartman6_fp.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;


taylor-b:
	rm -rf log tmp
	$(FPTAYLOR_B) -c approx.cfg doppler1.txt
	$(FPTAYLOR_B) -c approx.cfg doppler2.txt
	$(FPTAYLOR_B) -c approx.cfg doppler3.txt
	$(FPTAYLOR_B) rigidbody1.txt
	$(FPTAYLOR_B) rigidbody2.txt
	$(FPTAYLOR_B) verhulst.txt
	$(FPTAYLOR_B) carbonGas.txt
	$(FPTAYLOR_B) predPrey.txt
	$(FPTAYLOR_B) kepler0.txt
	$(FPTAYLOR_B) kepler1.txt
	$(FPTAYLOR_B) kepler2.txt
	$(FPTAYLOR_B) sineTaylor.txt
	$(FPTAYLOR_B) sineOrder3.txt
	$(FPTAYLOR_B) sqroot.txt
	$(FPTAYLOR_B) floudas1.txt
	$(FPTAYLOR_B) floudas2.txt
	$(FPTAYLOR_B) floudas3.txt
	$(FPTAYLOR_B) logexp.txt
	$(FPTAYLOR_B) sphere.txt
	$(FPTAYLOR_B) hartman3.txt
# Very slow
#	$(FPTAYLOR_B) -c no-simpl.cfg hartman6.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-c:
	rm -rf log tmp
	$(FPTAYLOR_C) doppler1.txt
	$(FPTAYLOR_C) doppler2.txt
	$(FPTAYLOR_C) doppler3.txt
	$(FPTAYLOR_C) rigidbody1.txt
	$(FPTAYLOR_C) rigidbody2.txt
	$(FPTAYLOR_C) verhulst.txt
	$(FPTAYLOR_C) carbonGas.txt
	$(FPTAYLOR_C) predPrey.txt
	$(FPTAYLOR_C) kepler0.txt
	$(FPTAYLOR_C) kepler1.txt
	$(FPTAYLOR_C) kepler2.txt
	$(FPTAYLOR_C) sineTaylor.txt
	$(FPTAYLOR_C) sineOrder3.txt
	$(FPTAYLOR_C) sqroot.txt
	$(FPTAYLOR_C) floudas1.txt
	$(FPTAYLOR_C) floudas2.txt
	$(FPTAYLOR_C) floudas3.txt
	$(FPTAYLOR_C) logexp.txt
	$(FPTAYLOR_C) sphere.txt
	$(FPTAYLOR_C) hartman3.txt
# Very slow
#	$(FPTAYLOR_C) -c no-simpl.cfg hartman6.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-c-no-simpl:
	rm -rf log tmp
	$(FPTAYLOR_C_NO_SIMPL) doppler1.txt
	$(FPTAYLOR_C_NO_SIMPL) doppler2.txt
	$(FPTAYLOR_C_NO_SIMPL) doppler3.txt
	$(FPTAYLOR_C_NO_SIMPL) rigidbody1.txt
	$(FPTAYLOR_C_NO_SIMPL) rigidbody2.txt
	$(FPTAYLOR_C_NO_SIMPL) verhulst.txt
	$(FPTAYLOR_C_NO_SIMPL) carbonGas.txt
	$(FPTAYLOR_C_NO_SIMPL) predPrey.txt
	$(FPTAYLOR_C_NO_SIMPL) kepler0.txt
	$(FPTAYLOR_C_NO_SIMPL) kepler1.txt
	$(FPTAYLOR_C_NO_SIMPL) kepler2.txt
	$(FPTAYLOR_C_NO_SIMPL) sineTaylor.txt
	$(FPTAYLOR_C_NO_SIMPL) sineOrder3.txt
	$(FPTAYLOR_C_NO_SIMPL) sqroot.txt
	$(FPTAYLOR_C_NO_SIMPL) floudas1.txt
	$(FPTAYLOR_C_NO_SIMPL) floudas2.txt
	$(FPTAYLOR_C_NO_SIMPL) floudas3.txt
	$(FPTAYLOR_C_NO_SIMPL) logexp.txt
	$(FPTAYLOR_C_NO_SIMPL) sphere.txt
#	$(FPTAYLOR_C_NO_SIMPL) hartman3.txt
# Very slow
#	$(FPTAYLOR_C) -c no-simpl.cfg hartman6.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-c-fp-no-simpl:
	rm -rf log tmp
	$(FPTAYLOR_C_NO_SIMPL) doppler1_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) doppler2_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) doppler3_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) rigidbody1_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) rigidbody2_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) verhulst_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) carbonGas_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) predPrey_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) kepler0_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) kepler1_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) kepler2_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) sineTaylor_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) sineOrder3_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) sqroot_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) floudas1_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) floudas2_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) floudas3_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) logexp_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) sphere_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) hartman3_fp.txt
	$(FPTAYLOR_C_NO_SIMPL) -c iters.cfg hartman6_fp.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;


taylor-b-fp:
	rm -rf log tmp
	$(FPTAYLOR_B) rigidbody2_fp.txt
	$(FPTAYLOR_B) predPrey_fp.txt
	$(FPTAYLOR_B) kepler0_fp.txt
	$(FPTAYLOR_B) sineOrder3_fp.txt
	$(FPTAYLOR_B) floudas1_fp.txt
	$(FPTAYLOR_B) floudas2_fp.txt
	$(FPTAYLOR_B) floudas3_fp.txt
	$(FPTAYLOR_B) logexp_fp.txt
	$(FPTAYLOR_B) sphere_fp.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-fp-z3:
	rm -rf log tmp
	$(FPTAYLOR_Z3) -c approx.cfg rigidbody2_fp.txt
	$(FPTAYLOR_Z3) predPrey_fp.txt
	$(FPTAYLOR_Z3) -c approx.cfg kepler0_fp.txt
	$(FPTAYLOR_Z3) sineOrder3_fp.txt
	$(FPTAYLOR_Z3) -c approx.cfg floudas1_fp.txt
	$(FPTAYLOR_Z3) floudas2_fp.txt
	$(FPTAYLOR_Z3) floudas3_fp.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;

taylor-dev:
	rm -rf log tmp
	$(FPTAYLOR_DEV) -c approx.cfg doppler1.txt
	$(FPTAYLOR_DEV) -c approx.cfg doppler2.txt
	$(FPTAYLOR_DEV) doppler3.txt
	$(FPTAYLOR_DEV) rigidbody1.txt
	$(FPTAYLOR_DEV) rigidbody2.txt
	$(FPTAYLOR_DEV) verhulst.txt
	$(FPTAYLOR_DEV) carbonGas.txt
	$(FPTAYLOR_DEV) predPrey.txt
	$(FPTAYLOR_DEV) kepler0.txt
	$(FPTAYLOR_DEV) kepler1.txt
	$(FPTAYLOR_DEV) kepler2.txt
	$(FPTAYLOR_DEV) sineTaylor.txt
	$(FPTAYLOR_DEV) sineOrder3.txt
	$(FPTAYLOR_DEV) sqroot.txt
	$(FPTAYLOR_DEV) floudas1.txt
	$(FPTAYLOR_DEV) floudas2.txt
	$(FPTAYLOR_DEV) floudas3.txt
	$(FPTAYLOR_DEV) logexp.txt
	$(FPTAYLOR_DEV) sphere.txt
	$(FPTAYLOR_DEV) hartman3.txt
# Very slow
#	$(FPTAYLOR_DEV) -c no-simpl.cfg hartman6.txt
	find log -name "*.log" -exec awk '/---/,0' '{}' \;


clean:
	rm -rf tmp generated log *.log *~



