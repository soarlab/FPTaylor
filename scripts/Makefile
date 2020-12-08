PYTHON = /usr/bin/env python3

GENERATE = $(PYTHON) generate_tests.py --config tests.cfg --
GENERATE_B = $(PYTHON) generate_tests.py --config tests.cfg plot/config-b.cfg --config-name _tests-b.cfg --test-file _tests-b.yml

RUN = $(PYTHON) run_tests.py

.PHONY: empty generate-basic generate-micro generate-core generate-trans run-all

empty:
	echo "Select target explicitly"

generate-basic:
	$(GENERATE) ../tests/basic
	$(GENERATE_B) ../tests/basic

generate-micro:
	$(GENERATE) ../tests/micro
	$(GENERATE_B) ../tests/micro

generate-core:
	$(GENERATE) ../tests/core
	$(GENERATE_B) ../tests/core

generate-trans:
	$(GENERATE) ../tests/transcendental
	$(GENERATE_B) ../tests/transcendental

run-all:
	$(RUN) -r ../tests


