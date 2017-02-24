#!/bin/bash
rm -f *log.txt

make taylor-a                 |& tee fptaylor-a-log.txt
make taylor-b                 |& tee fptaylor-b-log.txt
make taylor-c                 |& tee fptaylor-c-log.txt
make taylor-c-tol             |& tee fptaylor-c-tol-log.txt
make taylor-gelpia            |& tee fptaylor-gelpia-log.txt
make taylor-gelpia-tol        |& tee fptaylor-gelpia-tol-log.txt
make gappa                    |& tee gappa-log.txt
make gappa-hints              |& tee gappa-hints-log.txt
make gappa-simple-hints       |& tee gappa-simple-hints-log.txt
make rosa                     |& tee rosa-log.txt
make rosa-opt                 |& tee rosa-opt-log.txt
make fluctuat                 |& tee fluctuat-log.txt
make fluctuat-subdiv          |& tee fluctuat-subdiv-log.txt


make extra-taylor-a           |& tee extra-fptaylor-a-log.txt
make extra-taylor-b           |& tee extra-fptaylor-b-log.txt
make extra-taylor-c           |& tee extra-fptaylor-c-log.txt
make extra-taylor-c-tol       |& tee extra-fptaylor-c-tol-log.txt
make extra-taylor-gelpia      |& tee extra-fptaylor-gelpia-log.txt
make extra-taylor-gelpia-tol  |& tee extra-fptaylor-gelpia-tol-log.txt
make extra-gappa              |& tee extra-gappa-log.txt
make extra-gappa-hints        |& tee extra-gappa-hints-log.txt
make extra-gappa-simple-hints |& tee extra-gappa-simple-hints-log.txt
make extra-rosa               |& tee extra-rosa-log.txt
make extra-rosa-opt           |& tee extra-rosa-opt-log.txt
make extra-fluctuat           |& tee extra-fluctuat-log.txt
make extra-fluctuat-subdiv    |& tee extra-fluctuat-subdiv-log.txt
