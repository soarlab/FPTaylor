# Running FPTaylor demo

## Installation

1. Download and install [VirtualBox](https://www.virtualbox.org/).
2. [Download](https://sites.google.com/site/fptaylordemo/download) the virtual machine image with installed FPTaylor.
3. Run VirtualBox and import the downloaded virtual machine: File -> Import Appliance...
4. Run the virtual machine.

    debian login: *guest*

    Password: *guest*

## Running micro benchmarks

    cd FPTaylor/benchmarks/micro
    make taylor2


## Running other benchmarks

    cd FPTaylor/benchmarks/macro
    make taylor-c

Other options:

    make taylor-a
    make taylor-b

You can also try the following commands to see full output:

    make taylor-c > output.txt
    nano output.txt

## Running FPTaylor

You can run FPTaylor on a selected input file or create your own input file and try FPTaylor on it. Here is an example of running FPTaylor on `benchmarks/tests/test01_sum3.txt`:

    cd FPTaylor
    ./fptaylor benchmarks/tests/test01_sum3.txt