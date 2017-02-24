## Formal Verification of Results

Results of the FPTaylor(b) configuration can be formally verified in
HOL Light.

1) Prepare formal proof certificates:

    make taylor-b-formal
    make extra-taylor-b-formal

   Proof certificates are saved in `./FPTaylor/proofs`.

2) Follow the instructions in
   [../../formal/README.md](../../formal/README.md)
   to install HOL Light and all required formal
   libraries.

3) After loading `../../formal/load.hl` in HOL Light,
   run the following command:

    needs "formal-b.hl";;

   After several hours, results for the standard benchmarks
   will be printed.

4) Now run (in HOL Light)

    needs "formal-b-extra.hl";;

   After several hours, results for the extra benchmarks
   will be printed.