Formal verification framework for programs targeting REDFIN instruction-set architecture

## What is REDFIN?

REDFIN is an instruction-set architecture and a processing core designed to
be deployed into subsystems of spacecrafts. REDFIN targets space-qualified
Field Programmable Gate Arrays (FPGAs), which have limited resources, and
thus has the following distinguishing features:
* simple instruction set with a small hardware footprint,
* reduced complexity to support formal verification of programs,
* deterministic real-time behaviour.

## Program Verification

This software implements an executable model of REDFIN ISA and a verification framework
for REDFIN programs. The following diagram depicts the modules of the application and their
purpose:

## Building

To build the codebase and run the examples, the following software is required:

* Glasgow Haskell Compiler (GHC), version 8.0.1 or older
* cabal-install --- the Haskell build tool
* Z3 SMT solver

Run `cabal build` to build the code and `cabal run benchmark` to execute the set of examples.
Run `cabal repl` to run GHCi, the Haskell interpreter, and try the examples interactively.
