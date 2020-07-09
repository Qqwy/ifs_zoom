# IFSZoom

This is the main source code repository of the Bachelor Project of Wiebe-Marten Wijnja, for the Computing Science bachelor at the University of Groningen.

In this project, code is written to create an interactive program to zoom in (and out) on two-dimensional Iterated Function Systems.


## Technology

The code of this project is written using the programming language Haskell.
We use the Haskell tool _Stack_ for predictable package- and build-management.

### Accelerate

the Haskell library _Accelerate_ is used to write Haskell-style code that will be executed on the GPU.
This execution is done by 

- writing a program in the Accelerate DSL (a subset of 'normal' Haskell code), producing an AST
- transforming this first into LLVM bytecode.
- then into PTX assembly (Nvidia's proprietary GPU/CUDA assembly.)

This approach has the following advantages over writing e.g. CUDA-code by hand:

- All code is type-checked, making whole classes of bugs impossible.
- ML-style type-checking is very flexible, allowing for highly re-usable code.
- By manipulating AST we can program at a high level and then e.g. unroll repeated procedures to create GPU-targetted code that would be impossible to write/maintain by hand.

#### Backends

Accelerate supports running the same program on the GPU, on the (multithreaded) CPU and on a serial 'interpreter' back-end, making testing easier.

## Getting started

To build this project:

1. Install `stack` [installation instructions here](https://docs.haskellstack.org/en/stable/README/).
2. Install LLVM, version 8.0. [See here](http://llvm.org/). Required to turn Accelerate AST -> LLVM. _(The current version of Accelerate does not yet work with LLVM 9 or 10.)_
3. Install the Nvidia CUDA toolkit applicable for your GPU [See here](https://developer.nvidia.com/cuda-downloads). We require `ncc` to perform the compilation of LLVM -> PTX.
4. Install [LibFFI](https://sourceware.org/libffi/) for the LLVM->CPU-target.
5. In the repository folder, run `stack build`.
6. Wait for a long time; Stack will first install the correct version of GHC (the Haskell compiler) and then install all required packages with their dependencies, as well as certain native dependencies. This might take, say 45 minutes to complete the first time. 
7. Compilation of only the project code after this initial build will be much faster, only taking a couple of seconds.
8. Done.
  - Use `stack run` to run the main application binary. 
  - Use `stack ghci` to run an interactive shell in which the modules the binary contains are in scope to test/explore indiual parts.
  - TODO automated testing

## How to run

The program expects an IFS configuration as input on STDIN. Some example files can be found in the `./examples` folder.
Possible usage is thus:

```
stack run < ./examples/barnsley.ifs
# or:
cat ./examples/barnsley.ifs | stack run
# or:
cat ./examples/barnsley.ifs | stack run -- --maybe --some="options"
```
