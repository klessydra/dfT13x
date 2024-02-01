<img src="/pics/Klessydra_Logo.png" width="400">

# KLESSYDRA-fT13 PROCESSOR

Intro: The Klessydra processing core family is a set of processors featuring full compliance with RISC-V, and pin-to-pin compatible with the PULPino Riscy cores. Klessydra-T13 is a bare-metal 32-bit processor fully supporting the RV32IM from the RISC-V ISA, and one instruction from the Atomic "A" extension. 'T1' further extends the instruction set with a set of custom vector instructions.

Architecture: fT13 is a Fault Tolerant Implementation of the Klesydra-T13x core, which uses IMT to achieve temporal and partially-spatial redundancy. fT13 is made from T13 core, an interleaved multithreaded processor (Aka, barrel processor) which interleaves three hardware threads (harts) each one with it's own registerfile, CSR-unit, and program counter. 

We have three different threads with the corresponding hardware support for maintaining their critical state signals, thus having a spatial redundancy, and we use them to execute the same code, each 
instruction being interleaved with clock cycle granularity, thus having a temporal redundancy, so fT13 works like a single thread core.

Fencing role of the harts: The harts in our IMT archtiecture play an essential fencing role to avoid pipeline stalls. One role is to fence between registerfile RD & WR accesses, thus never having data-dependency pipeline stalls. The other is to fence between the execution and fetch stage, thus avoiding the need to perform any pipeline flushing. Once the number of harts become less then the required baseline required to create a fence, in that case the data dependency checker and the branch-predictor turn on in order to avoid execution hazards.

The fT13 doesn't support the vector accelerator present in the T13 and S1 until now.

# Merging fT13 User Guide

This guide explains how one can download and install Pulpino, and it's 
modified version of the riscv-gnu toolchain. It also demonstrates
how to patch the offcial riscv-toolchain in order to add the klessydra custom
vector extensions. And then it shows how you can easily merge the Klessydra-Core 
with the Pulpino project.

###########################################################################################
- Prerequisites as indicated by the pulpino group
	- ModelSim in reasonably recent version (we tested it with versions 10.2c)
	- CMake >= 2.8.0, versions greater than 3.1.0 recommended due to support for ninja
	- riscv-toolchain, there are two choices for getting the toolchain: 

  		1) RECOMENDED OPTION: Use the custom version of the RISC-V toolchain from ETH. 
  		The ETH versions supports all the ISA extensions that were incorporated 
	  	into the RI5CY core as well as the reduced base instruction set for zero-riscy.
	        " https://github.com/pulp-platform/ri5cy_gnu_toolchain.git "

		2) Or download the official RISC-V toolchain supported by Berkeley.
 	       	" https://github.com/riscv/riscv-gnu-toolchain "


	  	Please make sure you are using the newlib version of the toolchain.
	- python2 >= 2.6
	
###########################################################################################

- IF you already have pulpino and their own version of the riscv-toolchain, then skip ahead to step.4


PROCEDURE:
1.	Install the following packeges:
		
		sudo apt-get install git cmake python-yaml tcsh autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev

2.	Download and build the "ri5cy_gnu_toolchain"

		a) git clone https://github.com/pulp-platform/ri5cy_gnu_toolchain.git
		
		b) cd ri5cy_gnu_toolchain
		
		c) make ZERORISCY=1

		d) in case you need to build for RISCY cores, then just do "make" instead, and then add the symbolic links as shown from step 4.
		
	When the build is done, add the path **_<path_to_toolchain>/ri5cy_gnu_toolchain/install/bin_** to the environmental variables

3.	Download the PULPino suite:

		a) git clone https://github.com/pulp-platform/pulpino.git
		
		b) cd pulpino
		
		c) ./update-ips.py	


4.	If you want to run the klessydra specific tests, you have to download and patch the official riscv-toolchain, and then build it. Instructions for doing so are included in the README.md file
	inside the folder called "toolchain_files".

5.	To merge the Klessydra core, and tests:

		a) git clone https://github.com/klessydra/fT13x.git
		
		b) cd fT13
		
		c) ./runMErge.sh <pulpino_path>

6.	OPTIONAL: After merging is done, this is how you will be able to test Klessydra-ft1-3th.
		-Open a terminal and navigate to "sw" folder inside pulpino and execute the following commands

		a) e.g. mkdir build
		
		b) cp cmake_configure.klessydra-ft1-3th.gcc.sh build/
		
		c) cd build
		
		d) ./cmake_configure.klessydra-ft1-3th.gcc.sh
		   (Execute the above script twice if you ever change the variable that changes the riscv-compiler, since changing the compiler flushes the values of the variables in the cmake cache and gives an error. Executing the script for a second time after changing the riscv-compiler will let the variables be redfined again)
		   
		e) make vcompile

		For running Klessydra tests; the variable "USE_KLESSYDRA_TEST" in the above shell file is set to '1' by default. You only need to build and run your test
		f) (e.g.  make vect_sum_single_funct_call_all_test_perf.vsimc)
		General tests for all "Txx" versions of Klessydra are also available
		g) (e.g.  make barrier_test.vsimc)
		
		h) You can run one of the PULPino native tests,  (e.g. make testALU.vsimc)
			
	IT"S DONE!!!!!!

Supplimentary Information:

7.	In order to run tests in Modelsim, go to the build folder and do the following:
		make nameofthetest.vsim (or .vsimc to run a test without modelsim GUI)

8. Klessydra-fT13 libraries are available, and their function is described in the software runtime manual fuond in the Docs folder


Hope you like it :D
