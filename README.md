# ns-3 if77 module 

The ns-3 if77 module provides an implementation of the ns3::PropagationLossModel
which uses the If77 propagation model developed by G. D. Gierhart and M. E. 
Johnson.  The If77 propagation model was developed in the 1970s in order to
estimate the service coverage for radio systems. It can be used to calculate
propagation loss for ground/air, air/air, ground/satellite, and air/satellite
systems for frequencies in the 0.1Mhz to 20Ghz.

More details of the If77 propagation model including its uses and limitations 
can be found in the original If77 [report](lib/docs/Johnson\ and\ Gierhart\ 1978.pdf)

# Building

This documentation assumes that the reader is familiar with building and working 
with the ns-3 codebase.

### Clone if77 module repository

The if77 module is an ns-3 contrib module. Clone the repository under the 
contrib directory of your ns-3 installation.

```bash
$ cd contrib
$ git clone git@github.com:LLNL/ns3-if77-module.git if77 
```

### Build the if77 module

```bash
$ ./waf configure <options>
$ ./waf build
```

# License

The ns-3 if77 module is developed by Lawrence Livermore National Laboratory and is 
distributed under the terms of the GNU Public License (version  2.0).

The if77 library is owned by National Telecommunications and Information 
Administration (NTIA) and is not covered by this license. 
The copy of the if77 library provided with this repository has been modified 
to support calling from C/C++ code.
The original if77 library codebase can be found [here](https://github.com/NTIA/if77-gierhart-johnson)

All contributions to this project must be made under the GPL v2.0 license.

See [LICENSE](LICENSE) and [NOTICE](NOTICE) for details of the ns-3 if77 module
license.
See [LICENSE](lib/LICENSE.md) for details of the NTIA if77 library license. 

SPDX-License-Identifier: GPL-2.0-only

LLNL-CODE-661743
