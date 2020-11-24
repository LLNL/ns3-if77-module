# ns-3 if77 module 

# Building

### Clone if77 module repository

The if77 module is an ns-3 contrib module. Clone the repository under the 
contrib directory of your ns-3 installation.

```bash
$ cd contrib
$ git clone git@github.com:LLNL/ns3-if77-module.git if77 
```

### Build the if77 module

```bash
$ ./waf configure
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

See [LICENSE](LICENSE) and [NOTICE](NOTICE) for details. 

SPDX-License-Identifier: GPL-2.0-only

LLNL-CODE-661743
