# GAMathematica
Latest version of code: 9th June 2022.

This repository contains the code for a Genetic Algorithm (GA), 
implemented in Mathematica. This was intended for use in the paper:
https://doi.org/10.1002/prop.202200034.

## Installation

It's possible to use the package without installing, if you have access to the 
internet. Simply run
```
Get["https://raw.githubusercontent.com/harveyThomas4692/GAMathematica/\
main/Genetic.m"]
```
in the Mathematica Kernel.

Alternatively, if you wish to use the package without internet access, you will need
to install it. To install the package, please copy "Genetic.m" into your 
Mathematica "Applications" folder. This can be found in 
"Home_Directory/.Mathematica/Applications", where your home directory can
be found by running
```
$HomeDirectory

```
in the Mathematica kernel. The package can then be imported by running the line
```
Get["Genetic`"]
```

In either case, this has correctly imported the package if the following 
appears
![importImage](https://raw.githubusercontent.com/harveyThomas4692/GAMathematica/main/Import.png)

## Examples
There is currently one example notebook that can be found in the examples folder.
This example demonstrated how GA can be used to find factors of integers.

### Integer Factors
This simple example uses a this GA package to factorise a specified
integer. This example may also be useful to some in demonstrating 
how one can connect custom functions to this package.

### Monad Bundles (Code Not available)
This code has been used to find realistic string vacua from Heterotic
String theory via Monad Bundles on smooth Calabi-Yau manifolds. All information
on this can be found on SPIRES: https://inspirehep.net/literature/1953720

### Building Potentials for Cosmic Inflation (Code Not available)
The code has been used to construct models of cosmic inflation by tuning the 
parameters of the potential to satisfy the constraints from observation. Several
different types of inflation are considered. All information can be found on
SPIRES: https://inspirehep.net/literature/2143723

## Use
Documentation is built into the package. This can be accessed by running the
following lines in Mathematica:
```
?Genetic
```
```
?GeneticOptions
```
```
?GeneticModules
```
Similarly, any function listed in this last command has their own documentation.
This can be found by running 
```
?"FunctionName"
```
I also suggest looking at the examples, as these demonstrate how one can connect
custom functions to this package.

## Citation
If you use this code, please cite the following bib entries:

```
@article{GApackage,
  author = "Abel, Steven and Constantin, Andrei and Harvey, Thomas R. and Lukas, Andre",
  title = "{Mathematica package for Genetic Algorithms}",
  url="https://github.com/harveyThomas4692/GAMathematica",
  note="https://github.com/harveyThomas4692/GAMathematica"
}

@article{Abel:2021rrj,
    author = "Abel, Steven and Constantin, Andrei and Harvey, Thomas R. and Lukas, Andre",
    title = "{Evolving Heterotic Gauge Backgrounds: Genetic Algorithms versus Reinforcement Learning}",
    eprint = "2110.14029",
    archivePrefix = "arXiv",
    primaryClass = "hep-th",
    doi = "10.1002/prop.202200034",
    journal = "Fortsch. Phys.",
    volume = "70",
    number = "5",
    pages = "2200034",
    year = "2022"
}

@article{Abel:2022nje,
    author = "Abel, Steven and Constantin, Andrei and Harvey, Thomas R. and Lukas, Andre",
    title = "{Cosmic Inflation and Genetic Algorithms}",
    eprint = "2208.13804",
    archivePrefix = "arXiv",
    primaryClass = "hep-th",
    reportNumber = "IPPP/22/64",
    month = "8",
    year = "2022"
}
```
