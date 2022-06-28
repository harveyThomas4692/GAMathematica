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
![importImage]([\main/Import.jpg](https://raw.githubusercontent.com/harveyThomas4692/GAMathematica/main/Import.png))
## Examples

### Integer Factors

## Use

## Citation
If you use this code, please cite the following bib entry:

```
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
```
