# Simplex Algorithm in R

Project page <https://dirkdegel.github.io/simplexR>.

The `simplexR` package was developed with the intention to be used for educational purposes. The idea is to allow students to solve
linear programs with the **Simplex Method** in a similar way it is thought in most courses and text books (see literature below). Dedicated functions allow the user to apply the simplex step by step and be able to verify their own calculations. The aim is to allow a more comprehensive understanding of the **Simplex Algorithm** (primal simplex algorithm, bigM-method, two-phase simplex algorithm, revised simplex algorithm, dual simplex algorithm).

Additionally, the implementation in `R` is meant to provide an opportunity for users to get started with programming via an algorithm they are well familiar with.

The Wikipedia articles gives a good starting point if you would like to learn more about [linear programming](https://en.wikipedia.org/wiki/Linear_programming) and the [simplex algorithm](https://en.wikipedia.org/wiki/Simplex_algorithm) (simplex method).

I'm always happy to get bug reports or feedback.

## Install

### CRAN

Not available yet.

### Development Version

To install the current development version use devtools:

```R
library(devtools)
install_github("dirkdegel/simplexR")
```

## Project Page

Project page <https://dirkdegel.github.io/simplexR>.

## License

Currently CC0.

## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

## Contributing

Please post an issue first before sending a PR.

## Literature

- Linear Programming 1: Introduction, George B. Dantzig, Mukund N. Thapa
- Linear Programming 2: Theory and Extensions, George B. Dantzig, Mukund N. Thapa
- Algorithmische Diskrete Mathematik II: Lineare Optimierung, [Vorlesungsskript](http://www.zib.de/groetschel/teaching/skriptADMII.pdf), Martin Grötschel
- Grundlagen des Operations Research: Mit Aufgaben und Lösungen, Brigitte Werners
- Operations Research - Deterministische Modelle und Methoden - Stephan Dempe, Heiner Schreier, 1. Auflage September 2006, Wiesbaden

## Related Projects

[linprog](https://cran.r-project.org/web/packages/linprog/index.html) follows a similar approach, but it seems the package is not maintained anymore.

See CRAN Task View: [Optimization and Mathematical Programming]( https://cran.r-project.org/web/views/Optimization.html) for further information.

## Industrial Solutions

[optimization_software](https://en.wikipedia.org/wiki/List_of_optimization_software)

## Open Source Solver

- Coin OR
- GLPK
- lp_splve

## Commercial Solver

- Gurobi
- Cplex
- Xpress
- Scip
