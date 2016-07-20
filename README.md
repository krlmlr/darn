# darn

The `darn` package allows the definition of Directed Acyclic R script Networks.
A DARN is a collection of R scripts where each can have a (possibly empty)
set of prerequisites (other scripts that must be run before, and whose output is processed further).
Each R script also defines a (possibly empty) set of R objects
which are exported for use by the dependent scripts.
Unlike `remake` and other approaches, the dependencies are declared directly in the R scripts.
By design, each script can also be run individually, unchanged.
This makes it particularly easy to develop data preparation and analysis scripts
interactively, and then effortlessly integrate them in the script network
without worrying too much about interdependencies.

Each R script declares its prerequisites via a call to `darn::init()`,
and its outputs via a call to `darn::done()`;
the latter is mandatory to make the script part of the DARN.
A `Makefile` can be created from these implicit dependencies.
Running `make` only runs those scripts that are newer than their outputs,
i.e., only does as much as necessary to build the entire project.
New, modified or deleted scripts are detected,
and the dependencies are updated accordingly.


## Installation

You can install darn from github with:

```R
# install.packages("devtools")
devtools::install_github("krlmlr/darn")
```

## Example

See the "Walkthrough" vignette.
