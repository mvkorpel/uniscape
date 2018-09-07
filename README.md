# uniscape

Find location of single and double quoted strings in
[R](https://www.r-project.org/) code. Convert non-ASCII strings to a portable
format using Unicode escape codes.

The package contains [RStudio](https://www.rstudio.com/) addins for making
portable non-ASCII strings. The core functions also work in plain R, without
RStudio.
    
## Installation

Install [devtools](https://github.com/r-lib/devtools),
then run the following command in the R console:

```R
devtools::install_github("mvkorpel/uniscape")
```

## Usage

After installing the package, RStudio should show some new items in the Browse
Addins menu. The addins can be run from the menu, or keyboard shortcuts can be
created. The addins are implemented with functions `find_strings`,
`string_mutate`, and `u_escape`.