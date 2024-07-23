
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crewargs

<!-- badges: start -->
<!-- badges: end -->

{crewargs} provides an extention to `crew::crew_class_launcher_local`
and its associated controller `crew::crew_launcher_local_args()` to
allow passing command line arguments to the R sessions that the workers
use.

The motivating use case was to be able to set the maximum number of
connections (`--max-connections=`) that workers may use.

## Installation

You can install the development version of crewargs like so:

``` r
devtools::install_github("rpruim/crewargs")
```

## Example

``` r
library(crewargs)
#> Loading required package: crew
```

``` r
library(targets)
```

``` r
# save this to _targets_example.R
library(targets)
library(crewargs)

tar_option_set(
  packages = c("crewargs"), 
  controller = 
    crewargs_controller_local_args(
      workers = 2, seconds_idle = 60, 
      cmdargs = "--max-connections=321"
    )
)

list(
  tar_target(
    name = connections, command = parallelly::availableConnections()
  )
)
```

``` r
tar_make(
  script = "_targets_example.R",
  callr_arguments = list(
    'cmdargs' = c("--save", "--no-save", "--no-restore", "--max-connections=400")
  )
)
```

The number of connections available to workers (321) is determined by
`cmdargs` passed to the controller, not by `cmdargs` passed to
`callr::r()`.

``` r
tar_read(connections)
#> [1] 321
```