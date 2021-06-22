
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slate

<!-- badges: start -->
<!-- badges: end -->

A logging library that utilizes Redis as the backend.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kevinykuo/slate")
```

## Example

In this example, suppose that there is a Redis server running on socket
`/tmp/redis.sock`. We can instantiate a `slate` object using `slate()`:

``` r
library(slate)
sl <- slate(path = "/tmp/redis.sock", default_app_id = "my_app")
```

We can then `etch()` the slate as follows:

``` r
etch_info(sl, "This is an INFO")
etch_warn(sl, "This is a WARN", app_id = "some_other_app")
etch_error(sl, "This is an ERROR", some_field = "some_value")
```

We can inspect the slate using the `peruse()` function:

``` r
peruse(sl)
#> 2021-06-22 11:42:11.447 ERROR [my_app] This is an ERROR some_field=some_value
#> 2021-06-22 11:42:11.447 WARN [some_other_app] This is a WARN 
#> 2021-06-22 11:42:11.444 INFO [my_app] This is an INFO
```

For real time monitoring, the `gaze()` function prints new entries to
the console as they arrive.
