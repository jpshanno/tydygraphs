# tydygraphs
_(tie-dye graphs)_

Long time series datasets can be difficult to plot and examine using static
figures. Interactive plots using the
[dygraphs](https://rstudio.github.io/dygraphs/) package are a great tool but are
currently not easily implemented for tidy data. This package provides methods
for `dygraph()` to make it easier to plot tidy time series. Currently there are
methods for dataframes and tibbles, grouped tibbles, and tsibbles. 

## Installation

Currently tydygraphs is only available on GitHub. You can install the current
development version using [remotes](https://remotes.r-lib.org/) or by
downloading and building from source:

``` r
# Install using remotes
remotes::install_github("jpshanno/tydygraphs")

# or from source
tydygraphs_source <- file.path(tempdir(), "tydygraphs-master.zip")
download.file("https://github.com/jpshanno/tydygraphs/archive/master.zip",
              tydygraphs_source)
unzip(tydygraphs_source,
      exdir = dirname(tydygraphs_source))
install.packages(sub(".zip$", "", tydygraphs_source), 
                 repos = NULL,
                 type = "source")

```
## Use
The examples for tydygraphs can be seen at https://jpshanno.github.io/tydygraphs/, 
where the example plots can be interactive.

## Future Plans
- Automatic detection of time columns if `time = NULL`
- Add unit tests
