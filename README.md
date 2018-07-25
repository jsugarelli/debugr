# debugr

`debugr` is a package designed to support debugging in R. It mainly provides the `dwatch()` function which prints a debug output to the console or to a file. A debug output can consist of a static text message, the values of one or more objects (potentially transformed by applying some functions) or the value of one or multiple (more complex) R expressions. 

Whether or not a debug message is displayed can be made dependent on the evaluation of a criterion phrased as an R expression. Generally, debug messages are only shown if the debug mode is activated. The debug mode is activated and deactivated with `debugr_switchOn()` and `debugr_switchOff()`, respectively, which change the logical `debugr.active` value in the global options. Since debug messages are only displayed in debug mode, the `dwatch()` function calls can even remain in the original code as they remain silent and won't have any effect until the debug mode is switched on again.


## Example

This is a basic example which shows you how to use `dwatch()`. It prints out the value of the `z` object every time `z` exceeds the threshold of 40,000.


``` r

library(debugr)

myfunction <- function(x) {
  justastring <- "Not much information here"
  z <- 1

  for(i in 1:x) {
    # This call can remain in your code; it is only activated when the debug mode is switched on
    dwatch(crit = "z > 40000", objs = c("z"))
    z <- z * i
  }
  invisible(z)
}

# Turn debug mode on
debugr_switchOn()

# Call function for debugging
myfunction(10)

```
