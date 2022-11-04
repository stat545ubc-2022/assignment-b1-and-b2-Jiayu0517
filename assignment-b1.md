Assignment B-1
================

``` r
library("dplyr")
library("testthat")
```

## Exercise 1: Implement the function

## Exercise 2: Document the function

``` r
#' Count Numbers of Integer Digits of a Number
#'
#' @param data A numeric value.
#' @param na.rm A logical input indicating whether missing values should be removed before the computation proceeds.
#' @param verbose A logical input indicating whether keeps the user up-to-date with messages that print to screen as the function is running.
#' @param ... Other arguments passed on to methods.
#'
#' @return
#' A value indicating the numbers of integer digits of input.
#' @export
#'
#' @examples
#' DigitCounter_single(43458.99, verbose=TRUE)
#' DigitCounter_single(0.99, verbose=TRUE)
DigitCounter_single <- function(data, na.rm = TRUE, verbose = FALSE, ...) {
  if(na.rm) data <- na.omit(data)
  if(!is.numeric(data)) {
    stop('I am so sorry, but this function only works for numeric input!\n',
         'You have provided an object of class: ', class(data)[1])
  }
  count <- 0
  x_tester <- data
  if(length(x_tester) == 0) {
    stop('I am so sorry, but this function only works for nonempty input!\n')
  }
  if(abs(x_tester) < 1) {
    if(verbose) cat("Input numberical value", data, "is less than 1\n")
    return(count)
  }
  else count <- count + 1
  while (log10(abs(x_tester)) >= 1) {
    x_tester <- x_tester/10
    if(verbose) cat("x divided by 10, Times:", count,"and x becomes:", x_tester, "\n")
    count <- count + 1
  }
  return(count)
}
```

``` r
#' Count Numbers of Individual Integer Digits in a Vector
#'
#' @param x A numeric vector.
#' @param na.rm A logical input indicating whether missing values should be removed before the computation proceeds.
#' @param verbose A logical input indicating whether keeps the user up-to-date with messages that print to screen as the function is running.
#' @param ... Other arguments passed on to methods.
#'
#' @return
#' A vector containing the numbers of integer digits of each number in input.
#' @export
#' @importFrom "stats" "na.omit"
#'
#' @examples
#' IntDigitsCounter(c(0.123, 1, -245, 4308.99), na.rm = FALSE)
IntDigitsCounter <- function(x, na.rm = TRUE, verbose = FALSE, ...) {
  if(na.rm) x <- na.omit(x)  # remove missing value if na.rm is TRUE
  purrr::map_dbl(x, ~ DigitCounter_single(.x, na.rm = na.rm, verbose = verbose))
}
```

## Exercise 3: Include examples

``` r
DigitCounter_single(43458.99, verbose=TRUE)
```

    ## x divided by 10, Times: 1 and x becomes: 4345.899 
    ## x divided by 10, Times: 2 and x becomes: 434.5899 
    ## x divided by 10, Times: 3 and x becomes: 43.45899 
    ## x divided by 10, Times: 4 and x becomes: 4.345899

    ## [1] 5

``` r
DigitCounter_single(0.99, verbose=TRUE)
```

    ## Input numberical value 0.99 is less than 1

    ## [1] 0

``` r
IntDigitsCounter(c(0.123, 1, -245, 4308.99), na.rm = FALSE)
```

    ## [1] 0 1 3 4

## Exercise 4: Test the function

``` r
test_that("numeric vectors work", {
  # Check input with NA with na.rm = FALSE
  expect_error(IntDigitsCounter(c(2, 0, NA, 43478.99), na.rm = FALSE))
  # Check input with NA with na.rm = TRUE
  expect_equal(IntDigitsCounter(c(2, 0, NA, 43478.99), na.rm = TRUE), c(1, 0 ,5))
  # Check whether vector with negative numbers and non-integer values can work(without NA)
  expect_equal(IntDigitsCounter(c(0.123, 1, -245, 4308.99)), c(0, 1, 3, 4))
})
```

    ## Test passed 🥳

``` r
test_that("list inputs work", {
  # Check whether list of numerical numbers can work
  expect_equal(IntDigitsCounter(list(0.99, 9.99, 99.99)), c(0, 1, 2))
  # Check whether list of numerical numbers with negative numbers and non-integer values can work
  expect_equal(IntDigitsCounter(list(0.123, 1, -245, 4308.99)), c(0, 1, 3, 4))
})
```

    ## Test passed 😸

``` r
test_that("Logicals and character vectors don't work", {
  # Check character input couldn't work
  expect_error(IntDigitsCounter(c("hi","there")))
  # Check logical input couldn't work
  expect_error(IntDigitsCounter(c(TRUE, FALSE)))
})
```

    ## Test passed 🎊

``` r
test_that("check output produced by verbose", {
  expect_output(IntDigitsCounter(245, verbose = TRUE), "x divided by 10, Times: 1 and x becomes: 24.5 \nx divided by 10, Times: 2 and x becomes: 2.45 ")
})
```

    ## Test passed 😀
