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