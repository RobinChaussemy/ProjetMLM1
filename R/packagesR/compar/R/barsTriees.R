#' barsTriees
#' une fonction retournant  un barplot triee ggplot.
#' @param data un dataframe
#' @param var une variable qualitative
#' @import ggplot2
#' @import magrittr
#' @import dplyr
#' @return
#' @export
#'
#' @examples
barsTriees <- function(data, var) {
  data |>
    mutate({{ var }} := fct_rev(fct_infreq({{ var }})))  |>
    ggplot(aes(x = {{ var }},fill= {{var}})) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
