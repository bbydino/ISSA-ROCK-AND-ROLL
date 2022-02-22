#' myddt
#'
#' Plot LENGTH vs WEIGHT for a given SPECIES of fish from DDT
#' From MATH 4753 Project 1.
#'
#' @param df the "DDT" data frame
#' @param SPECIES the fish species to plot
#'
#' @importFrom utils write.csv
#' @importFrom ggplot2 aes_string geom_point geom_smooth ggplot ggtitle
#' @importFrom dplyr filter
#'
#' @examples
#' myddt(df = ddt, SPECIES = "CCATFISH")
#' myddt(df = ddt, SPECIES = "SMBUFFALO")
#'
#' @export
myddt <- function(df, SPECIES) {
  # specified plot parameters
  cond = df$SPECIES == SPECIES
  x = "LENGTH"
  y = "WEIGHT"
  col = "RIVER"

  ## FILTER: only specified SPECIES of fish
  # df1 <- df %>% filter({{cond}})
  df1 <- df[cond, ]

  ## PLOT:
  # geom_point: points colored according to RIVER variable
  # geom_smooth: quadratic curve placed over the data
  # ggtitle: name on the title
  # use print(g) because output from call inside function
  g <-
    ggplot(df1, aes_string(x = x, y = y)) +
    geom_point(aes_string(color = col)) + #
    geom_smooth(formula = y ~ x + I(x ^ 2), method = "lm") +
    ggtitle("ISSA ROCK AND ROLL", "Daniel Ngo")
  print(g)

  # WRITE DF TO WD: LvsWforSPECIES.csv
  # "SPECIES" string is replaced w/ name of SPECIES used
  names(df1)[3] <- paste(SPECIES)
  write.csv(x = df1,
            file = paste("LvsWfor", SPECIES, ".csv", sep = ""))

  ## PRINT NAMED LIST containing:
  # 1. DDT df before subsetting
  # 2. DDT df after subsetting
  # 3. relative frequency table of RIVER before subsetting
  print(list(df, df1, table(df$RIVER)))
}
