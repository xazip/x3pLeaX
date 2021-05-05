#' Convert labels to numeric to be used for keras one-hot encoding for DL multiclassification
#'
#' @param df object of class 'x3p' or a tibble style dataframe containing 'x3p' objects
#' @return A numeric vector
#' @export
x3p_labels_to_numeric <- function(df){

  result <- c()

  df$label_chosen[df$label_chosen == "#CD7F32FF"] <- 0
  df$label_chosen[df$label_chosen == "#00FF00FF"] <- 1
  df$label_chosen[df$label_chosen == "#66FFFFFF"] <- 2
  df$label_chosen[df$label_chosen == "#FFFFFFFF"] <- 3
  df$label_chosen[df$label_chosen == "#1F376CFF"] <- 1
  df$label_chosen[df$label_chosen == "#FF0080FF"] <- 4
  df$label_chosen[df$label_chosen == "#000000FF"] <- 5

  result <- as.numeric(df$label_chosen)

  return(result)
}
