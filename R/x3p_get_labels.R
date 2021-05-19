#' Obtain all labels and majority label from an 'x3p' object's mask
#'
#' This function gives the user each label associated with a single or multiple 'x3p' objects
#' Majority label is defined by a decision ratio specified by the user
#'
#' @param df object of class 'x3p' or a tibble style dataframe containing 'x3p' objects
#' @param decision_ratio numeric value specified by the user for majority label
#' @param multiple clarify whether user is working with a single 'x3p' object or multiple in a tibble style dataframe
#' @param select_col if multiple = TRUE, location of the x3p objects
#' @return A list containing labels and majority labels associated
#' @export
x3p_get_labels <- function(df, decision_ratio, multiple = FALSE, select_col){

  Freq <- NULL

  if(multiple == TRUE){
  if(select_col %in% colnames(df)){
  df <- tidyr::unnest(df, select_col)

  for(i in 1:nrow(df)){

    df$labels[[i]] <- unique(purrr::pluck(df[[select_col]][[i]], 6))  #store all annotations in a crop in a list

    df$label_chosen[[i]] <- data.frame(prop.table(table(df[[select_col]][[i]]$mask))) %>%
      filter(Freq > decision_ratio | Freq == max(Freq)) %>% {as.character(.$Var1[1])} #Store the majority annotation for a crop

      }
    } else {stop("Error: column selected containing x3p object does not exist")}
  } else if(multiple == FALSE){

    labels <- unique(purrr::pluck(df, 6))

    label_chosen <- c(prop.table(table(df$mask))) %>%
      filter(Freq > decision_ratio | Freq == max(Freq)) %>% {as.character(.$Var1[1])}

    label_info <- list(labels, label_chosen)

    return(label_info)

  }

  return(df)
}
