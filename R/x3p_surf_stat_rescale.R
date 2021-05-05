
#' Get Surface Summary Statistics from x3p object
#' Rescale Surface of an x3p object
#'
#' This function takes a single or multiple x3p objects and allows
#' a user to obtain summary statistics regarding the surface.
#'
#' This function also takes single or multiple x3p objects and allows
#' the user to rescale the surface elements.
#'
#'
#' @param df object of class 'x3p' or a tibble style dataframe containing 'x3p' objects
#' @param obtain user specification for which statistic/action they wish to obtain/perform
#' @param multiple clarify whether user is working with a single 'x3p' object or multiple in a tibble style dataframe
#' @param select_col if multiple = TRUE, location of the x3p objects
#' @param value user specified value for rescaling surface elements of an 'x3p' object
#' @return Statistics regarding surface of 'x3p' object
#' @return Rescaled surface.matrix of 'x3p' object
#' @export
x3p_surface_rescale <- function(df, obtain = c("minimum", "maximum", "both", "rescale"), multiple = FALSE, select_col, value){
  if(multiple == TRUE){

    if(select_col %in% colnames(df)){

      if(obtain == "minimum"){
        for(i in 1:nrow(df)){
          df$x3pmin[[i]] <- min(df[[select_col]][[i]]$surface.matrix, na.rm = TRUE)
        }

      } else if(obtain == "maximum"){
        for(i in 1:nrow(df)){
          df$x3pmax[[i]] <- max(df[[select_col]][[i]]$surface.matrix, na.rm = TRUE)
        }

      } else if(obtain == "both"){
        for(i in 1:nrow(df)){
          df$x3pmin[[i]] <- min(df[[select_col]][[i]]$surface.matrix, na.rm = TRUE)
          df$x3pmax[[i]] <- max(df[[select_col]][[i]]$surface.matrix, na.rm = TRUE)
        }

      } else if(obtain == "rescale"){
        for(i in 1:nrow(df)){
          df$x3pmin[[i]] <- min(df[[select_col]][[i]]$surface.matrix, na.rm = TRUE)
          df$x3pmax[[i]] <- max(df[[select_col]][[i]]$surface.matrix, na.rm = TRUE)
          df[[select_col]][[i]]$surface.matrix <- (df[[select_col]][[i]]$surface.matrix - df$x3pmin[[i]])/(df$x3pmax[[i]] - df$x3pmin[[i]])*value
        }
      }


    } else {stop("Error: column selected containing x3p object does not exist")}


  } else if(multiple == FALSE){

    if(obtain == "minimum") return({min(df$surface.matrix, na.rm = TRUE)})
    if(obtain == "maximum") return({max(df$surface.matrix, na.rm = TRUE)})
    if(obtain == "rescale") return({(df$surface.matrix - min(df$surface.matrix, na.rm = TRUE))/(max(df$surface.matrix, na.rm = TRUE) - min(df$surface.matrix, na.rm = TRUE))*value})

  }

  return(df)

}







#'
x3p_lea_df <- function(df){

  for(i in 1:nrow(df)){

    acutal_dataframe <- x3p_to_df(df$x3p[[i]])
    df$breakoff_stats[[i]] <- acutal_dataframe %>% filter(annotation == "breakoff") %>% select(x, y, value)
    df$damage_stats[[i]] <- acutal_dataframe %>% filter(annotation == "damage") %>% select(x, y, value)
    df$lg_stats[[i]] <- acutal_dataframe %>% filter(annotation == "left groove") %>% select(x, y, value)
    df$ms_locations[[i]] <- acutal_dataframe %>% filter(annotation == "Missing Value") %>% select(x, y, value)
    df$rg_stats[[i]] <- acutal_dataframe %>% filter(annotation == "right groove") %>% select(x, y, value)
    df$no_striae_stats[[i]] <- acutal_dataframe %>% filter(annotation == "no striations") %>% select(x, y, value)
    df$vertical_striae_stats[[i]] <- acutal_dataframe %>% filter(annotation == "vertical change in striations") %>% select(x, y, value)

  }

  breakoff_df <- dfST %>% select(breakoff_stats, index)
  breakoff_df <- unnest(breakoff_df)
  breakoff_df$name <- "breakoff"

  damage_df <- dfST %>% select(damage_stats, index)
  damage_df <- unnest(damage_df)
  damage_df$name <- "damage"

  left_groove_df <- dfST %>% select(lg_stats, index)
  left_groove_df <- unnest(left_groove_df)
  left_groove_df$name <- "left groove"

  MS_df <- dfST %>% select(ms_locations, index)
  MS_df <- unnest(MS_df)
  MS_df$name <- "missing value"

  right_groove_df <- dfST %>% select(rg_stats, index)
  right_groove_df <- unnest(right_groove_df)
  right_groove_df$name <- "right groove"

  no_striae_df <- dfST %>% select(no_striae_stats, index)
  no_striae_df <- unnest(no_striae_df)
  no_striae_df$name <- "no striations"

  vertical_striae_df <- dfST %>% select(vertical_striae_stats, index)
  vertical_striae_df <- unnest(vertical_striae_df)
  vertical_striae_df$name <- "vertical change in striations"

  df_final <- as.data.frame(rbind(breakoff_df, damage_df, left_groove_df, right_groove_df, no_striae_df, vertical_striae_df, MS_df))

  return(df_final)

}
