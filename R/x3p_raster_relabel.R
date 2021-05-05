#' Relabel mask of an 'x3p' object
#' This function allows a user to relabel a mask of an 'x3p' object
#' The user is also allowed to specify a new mask name and mask color.
#'
#' @param df object of class 'x3p' or a tibble style dataframe containing 'x3p' objects
#' @param mask_1 old mask label the user wishes to replace, must be a color code #8digits
#' @param mask_2 new mask label user wishes to replace with old mask label, must be a color code #8digits
#' @param color_new Optional color specification for new mask label
#' @param annotation_new Rename label associated with the new mask
#' @param multiple clarify whether user is working with a single 'x3p' object or multiple in a tibble style dataframe
#' @param select_col if multiple = TRUE, location of the x3p objects
#' @return 'x3p' object with altered mask
#' @importFrom grDevices as.raster
#' @export
x3p_raster_relabel <- function(df, mask_1, mask_2, color_new, annotation_new, multiple = FALSE, select_col){
  if(multiple == TRUE){
    if(select_col %in% colnames(df)){
      for(i in 1:nrow(df)){

        df[[select_col]][[i]]$mask <- as.matrix(df[[select_col]][[i]]$mask)
        df[[select_col]][[i]]$mask <- t(df[[select_col]][[i]]$mask)
        df[[select_col]][[i]]$mask[is.na(df[[select_col]][[i]]$surface.matrix) & df[[select_col]][[i]]$mask == mask_1 ] <- mask_2
        df[[select_col]][[i]]$mask <- t(df[[select_col]][[i]]$mask)
        df[[select_col]][[i]]$mask <- as.raster(df[[select_col]][[i]]$mask)
        df[[select_col]][[i]] <- x3ptools::x3p_add_annotation(df[[select_col]][[i]], color = color_new, annotation = annotation_new)

      }
    } else {stop("Error: column selected containing x3p object does not exist")}

  } else if(multiple == FALSE){

    df$mask <- as.matrix(df$mask)
    df$mask <- t(df$mask)
    df$mask[is.na(df$surface.matrix) & df$mask == mask_1] <- mask_2
    df$mask <- t(df$mask)
    df$mask <- as.raster(df$mask)
    df <- x3ptools::x3p_add_annotation(df, color = color_new, annotation = annotation_new)

  }

  return(df)

}


#' Relabel mask of an 'x3p' object with x and y border specifications
#' This function allows a user to relabel a mask of an 'x3p' object
#' The user is also allowed to specifiy a new mask name and mask color
#' The user is required to specify minimum and maximum, x and y locations, beaware of surface dimensions
#'
#' @param df object of class 'x3p' or a tibble style dataframe containing 'x3p' objects
#' @param mask_1 old mask label the user wishes to replace, must be a color code #8digits
#' @param mask_2 new mask label user wishes to replace with old mask label, must be a color code #8digits
#' @param x1 minimum x location...
#' @param x2 maximum x location...
#' @param y1 minimum y location...
#' @param y2 maximum y location...
#' @param multiple clarify whether user is working with a single 'x3p' object or multiple in a tibble style dataframe
#' @param select_col if multiple = TRUE, location of the x3p objects
#' @return 'x3p' object with altered mask
#' @export
x3p_raster_relabel_2 <- function(df, mask_1, mask_2, x1, x2, y1, y2, multiple = FALSE, select_col){
  if(multiple == TRUE){
    if(select_col %in% colnames(df)){

      for(i in 1:nrow(df)){

        df[[select_col]][[i]]$mask <- as.matrix(df[[select_col]][[i]]$mask)
        df[[select_col]][[i]]$mask <- t(df[[select_col]][[i]]$mask)
        new_mask <- df[[select_col]][[i]]$mask

        for(j in c(x1:x2)){
          for(k in c(y1:y2)){

            if(is.na(df[[select_col]][[i]]$surface.matrix[j,k]) & new_mask[j,k] == mask_1){

              new_mask[j,k] <- mask_2

            }

          }
        }

        new_mask <- t(new_mask)
        new_mask <- as.raster(new_mask)
        df[[select_col]][[i]]$mask <- new_mask

      }

    } else {stop("Error: column selected containing x3p object does not exist")}

  } else if(multiple == FALSE){

    df$mask <- as.matrix(df$mask)
    df$mask <- t(df$mask)

    for(j in c(x1:x2)){
      for(k in c(y1:y2)){
        if(is.na(df$surface.matrix[j,k]) & df$mask[j,k] == mask_1){
          df$mask[j,k] <- mask_2
        }
      }

    }

    df$mask <- t(df$mask)
    df$mask <- as.raster(df$mask)

  }

  return(df)

}
