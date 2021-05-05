#' Interpolate Missing Values in surface.matrix of an 'x3p' object
#'
#' This function interpolates the missing values contained in a single 'x3p' object or multiple 'x3p' objects
#' A missing ith jth cell is interpolated by taking the average of that cell's 8 neighbors
#'
#' @param df object of class 'x3p' or a tibble style dataframe containing 'x3p' objects
#' @param multiple clarify whether user is working with a single 'x3p' object or multiple in a tibble style dataframe
#' @param select_col if multiple = TRUE, location of the x3p objects
#' @return A 'x3p' object with a surface.matrix containing no missing values
#' @export
x3p_interpolate_NA <- function(df, multiple = FALSE, select_col){
  if(multiple == TRUE){
    if(select_col %in% colnames(df)){
      for(i in 1:nrow(df)){

        pad_surf <- rbind(0, cbind(0, df[[select_col]][[i]]$surface.matrix, 0), 0)

        for(j in 1:nrow(df[[select_col]][[i]]$surface.matrix)){
          for(k in 1:ncol(df[[select_col]][[i]]$surface.matrix)){

            if(is.na(pad_surf[j,k]) &

               any(!is.na(c(pad_surf[j-1, k-1], pad_surf[j-1, k], pad_surf[j-1, k+1], pad_surf[j, k-1], pad_surf[j+1, k-1], pad_surf[j+1, k], pad_surf[j+1, k+1])))){

              pad_surf[j,k] <- mean(c(pad_surf[j-1, k-1], pad_surf[j-1, k], pad_surf[j-1, k+1], pad_surf[j, k-1], pad_surf[j+1, k-1], pad_surf[j+1, k], pad_surf[j+1, k+1]), na.rm = TRUE)

            }

          }
        }

        pad_surfx <- pad_surf[-c(min(seq(nrow(pad_surf))), max(seq(nrow(pad_surf)))), -c(min(seq(ncol(pad_surf))), max(seq(ncol(pad_surf))))]
        pad_surfx[, ncol(pad_surfx) ] <- pad_surfx[, ncol(pad_surfx) - 1]
        pad_surfx[nrow(pad_surfx), ] <- pad_surfx[nrow(pad_surfx)-1, ]
        df[[select_col]][[i]]$surface.matrix <- pad_surfx
      }
    } else {stop("Error: column selected containing x3p object does not exist")}

  } else if(multiple == FALSE){

    pad_surf <- rbind(0, cbind(0, df$surface.matrix, 0), 0)

    for(j in 1:nrow(df$surface.matrix)){
      for(k in 1:ncol(df$surface.matrix)){

        if(is.na(pad_surf[j,k]) &

           any(!is.na(c(pad_surf[j-1, k-1], pad_surf[j-1, k], pad_surf[j-1, k+1], pad_surf[j, k-1], pad_surf[j+1, k-1], pad_surf[j+1, k], pad_surf[j+1, k+1])))){

          pad_surf[j,k] <- mean(c(pad_surf[j-1, k-1], pad_surf[j-1, k], pad_surf[j-1, k+1], pad_surf[j, k-1], pad_surf[j+1, k-1], pad_surf[j+1, k], pad_surf[j+1, k+1]), na.rm = TRUE)

        }

      }
    }

    pad_surfx <- pad_surf[-c(min(seq(nrow(pad_surf))), max(seq(nrow(pad_surf)))), -c(min(seq(ncol(pad_surf))), max(seq(ncol(pad_surf))))]
    pad_surfx[, ncol(pad_surfx) ] <- pad_surfx[, ncol(pad_surfx) - 1]
    pad_surfx[nrow(pad_surfx), ] <- pad_surfx[nrow(pad_surfx)-1, ]
    df$surface.matrix <- pad_surfx

  }

  return(df)
}
