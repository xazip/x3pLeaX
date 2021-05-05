#' Create Tensor from 'x3p' object
#'
#' This function takes a single 'x3p' object or multiple and creates a tensor used for training Deep Learning Algorithms
#'
#' @param df object of class 'x3p' or a tibble style dataframe containing 'x3p' objects
#' @param array_dim specify the tensor dimension
#' @param dim1 dimension 1, x
#' @param dim2 dimension 2, y
#' @param dim3 dimension 3, z, surface values
#' @param dim4 dimension 4, color scale
#' @param multiple clarify whether user is working with a single 'x3p' object or multiple in a tibble style dataframe
#' @param select_col if multiple = TRUE, location of the x3p objects
#' @return A array/tensor with user specificed dimensions
#' @export
x3p_to_tensor <- function(df, array_dim, dim1, dim2, dim3, dim4, multiple = FALSE, select_col){
  if(multiple == TRUE){
    if(select_col %in% colnames(df)){
      if(array_dim == 5){

        array_storage = list()

        for(i in 1:nrow(df)){

          array_storage[[i]] <- array(c(df[[select_col]][[i]]$surface.matrix),
                                      dim = c(dim1, dim2, dim3, dim4))

        }

      } else if(array_dim == 4){

        array_storage = list()

        for(i in 1:nrow(df)){

          array_storage[[i]] <- array(c(df[[select_col]][[i]]$surface.matrix),
                                      dim = c(dim1, dim2, dim3))

        }

      } else if(array_dim == 3){

        array_storage = list()

        for(i in 1:nrow(df)){

          array_storage[[i]] <- array(c(df[[select_col]][[i]]$surface.matrix),
                                      dim = c(dim1, dim2))

        }

      } else if(array_dim == 2){

        array_storage = list()

        for(i in 1:nrow(df)){

          array_storage[[i]] <- array(c(df[[select_col]][[i]]$surface.matrix),
                                      dim = c(dim1))

        }

      } else {stop("Error: tensor can only be of dimensions greater than or equal to 2 and less than or equal to 5")}
    } else {stop("Error: column selected containing x3p object does not exist")}

  } else if(multiple == FALSE){

    if(array_dim == 4) return({array(c(df$surface.matrix), dim = c(dim1, dim2, dim3, dim4))})
    if(array_dim == 3) return({array(c(df$surface.matrix), dim = c(dim1, dim2, dim3))})
    if(array_dim == 2) return({array(c(df$surface.matrix), dim = c(dim1, dim2))})
    if(array_dim == 1) return({array(c(df$surface.matrix), dim = c(dim1))})

  }

  array_storage <- do.call(abind::abind, c(array_storage, along = 0))

  return(array_storage)

}
