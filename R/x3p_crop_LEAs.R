#' Crop an x3p object
#'
#' This function allows the user to crop an 'x3p' object into the maximum number of smaller 'x3p' objects
#' by specifying a window size
#'
#' @param df object of class 'x3p' or a tibble style dataframe containing 'x3p' objects
#' @param obtain_index Grid locations from where the crop occured
#' @param value window size
#' @param multiple clarify whether user is working with a single 'x3p' object or multiple in a tibble style dataframe
#' @param select_col if multiple = TRUE, location of the x3p objects
#' @return A list containig cropped x3ps and original indexes if specified
#' @export
x3p_crop_LEAs <- function(df, obtain_index = TRUE, value, multiple = FALSE, select_col){

  f = function(x, n){
    x - 128*(1:n)

  }

  if(multiple == TRUE){
    if(select_col %in% colnames(df)){
      if(obtain_index == FALSE){

        for(i in 1:nrow(df)){


      dimension_x <- dim(df[[select_col]][[i]]$surface.matrix)[1] #obtain dimension x
      dimension_y <- dim(df[[select_col]][[i]]$surface.matrix)[2] #obtain dimension y

      division_x <- dim(df[[select_col]][[i]]$surface.matrix)[1] / value
      division_y <- dim(df[[select_col]][[i]]$surface.matrix)[2] / value

      grid_lengths_x <- c(f(dimension_x, division_x), 0)
      grid_lengths_y <- c(f(dimension_y, division_y), 0) # Create so called grid lengths

      grid_outline <- expand.grid(grid_lengths_x, grid_lengths_y) # Grid outline

      grid_outline <- grid_outline %>% filter(Var1 >= 0 & Var2 >= 0)

      df$crop_storage[[i]] <- map2(.x = grid_outline$Var1 ,
                                   .y = grid_outline$Var2 ,
                                   .f = x3ptools::x3p_crop, x3p = df[[select_col]][[i]],
                                   width = value,
                                   height = value) # Creates the new cropped x3ps and store as a list

    }

  } else{

    for(i in 1:nrow(df)){


      dimension_x <- dim(df[[select_col]][[i]]$surface.matrix)[1] #obtain dimension x
      dimension_y <- dim(df[[select_col]][[i]]$surface.matrix)[2] #obtain dimension y

      division_x <- dim(df[[select_col]][[i]]$surface.matrix)[1] / value
      division_y <- dim(df[[select_col]][[i]]$surface.matrix)[2] / value

      grid_lengths_x <- c(f(dimension_x, division_x), 0)
      grid_lengths_y <- c(f(dimension_y, division_y), 0) # Create so called grid lengths

      grid_outline <- expand.grid(grid_lengths_x, grid_lengths_y) # Grid outline

      grid_outline <- grid_outline %>% filter(Var1 >= 0 & Var2 >= 0)

      df$original_indexes[[i]] <- grid_outline

      df$crop_storage[[i]] <- map2(.x = grid_outline$Var1 ,
                                   .y = grid_outline$Var2 ,
                                   .f = x3ptools::x3p_crop, x3p = df[[select_col]][[i]],
                                   width = value,
                                   height = value) # Creates the new cropped x3ps and store as a list

    }

  }


  } else {stop("Error: column selected containing x3p object does not exist")}
  } else if(multiple == FALSE){


    dimension_x <- dim(df$surface.matrix)[1] #obtain dimension x
    dimension_y <- dim(df$surface.matrix)[2] #obtain dimension y

    division_x <- dim(df$surface.matrix)[1] / value
    division_y <- dim(df$surface.matrix)[2] / value

    grid_lengths_x <- c(f(dimension_x, division_x), 0)
    grid_lengths_y <- c(f(dimension_y, division_y), 0) # Create so called grid lengths

    grid_outline <- expand.grid(grid_lengths_x, grid_lengths_y) # Grid outline

    grid_outline <- grid_outline %>% filter(Var1 >= 0 & Var2 >= 0)

    index <- grid_outline

    crop_storage <- map2(.x = grid_outline$Var1 ,
                                 .y = grid_outline$Var2 ,
                                 .f = x3ptools::x3p_crop, x3p = df,
                                 width = value,
                                 height = value)

    crops <- list(index, crop_storage)



    return(crops)
  }

  return(df)
}








