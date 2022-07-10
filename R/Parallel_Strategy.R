

Deep_Learning_parallel <- function(x3px,
                                   offset_x,
                                   offset_y,
                                   total_len,
                                   block_len,
                                   nb_split = ceiling(total_len/block_len),
                                   detect_cores = "TRUE",
                                   number_of_cores,
                                   path_to_model,
                                   input_dim,
                                   output_dim,
                                   application_source_file = TRUE,
                                   application_source_path,
                                   path_to_save
                                        
                                       ){
  
  assert_one_int(total_len)
  assert_pos(total_len)
  if (nb_split > total_len) {
    nb_split <- total_len
  }
  else if (nb_split == 0) {
    nb_split <- 1
  }
  assert_one_int(nb_split)
  assert_pos(nb_split)
  int <- total_len/nb_split
  upper <- round(1:nb_split * int)
  lower <- c(1, upper[-nb_split] + 1)
  size <- c(upper[1], diff(upper))
 range.parts <-  cbind(lower, upper, size)
  
  if(detect_cores == "TRUE"){
    
    n.cores <- as.numeric(parallel::detectCores())
  } else{
    n.cores <- as.numeric(number_of_cores)
  }
  
  
  cnn <- list()
  for(i in 1:n.cores){
    cnn[[i]] <- load_model_hdf5(path_to_model)
  }
  
  cl <- parallel::makeCluster(no_cores)
  
  doParallel::registerDoParallel(cl)
  
  cnn <- lapply(cnn, keras::serialize_model)
  
  tmpfile <- path_to_save 
  
  foreach(ic = 1:no_cores, model = cnn, .packages = c("keras")) %dopar% {
    
    ind <- bigstatsr:::seq2(range.parts[ic, ])
    model_local <- keras::unserialize_model(model)
    
    #Insert Application Here Application 
    
    if(application_source_file == TRUE){
      
      app_source_script <- source(application_source_path)
      
      app_source_script()
      
      
    } else if(application_source_file == FALSE){
      
      dim_x <- dim(x3px$surface.matrix)[1] - offset_x
      dim_y <- dim(x3px$surface.matrix)[2] - offset_y
      
      x.probs <- array(dim=c(dim_x,dim_y,6))
      x.all <- array(dim=c(dim_x,dim_y,6))

      for (i in c(ind[1]:ind[length(ind)])){
        for (j in 1:dim_y){
          crop <- x3ptools::x3p_crop(x3px, x = i, y = j, width = 128, height = 128)
          input <- array(c(crop$surface.matrix), dim = c(128, 128, 1, 1))
          input <- abind::abind(input, along = 0)
          output <- predict(model_local, input)
          output_color <- colors[apply(output, MARGIN = 1, which.max)]

          x.probs[i, j, 1] <- output[1]
          x.probs[i, j, 2] <- output[2]
          x.probs[i, j, 3] <- output[3]
          x.probs[i, j, 4] <- output[4]
          x.probs[i, j, 5] <- output[5]
          x.probs[i, j, 6] <- output[6]

          if(output_color == "#000000FF"){
            x.all[i, j, 6] <- output_color
          } else if(output_color == "#CD7F32FF"){
            x.all[i, j, 1] <- output_color
          } else if(output_color == "#00FF00FF"){
            x.all[i, j, 2] <- output_color
          } else if(output_color == "#66FFFFFF"){
            x.all[i, j, 3] <- output_color
          } else if(output_color == "#FFFFFFFF"){
            x.all[i, j, 4] <- output_color
          } else if (output_color == "#FF0080FF"){
            x.all[i, j, 5] <- output_color
          }
        }
      }
      
    }
    
    keras::serialize_model(model_local)
    saveRDS(x.all, file = paste0(tmpfile, "_", ic, ".rds"))
    saveRDS(x.probs, file = paste0(tmpfile, "probs_", ic, ".rds"))
    invisible(capture.output(x.all <- array(dim=c(dim_x,dim_y,6))))
    invisible(capture.output(x.probs <- array(dim=c(dim_x,dim_y,6))))
  }
  
}