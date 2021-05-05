#' To be continued
#'
#' @export
callback_lr_init <- function(logs){
  iter <<- 0
  lr_hist <<- c()
  iter_hist <<- c()
}

#'To be continued
#'
#'@export
callback_lr_set <- function(batch, logs){
  iter <<- iter + 1
  LR <- l_rate[iter]
  if(is.na(LR)) LR <- l_rate[length(l_rate)]
  k_set_value(cnn_model$optimizer$lr, LR)
}


#'To be continued
#'@export
callback_lr_log <- function(batch, logs){
  lr_hist <<- c(lr_hist, k_get_value(cnn_model$optimizer$lr))
  iter_hist <<- c(iter_hist, k_get_value(cnn_model$optimizer$iterations))
}
