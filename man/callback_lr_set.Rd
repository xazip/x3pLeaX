% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cnn_callbacks.R
\name{callback_lr_set}
\alias{callback_lr_set}
\title{This function should be used with keras callback_lambda
Use for metrics on each epoch, iteration, batch
When training a model, each epoch has 'n' iterations, where each iteration has 'k' batches}
\usage{
callback_lr_set(batch, logs)
}
\arguments{
\item{batch}{created from keras callbacks, logs 'which batch'}

\item{logs}{metrics contained in a list generated by R6 class}
}
\value{
updated iter, LR
}
\description{
This function sets the learning rate accordingto the learning for each batch of data.
}
