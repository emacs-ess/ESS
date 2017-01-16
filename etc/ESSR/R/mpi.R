## simple Message Parsing Inerface

.ess_mpi_send <- function(head, ...){
    payload <- paste(..., sep = "")
    cat(sprintf("%s%s", head, payload))
}

.ess_mpi_message <- function(msg){
    .ess_mpi_send("message", msg)
}

.ess_mpi_y_or_n <- function(prompt, callback){
    .ess_mpi_send("y-or-n", prompt, callback)
}

.ess_mpi_eval <- function(expr, callback){
    .ess_mpi_send("eval", expr, callback)
}

.ess_mpi_error <- function(msg) {
    .ess_mpi_send("error", msg)
}

