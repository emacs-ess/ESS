## simple Message Parsing Inerface

.ess_mpi_send <- function(head, payload, ...){
    payload <- sprintf(as.character(payload), ...)
    cat(sprintf("%s%s", head, payload))
}

.ess_mpi_MSG <- function(msg, ...){
    .ess_mpi_send("MSG", msg, ...)
}
