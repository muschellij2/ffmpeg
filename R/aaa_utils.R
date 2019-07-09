duration <- function(wav){
  stopifnot(is_Wave(wav))
  length(wav@left) / wav@samp.rate
}