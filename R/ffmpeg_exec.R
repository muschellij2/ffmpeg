#' Get Path to ffmpeg Executable
#'
#' @return The path to the `ffmpeg` executable, or an error.
#' 
#' @note This looks using `Sys.getenv("ffmpeg")` and `Sys.which("ffmpeg")`
#' to find `ffmpeg`.  If `ffmpeg` is not in your PATH, then please set the
#' path to `ffmpeg` using `Sys.setenv(ffmpeg = "/path/to/ffmpeg")`
#' @export
#' 
#' @examples
#' if (have_ffmpeg_exec()) { 
#' ffmpeg_exec()
#' }
ffmpeg_exec = function() {
  ffmpeg = c(
    Sys.getenv("ffmpeg"), 
    Sys.which("ffmpeg"))
  ffmpeg = ffmpeg[nchar(ffmpeg) > 0]
  ffmpeg = ffmpeg[1]
  
  if (is.na(ffmpeg)) {
    stop(paste("Could not find ffmpeg. See the documentation ", 
               "for ffmpeg_exec() ", 
               "for more details."))
  }
  if (!ffmpeg %in% c("ffmpeg", "ffmpeg.exe")) {
    ffmpeg = normalizePath(ffmpeg, winslash = "/")
    if (get_os() == "windows") {
      ffmpeg = shQuote(ffmpeg)
    }    
  }

  return(ffmpeg)
}

#' @export
#' @rdname ffmpeg_exec
have_ffmpeg_exec = function() {
  exec = try({
    ffmpeg_exec()
  }, silent = TRUE)
  !inherits(exec, "try-error")
}
