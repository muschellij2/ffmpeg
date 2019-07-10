#' Video information from ffmpeg
#'
#' @param file Video file to get information from 
#'
#' @return A character string
#' @export
#'
#' @examples
#' fname = system.file("extdata", "example.mp4", package = "ffmpeg")
#' if (have_ffmpeg_exec()) {
#' res = ffmpeg_video_info(fname)
#' }
ffmpeg_video_info = function(file) {
  ffmpeg = ffmpeg_exec(quote = FALSE)
  file = normalizePath(file)
  args = c("-i", file, "-hide_banner")
  suppressWarnings({
  res = system2(ffmpeg, args = args, stdout = FALSE, stderr = TRUE)
  })
  res = res[ !trimws(res) %in% 
               "At least one output file must be specified"]
  return(res)
}