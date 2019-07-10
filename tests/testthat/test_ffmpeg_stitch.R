testthat::context("Test ffmpeg()")


stitcher = function(images, audio, 
                    audio_codec = get_audio_codec(), 
                    video_codec = get_video_codec(),
                    output = tempfile(fileext = ".mp4"),
                    verbose = TRUE) {
  
  tdir = normalizePath(tempdir(), winslash = "/")
  
  audio_bitrate = "192k"
  video_bitrate = NULL
  
  stopifnot(length(images) > 0)
  images <- normalizePath(images, mustWork = TRUE, winslash = "/")
  output_dir <- normalizePath(dirname(output))
  stopifnot(
    length(audio) > 0,
    identical(length(images), length(audio)),
    all(file.exists(images)),
    dir.exists(output_dir)
  )
  if (is.character(audio)) {
    audio = lapply(audio, tuneR::readMP3)
    audio = lapply(audio, function(wav) {
      ideal_duration <- ceiling(length(wav@left) / wav@samp.rate)
      left = rep(0, 
                 wav@samp.rate * ideal_duration - length(wav@left))
      right = numeric(0)
      if (wav@stereo) {
        right = left
      }
      end_wav = tuneR::Wave(
        left = left,
        right = right,
        bit = wav@bit, samp.rate = wav@samp.rate)         
      wav <- tuneR::bind(wav, end_wav)
      wav      
    })
  }
  # Make a hard path
  output = file.path(output_dir, basename(output))
  
  if (verbose > 0) {
    message("Writing out Wav for audio")
  }
  wav <- Reduce(audio, f = tuneR::bind)
  wav_path <- file.path(output_dir, 
                        paste0("audio_", 
                               basename(tempfile(tmpdir = tdir)), 
                               ".wav"))
  tuneR::writeWave(wav, filename = wav_path)
  on.exit(unlink(wav_path, force = TRUE), add = TRUE)
  
  input_txt_path <- paste0("input_", 
                           basename(tempfile(tmpdir = tdir)), 
                           ".txt")
  ## on windows ffmpeg cancats names adding the working directory, so if
  for (i in seq_along(images)){
    cat(paste0("file ", "'", images[i], "'", "\n"), 
        file = input_txt_path, append = TRUE)
    cat(paste0("duration ", duration(audio[[i]]), "\n"), 
        file = input_txt_path, append = TRUE)
  }
  cat(paste0("file ", "'", images[i], "'", "\n"), 
      file = input_txt_path, append = TRUE)
  
  # needed for users as per 
  # https://superuser.com/questions/718027/
  # ffmpeg-concat-doesnt-work-with-absolute-path
  # input_txt_path = normalizePath(input_txt_path, winslash = "\\")
  
  ffmpeg = ffmpeg_exec(quote = TRUE)
  ffmpeg_opts = ""
  ffmpeg_opts = c(ffmpeg_opts, 
                  '-vf "scale=trunc(iw/2)*2:trunc(ih/2)*2"')
  
  ffmpeg_opts = paste(ffmpeg_opts, collapse = " ")
  command <- paste(
    ffmpeg, "-y -f concat -safe 0 -i", shQuote(input_txt_path), 
    "-i", shQuote(wav_path), 
    ifelse(!is.null(video_codec), paste("-c:v", video_codec),
           ""),
    ifelse(!is.null(audio_codec), paste("-c:a", audio_codec),
           ""),    
    ifelse(!is.null(audio_bitrate), paste("-b:a", audio_bitrate),
           ""), 
    ifelse(!is.null(video_bitrate), paste("-b:v", video_bitrate),
           ""), 
    " -shortest -vsync vfr -pix_fmt yuv420p",
    ffmpeg_opts,
    shQuote(output))
  if (verbose > 0) {
    message(command)
    cat(command)
  }
  if (verbose > 1) {
    message("Input text path is:")
    cat(readLines(input_txt_path), sep = "\n")
  }
  res = system(command)
  if (res != 0) {
    warning("Result was non-zero for ffmpeg")
  }
  
  on.exit(unlink(input_txt_path, force = TRUE), add = TRUE)
  res = file.exists(output) && file.size(output) > 0
  attr(res, "outfile") = output
}

if (have_ffmpeg_exec()) {
  res = ffmpeg_audio_codecs()
  if (is.null(res)) {
    fdk_enabled = FALSE
  } else {
    fdk_enabled = grepl("fdk", res[ res$codec == "aac", "codec_name"])
  }  
} else {
  fdk_enabled = FALSE
}
if (fdk_enabled) {
  audio_codec = "libfdk_aac"
} else {
  audio_codec = "ac3"
}

testthat::test_that("ffmpeg can combine audio and images into a video", {
  testthat::skip_on_cran()
  
  tdir = normalizePath(tempdir(), winslash = "/")
  
  n_plots = 3
  graphs <- sapply(1:n_plots, function(x) {
    tempfile(fileext = ".jpg", tmpdir = tdir)
  })
  graphs = file.path(tdir, basename(graphs))
  for (i in seq_along(graphs)) {
    jpeg(graphs[i])
    plot(1:5 * i, 1:5, main = i)
    dev.off()
  }
  graphs = normalizePath(
    graphs, mustWork = TRUE,
    winslash = "/")
  
  sound <- replicate(
    n_plots, 
    tuneR::Wave(round(rnorm(88200, 127, 20)), 
                samp.rate = 44100, bit = 16))
  
  output <- tempfile(fileext = ".mp4", tmpdir = tdir)
  
  stitcher(images = graphs, sound, 
           output = output,
           audio_codec = audio_codec,
           verbose = 2)
  
  expect_true(file.size(output) > 50000)
})
