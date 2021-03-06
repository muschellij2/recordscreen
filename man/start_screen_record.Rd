% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/start_screen_record.R
\name{start_screen_record}
\alias{start_screen_record}
\title{Start Screen Recording}
\usage{
start_screen_record(device = guess_recording_device(),
  outfile = tempfile(fileext = ".avi"), duration = NULL, args = NULL,
  video_number = 1, audio_number = 1, overwrite = TRUE,
  audio = FALSE, video = TRUE, run = TRUE, verbose = FALSE)
}
\arguments{
\item{device}{Device to record on}

\item{outfile}{Output filename (with extension)}

\item{duration}{record for a fixed duration, passed to the
\code{args}, in seconds}

\item{args}{additional arguments to pass to \code{ffmpeg}}

\item{video_number}{number of video device}

\item{audio_number}{number of audio device}

\item{overwrite}{Should output file be overwritten if it exists?}

\item{audio}{Should audio be captured}

\item{video}{Should video be captured}

\item{run}{Should the code be run (default \code{TRUE}) or simply
have the arguments printed (\code{FALSE})}

\item{verbose}{print diagnostic messages}
}
\value{
A list of the process ID and output file.
}
\description{
Start Screen Recording
}
\note{
https://trac.ffmpeg.org/wiki/Capture/Desktop
}
\examples{
res = Sys.which("ffmpeg")
stopifnot(res != "")
print(list_output_devices())
tempdir(check = TRUE)
start_screen_record(run = FALSE)
out = start_screen_record(audio = FALSE)
out
Sys.sleep(4)
out
res = end_screen_record(out)
stopifnot(file.exists(out$outfile))

out = start_screen_record(audio = TRUE, verbose = 2)
out
Sys.sleep(4)
out
res = end_screen_record(out)
stopifnot(file.exists(out$outfile))

out = start_screen_record(audio = TRUE, video = FALSE,
outfile = tempfile(fileext = ".mp3"), verbose = 2)
out
Sys.sleep(4)
out
res = end_screen_record(out)
stopifnot(file.exists(out$outfile))
mp3 = tuneR::readMP3(out$outfile)
range(mp3@left)

out = start_screen_record(audio = FALSE,
outfile = tempfile(fileext = ".gif"))
out
Sys.sleep(4)
res = end_screen_record(out)
stopifnot(file.exists(out$outfile))


}
