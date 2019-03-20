% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/start_screen_record.R
\name{start_screen_record}
\alias{start_screen_record}
\title{Start Screen Recording}
\usage{
start_screen_record(device = guess_recording_device(),
  outfile = tempfile(fileext = ".avi"), args = NULL,
  overwrite = TRUE, audio = FALSE, video = TRUE, run = TRUE,
  verbose = TRUE, show_std_err = FALSE)
}
\arguments{
\item{device}{Device to record on}

\item{outfile}{Output filename (with extension)}

\item{args}{additional arguments to pass to \code{ffmpeg}}

\item{overwrite}{Should output file be overwritten if it exists?}

\item{audio}{Should audio be captured}

\item{video}{Should video be captured}

\item{run}{Should the code be run (default \code{TRUE}) or simply
have the arguments printed (\code{FALSE})}

\item{verbose}{print diagnostic messages}

\item{show_std_err}{Show \code{STDERR} output, passed to \link[sys:exec_background]{sys::exec_background}.}
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
start_screen_record(run = FALSE)
out = start_screen_record(audio = FALSE)
Sys.sleep(2)
res = end_screen_record(out$pid)
stopifnot(file.exists(out$outfile))
if (interactive()) {
   sys::exec_wait("open", args = out$outfile)
}

}