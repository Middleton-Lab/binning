#' Aggregate running (new system)
#'
#' Aggregate running data from new system. Writes a file that
#' includes the aggregated data.
#'
#' @title Aggregate running (new system)
#'
#' @param all boolean: If \code{TRUE} (default), all \code{.dat}
#'   files in the working directory will be processed. Note that if
#'   \code{all = TRUE}, then all dates must be represented by at
#'   least one \code{.dat} file.
#' @param by The period of time for aggregation. This parameter gets
#'   passed to \code{seq.POSIXt()} and should be one of: \code{"day"}
#'   (default), \code{"min"}, \code{"hour"}, etc. See
#'   \code{\link{seq.POSIXt}} for more options.
#' @param analyze boolean: If \code{TRUE} (default is \code{FALSE}),
#' also run \code{analyze_runnung()}.
#' @param ... If \code{all = TRUE}, then \code{start} and \code{end}
#'   should also be supplied as a quoted string in the format
#'   \code{"2011-12-31"}.
#'
#' @author Kevin Middleton {middletonk@@missouri.edu}
#'
#' @seealso \code{\link{analyze_running}}
#'
#' @keywords file data
#'
#' @export
#'
aggregate_running <- function(all = FALSE,
                              by = "day",
                              analyze = FALSE,
                              ...) {
  now <- Sys.time()
  if (all) {
    ## Get list of files
    files <- list.files(pattern = ".dat")

    ## Get date range
    strip_date <- function(x){
      x <- gsub("Data_", "", x, fixed = TRUE)
      x <- gsub(".dat", "", x, fixed = TRUE)
      return(x)
    }
    dates <- as.Date(strip_date(files), "%m-%d-%Y")

    start.date <- min(dates)
    end.date <- max(dates)
  } else {
    dots <- list(...)

    if(!"start" %in% names(dots)) {
      stop("If all = FALSE, then start date must be supplied.")
    }

    if(!"end" %in% names(dots)) {
      stop("If all = FALSE, then end date must be supplied.")
    }

    start.date <- as.Date(dots$start)
    end.date <- as.Date(dots$end)
  }

  ## Generate date range
  date.range <- seq(from = as.Date(start.date, "%m-%d-%Y"),
                    to = as.Date(end.date, "%m-%d-%Y"),
                    by = "day")
  date.range <- format(date.range, "%m-%d-%Y")
  files.in <- paste("Data_", date.range, ".dat", sep = "")

  rawdat <- list()
  for (i in seq_len(length(files.in))) {
    message("Reading ", files.in[i])
    x <- read.table(files.in[i], sep = "\t", as.is = TRUE)
    rawdat[[i]] <- x
  }

  dat <- as.data.frame(do.call("rbind", rawdat))

  rm(rawdat)
  names(dat) <- c("Wheel", "TimeStamp")
  message("Read ", nrow(dat), " revolutions.")

  ## Drop duplicated rows (phantom revolutions)
  ## It's unlikely that two sensors will record at the same 0.001 sec
  dat <- drop_dupes(dat)
  message(nrow(dat), " revolutions remain.")

  ## Strip milliseconds
  dat$TimeStamp <- gsub(".{4}$", "", dat$TimeStamp)

  ## Convert to date time
  dat$TimeStamp <- as.POSIXlt(dat$TimeStamp, format = "%m/%d/%Y %H:%M:%S")

  ## Split wheels
  Wheels <- levels(as.factor(dat$Wheel))
  whldat <- list()
  for (i in seq_len(length(Wheels))) {
    message("Processing ", Wheels[i])
    whldat[[i]] <- dat[dat$Wheel == Wheels[i], ]
  }

  names(whldat) <- Wheels

  ## Aggregate by day
  message("Aggregating data by ", by, " from ", start.date,
          " to ", end.date, ".")
  message("Each day runs from 12:00 to 11:59 the next day.")
  message("Note that any revolutions before 12:00 on ", start.date,
          " or after 12:00 on ", end.date, " will be deleted.")
  agg.dat <- lapply(whldat, FUN = agg.revs, start.date, end.date, by)
  rm(whldat)

  ## Reformat and write out
  # Format output POSIX string
  if (by == "day") outstring <- "%m/%d/%Y"
  if (by == "hr") outstring <- "%m/%d/%Y %H"
  if (by == "min") outstring <- "%m/%d/%Y %H:%M"

  wide.dat <- t(do.call("rbind", agg.dat))
  wide.dat <- as.data.frame(wide.dat)
  wide.dat <- cbind(Date = format(as.POSIXlt(rownames(wide.dat)),
                    outstring),
                    wide.dat)
  outfile <- paste(start.date, "_to_", end.date, ".csv", sep = "")
  write.csv(wide.dat, file = outfile, row.names = FALSE)
  message("Wrote file: ", outfile)
  Sys.time() - now
  if (analyze) {
    run <- analyze_running(outfile)
    return(run)
  }
}
