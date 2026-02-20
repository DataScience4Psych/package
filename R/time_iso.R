
#' Append a fixed due-time suffix to a Date and return an ISO-like date string
#'
#' Converts a Date (or date-coercible object) to `"YYYY-MM-DD"` and appends the
#' constant `DUE_TIME`. This is typically used to build Canvas-style ISO strings
#' where the time portion is fixed globally (for example, `"T23:59:00Z"`).
#'
#' @param d A Date (or object coercible to Date) to format.
#' @param DUE_TIME A character scalar representing the time suffix to append, e.g. `"T23:59:00Z"`.
#' @return A character scalar of the form `"YYYY-MM-DD{DUE_TIME}"`.
#'
#' @details
#' This function assumes `DUE_TIME` exists in the package namespace.
#'
#' @examples
#' to_iso(as.Date("2026-02-20"))
#' @export
to_iso <- function(d, DUE_TIME = "T23:59:00Z"
                   ){ paste0(format(d, "%Y-%m-%d"), DUE_TIME)}

#' Parse an ISO 8601 UTC timestamp string to POSIXct
#'
#' Parses timestamps in the form `"YYYY-MM-DDTHH:MM:SSZ"` into a POSIXct value
#' in the UTC timezone.
#'
#' @param x A character scalar (or vector) of ISO 8601 timestamps ending in `Z`.
#'
#' @return A POSIXct vector in timezone `"UTC"`.
#'
#' @examples
#' iso_utc("2026-01-12T21:00:00Z")
#' @export
iso_utc <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

#' Format a POSIXct time as an ISO 8601 UTC timestamp string
#'
#' Formats a POSIXct (or POSIXt) value as `"YYYY-MM-DDTHH:MM:SSZ"` using UTC.
#'
#' @param x A POSIXct/POSIXt vector.
#'
#' @return A character vector of ISO 8601 UTC timestamps ending in `Z`.
#'
#' @examples
#' iso_utc_str(as.POSIXct("2026-01-12 21:00:00", tz = "UTC"))
#' @export
iso_utc_str <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

#' Compute a due date-time from a semester anchor, week number, weekday, and time
#'
#' Generates a POSIXct deadline by starting from `day_one_monday` and adding:
#' (1) `week_num - 1` weeks, (2) an offset to the requested weekday, and
#' (3) the time-of-day parsed from `time`.
#'
#' @param day_one_monday POSIXct anchor corresponding to the Monday of week 1.
#'   Default is `iso_utc("2026-01-12T21:00:00Z")`.
#' @param week_num Integer week number (1 = the week containing `day_one_monday`).
#' @param weekday Character weekday name, one of
#'   `"Monday"`, `"Tuesday"`, `"Wednesday"`, `"Thursday"`, `"Friday"`, `"Saturday"`, `"Sunday"`.
#' @param time Character time-of-day in `"%I:%M %p"` format, e.g. `"6:00 PM"`.
#'
#' @return A POSIXct value (UTC) representing the computed due date-time.
#'
#' @details
#' - Weekday offsets are computed using `match()` against a fixed Monday..Sunday list.
#' - Time parsing uses `strptime(time, "%I:%M %p")` and converts hours + minutes
#'   to fractional hours.
#'
#' @examples
#' make_duedate(week_num = 1, weekday = "Friday", time = "6:00 PM")
#' @export
make_duedate <- function(day_one_monday = iso_utc("2026-01-12T21:00:00Z"),
                         week_num = 1,
                         weekday = "Friday",
                         time = "6:00 PM") {
  # Calculate the date of the deadline for a given week number
  day_one_monday +
    as.difftime(week_num - 1, units = "weeks") +
    as.difftime((match(weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) - 1) %% 7, units = "days") +
    as.difftime(strptime(time, "%I:%M %p")$hour + strptime(time, "%I:%M %p")$min / 60, units = "hours")
}
