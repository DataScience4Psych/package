#' Advance Date Calculation (v0 - Legacy Version)
#'
#' This function calculates and formats dates for each day of a specified week
#' (older implementation).
#' @param weekonemonday The date of the first Monday of the schedule in "YYYY-MM-DD" format.
#' @param week The week number for which the dates need to be calculated.
#' @param topic A vector of topics for each week. Default is NULL.
#' @param assignment The specific assignment day, e.g., "Friday". Default is NULL.
#' @param unit The unit prefix for the week number. Default is "Module ".
#'
#' @return A formatted string indicating the dates for the specified week and optional topic or assignment day.

advdatev0 <- function(weekonemonday,
                    week,
                    topic =  NULL,
                    assignment = NULL,
                    unit = "Module ") {
  # as.Date does not like piping
  tmon <- format(base::as.Date(weekonemonday + 7 * (week - 1)),format = "%m/%d")
  ttue <- format(base::as.Date(weekonemonday + 1 + 7 * (week - 1)),format = "%m/%d")
  twed <- format(base::as.Date(weekonemonday + 2 + 7 * (week - 1)),format = "%m/%d")
  tthu <- format(base::as.Date(weekonemonday + 3 + 7 * (week - 1)),format = "%m/%d")
  tfri <- format(base::as.Date(weekonemonday + 4 + 7 * (week - 1)),format = "%m/%d")
  tsat <- format(base::as.Date(weekonemonday + 5 + 7 * (week - 1)),format = "%m/%d")
  tsun <- format(base::as.Date(weekonemonday + 6 + 7 * (week - 1)),format = "%m/%d")
  zadv <- sprintf("%02d", week)


  if (is.null(topic) & is.null(assignment)) {
    tmp <- paste0(unit, zadv, ", ", tmon, " - ", tfri)
  } else if (!is.null(assignment)) {
    if (assignment %in%
        c("Friday",
          "friday",
          "f",
          "fri",
          "FRI")) {
      tmp <- paste0(tfri)
    } else if (assignment %in%
               c("thursday",
                 "Thursday",
                 "thurs",
                 "r",
                 "R")) {
      tmp <- paste0(tthu)
    } else if (assignment %in%
               c("wednesday",
                 "Wednesday",
                 "w",
                 "wed",
                 "W")) {
      tmp <- paste0(twed)
    } else if (assignment %in%
               c("Tuesday",
                 "tues",
                 "t",
                 "Tue")) {
      tmp <- paste0(ttue)
    } else if (assignment %in%
               c("monday",
                 "Monday",
                 "m",
                 "mon")) {
      tmp <- paste0(tmon)
    }
  } else{
    tmp <- paste0(unit, zadv, ", ", tmon, " - ", tfri, " : ", topic[week])
  }
  return(tmp)
}
#' Advance Date Calculation
#'
#' This function calculates and formats dates for each day of a specified week, based on the start date of the first Monday.
#'
#' @param weekonemonday The date of the first Monday of the schedule in "YYYY-MM-DD" format.
#' @param week The week number for which the dates need to be calculated.
#' @param topic A vector of topics for each week. Default is NULL.
#' @param assignment The specific assignment day, e.g., "Friday". Default is NULL.
#' @param unit The unit prefix for the week number. Default is "Module ".
#' @param version The version of the function to use (0 or 1). Default is 1. Allows for older version to be used.
#' @param ... Additional arguments to be passed to the function.
#'
#' @return A formatted string indicating the dates for the specified week and optional topic or assignment day.
#' @export
#'
#' @examples
#' advdate("2023-01-09", 1)
#' advdate("2023-01-09", 2, topic = c("Intro", "Advanced Topics"))
#' advdate("2023-01-09", 3, assignment = "Friday")
#'
advdate <- function(weekonemonday,
                    week,
                    topic = NULL,
                    assignment = NULL,
                    unit = "Module ",
                    version = 1,
                    ... ) {


# Use the older version if specified
if (version == 0 & !is.null(version)) {
  return(advdatev0(weekonemonday=weekonemonday,
                                         week = week,
                                         topic =  topic,
                                         assignment = assignment,
                                         unit = unit))
} else {

  # Validate inputs
  tryCatch({
    start_date <- as.Date(weekonemonday)
    if (is.na(start_date)) stop("Invalid date provided for weekonemonday.")
  }, error = function(e) {
    stop("weekonemonday must be a valid date in 'YYYY-MM-DD' format.")
  })

  if (!version %in% c(0, 1)) {
    stop("Invalid version specified. Please use 0 or 1.")
  }

  if (!inherits(weekonemonday, "Date") && (!is.character(weekonemonday) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", weekonemonday))) {
    stop("weekonemonday must be a Date object or a character string in 'YYYY-MM-DD' format.")
  }

    if (!is.numeric(week) || week < 0) {
      stop("week must be a non-negative integer.")
    }

    if (!is.null(topic) && (!is.vector(topic) || length(topic) < week)) {
      warning("topic must be a vector with length at least as long as the week number.")
    }

  # Define day mapping
  day_map <- c(
    "mon" = "Mon", "tue" = "Tue", "wed" = "Wed", "thu" = "Thu", "fri" = "Fri",
    "sat" = "Sat", "sun" = "Sun", "m" = "Mon", "t" = "Tue", "w" = "Wed",
    "r" = "Thu", "f" = "Fri", "s" = "Sat", "u" = "Sun"
  )

  # Validate assignment using day_map
  if (!is.null(assignment)) {
    day_abbr <- day_map[tolower(substr(assignment, 1, 3))]
    if (is.na(day_abbr)) stop("Invalid assignment day provided.")
  }


    # Calculate dates
    start_date <- base::as.Date(weekonemonday)
    dates <- start_date + 0:6 + 7 * (week - 1)
    formatted_dates <- format(dates, "%m/%d")
    names(formatted_dates) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

    # Format the output
    if (is.null(topic) && is.null(assignment)) {
      return(paste0(unit, sprintf("%02d", week), ", ", formatted_dates["Mon"], " - ", formatted_dates["Fri"]))
    }
    if (!is.null(topic) && length(topic) < week) {
      warning("Topic not defined for the specified week. Defaulting to 'No topic'.")
      topic <- c(topic, rep("No topic", week - length(topic)))
    }
    # Return the assignment day
    if (!is.null(assignment)) {
      return(unname(formatted_dates[day_abbr]))
    }

    if (!is.null(topic)) {
      return(paste0(unit, sprintf("%02d", week), ", ", formatted_dates["Mon"], " - ", formatted_dates["Fri"], " : ", topic[week]))
    }
  }

}

