

#' Create a Canvas assignment via the REST API
#'
#' Posts an assignment to the Canvas API for a given course.
#'
#' @param canvas A list-like object containing Canvas connection info, expected to
#'   include `base_url` and `api_key`.
#' @param course_id Canvas course ID.
#' @param assignment_name Assignment name/title.
#' @param position Numeric position in the assignment list.
#' @param submission_types Character vector of submission types (sent as
#'   `assignment[submission_types][]`), e.g. `"online_text_entry"`, `"online_upload"`.
#' @param points_possible Numeric points possible.
#' @param grading_type Grading type argument. Currently not used because the
#'   request sets `assignment[grading_type] = "points"` unconditionally.
#' @param due_at Due date-time string for Canvas (commonly ISO 8601 with `Z`).
#' @param unlock_at Unlock/open date-time string for Canvas.
#' @param lock_at Lock/close date-time string for Canvas.
#' @param published Logical; whether the assignment is published. Default TRUE.
#' @param allowed_attempts Integer; allowed attempts for quiz-like submissions.
#'   Default -1 (Canvas convention for unlimited attempts in some contexts).
#'
#' @return An `httr` response object from the POST request.
#'
#' @details
#' - The base URL is normalized by stripping trailing slashes.
#' - The request is sent as `multipart` form fields using Canvas-style parameter
#'   names like `assignment[name]`.
#' - `grading_type` is accepted but ignored in the payload (fixed to `"points"`).
#'
#' @examples
#' \dontrun{
#' canvas <- list(base_url = "https://canvas.instructure.com", api_key = Sys.getenv("CANVAS_TOKEN"))
#' make_assignment(canvas, 123, "Lab 1", position = 1,
#'                 submission_types = c("online_upload"),
#'                 points_possible = 10, grading_type = "points",
#'                 due_at = "2026-02-20T23:59:00Z",
#'                 unlock_at = "2026-02-13T00:00:00Z",
#'                 lock_at = "2026-02-21T23:59:00Z")
#' }
#' @importFrom httr POST add_headers
#' @export
make_assignment <- function(canvas, course_id, assignment_name,
                            position,
                            submission_types,
                            points_possible, grading_type = "points",
                            due_at,
                            unlock_at,
                            lock_at,
                            published = TRUE,
                            allowed_attempts = -1) {

  base <- sub("/+$", "", canvas$base_url)

  # Construct the API endpoint URL
  url <- paste0(base, "/api/v1/courses/", course_id,
                "/assignments/")

  # Create the request payload
  body <- list(
    `assignment[name]` = assignment_name,
    `assignment[position]` = position,
    `assignment[submission_types][]` = submission_types,
    `assignment[points_possible]` = points_possible,
    `assignment[grading_type]` = grading_type,
    `assignment[due_at]` = due_at,
    `assignment[unlock_at]` = unlock_at,
    `assignment[lock_at]` = lock_at,
    `assignment[published]` = published,
    `assignment[allowed_attempts]` = allowed_attempts
  )

  # Make the API request
  response <- httr::POST(
    url,
    httr::add_headers(Authorization = paste("Bearer", canvas$api_key)),
    body = body,
    encode = "multipart"
  )

  response
}

#' Create an assignment override for a specific student in Canvas
#'
#' Posts an assignment override (typically to adjust due dates per-student) to
#' the Canvas API.
#'
#' @param canvas A list-like object containing Canvas connection info, expected to
#'   include `base_url` and `api_key`.
#' @param course_id Canvas course ID.
#' @param assignment_id Canvas assignment ID.
#' @param student_id Canvas user ID for the student.
#' @param title Override title (Canvas UI label).
#' @param due_at Due date-time string for the override (commonly ISO 8601 with `Z`).
#' @param lock_at Optional lock date-time string for the override. Default NULL.
#'
#' @return An `httr` response object from the POST request.
#'
#' @examples
#' \dontrun{
#' create_override(canvas, 123, 456, 789, "Extension", "2026-02-21T23:59:00Z")
#' }
#' @importFrom httr POST add_headers
#' @export
create_override <- function(canvas, course_id, assignment_id,
                            student_id, title, due_at, lock_at = NULL) {
  base <- sub("/+$", "", canvas$base_url)
  url  <- paste0(base, "/api/v1/courses/", course_id,
                 "/assignments/", assignment_id, "/overrides")
  body <- list(
    `assignment_override[student_ids][]` = student_id,
    `assignment_override[title]`         = title,
    `assignment_override[due_at]`        = due_at
  )
  if (!is.null(lock_at)) body[["assignment_override[lock_at]"]] <- lock_at
  httr::POST(
    url,
    httr::add_headers(Authorization = paste("Bearer", canvas$api_key)),
    body = body,
    encode = "multipart"
  )
}
