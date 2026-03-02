
#' Convert a GitHub repo URL to the raw URL for contract.md
#'
#' Transforms a repository URL like `https://github.com/user/repo` into the raw
#' GitHub content URL for `contract.md` on the `main` branch.
#'
#' @param repo_url GitHub repository URL.
#'
#' @return A character scalar raw-content URL of the form
#'   `https://raw.githubusercontent.com/user/repo/main/contract.md`.
#'
#' @examples
#' repo_to_raw("https://github.com/user/repo")
#' @export
repo_to_raw <- function(repo_url) {
  # https://github.com/user/repo -> https://raw.githubusercontent.com/user/repo/main/contract.md
  repo_url <- sub("/$", "", repo_url)
  sub("https://github.com/", "https://raw.githubusercontent.com/", repo_url,
      fixed = TRUE) |>
    paste0("/main/contract.md")
}


# R/contracts-fetch.R

#' Fetch a student's contract.md from a GitHub repository
#'
#' Downloads `contract.md` from a GitHub repository using the raw-content URL.
#' Attempts `main` first, then falls back to `master` if needed.
#'
#' @param repo_url GitHub repository URL (for example, `https://github.com/user/repo`).
#'
#' @return A UTF-8 character string containing the file contents, or `NA_character_`
#'   on failure (request error or non-200 HTTP status).
#'
#' @examples
#' \dontrun{
#' fetch_contract("https://github.com/user/repo")
#' }
#' @importFrom httr GET status_code content
#' @export
fetch_contract <- function(repo_url) {
  raw_url <- repo_to_raw(repo_url)
  resp <- tryCatch(httr::GET(raw_url), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) {
    # Try master branch as fallback
    raw_url <- sub("/main/contract.md$", "/master/contract.md", raw_url)
    resp <- tryCatch(httr::GET(raw_url), error = function(e) NULL)
  }
  if (is.null(resp) || httr::status_code(resp) != 200) return(NA_character_)
  httr::content(resp, as = "text", encoding = "UTF-8")
}

# R/contracts-parse.R

#' Parse an MM/DD date string into a Date using a fixed year
#'
#' Takes a month/day string such as `"3/7"` or `"03/07"` (optionally with
#' surrounding whitespace) and produces a Date by combining it with `year`.
#'
#' @param md_str Character scalar month/day string in `M/D` or `MM/DD` format.
#' @param year Integer year to prepend. Default is `SEMESTER_YEAR`.
#'
#' @return A Date.
#'
#' @details
#' This function assumes `SEMESTER_YEAR` exists in the package namespace when
#' `year` is not supplied explicitly.
#'
#' @examples
#' parse_md_date("3/7", year = 2026)
#' @export
parse_md_date <- function(md_str, year = SEMESTER_YEAR) {
  md_str <- trimws(md_str)
  as.Date(sprintf("%d/%s", year, md_str), format = "%Y/%m/%d")
}

#' Extract per-lab deadlines from contract markdown
#'
#' Scans contract markdown text for lab deadline lines like
#' `"Finish Lab 3 by Friday: 02/20"` and returns a tibble of lab numbers and due dates.
#'
#' @param md_text Character scalar containing the markdown text.
#'
#' @return A tibble with columns:
#' - `num` (integer): lab number
#' - `due_date` (Date): parsed due date
#'
#' @details
#' Matching uses a case-insensitive regex intended to capture variants of:
#' `"Finish Lab N by <Day>: MM/DD"`.
#'
#' @examples
#' extract_lab_dates("Finish Lab 1 by Friday: 02/20")
#' @importFrom stringr str_extract_all str_extract regex
#' @importFrom tibble tibble
#' @export
extract_lab_dates <- function(md_text) {
  lines <- stringr::str_extract_all(
    md_text,
    stringr::regex("Finish\\s*Lab\\s*\\d+\\s*by\\s*[\\w\\s]+:?\\s*\\d{1,2}/\\d{1,2}", ignore_case = TRUE)
  )[[1]]

  if (length(lines) == 0) {
    return(tibble::tibble(num = integer(), due_date = as.Date(character())))
  }

  nums  <- as.integer(stringr::str_extract(lines, "(?<=Lab )\\d+"))
  dates <- stringr::str_extract(lines, "\\d{1,2}/\\d{1,2}$")

  tibble::tibble(
    num = nums,
    due_date = map_vec(dates, parse_md_date)
  )
}

#' Extract per-portfolio deadlines from contract markdown
#'
#' Scans contract markdown text for portfolio deadline lines and returns a tibble
#' of piece numbers and due dates. Handles both single-piece and multi-piece lines.
#'
#' @param md_text Character scalar containing the markdown text.
#'
#' @return A tibble with columns:
#' - `num` (integer): portfolio piece number
#' - `due_date` (Date): parsed due date
#'
#' @details
#' Supported patterns include:
#' - Single: `"Finish portfolio piece 3 by Friday: 04/10"`
#' - Multi: `"Finish portfolio pieces 7 and 8 by Friday: 04/10"`
#'
#' The multi-piece parser extracts all digits and attempts to treat the trailing
#' `MM/DD` digits as the date, mapping the remaining numbers to piece numbers.
#'
#' @examples
#' extract_portfolio_dates("Finish portfolio pieces 7 and 8 by Friday: 04/10")
#' @importFrom stringr str_extract_all str_extract regex
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @export
extract_portfolio_dates <- function(md_text) {
  # Single-piece lines
  single <- stringr::str_extract_all(
    md_text,
    stringr::regex(
      "Finish\\s*portfolio\\s*piece\\s*\\d+\\s*by\\s*[\\w\\s]+:?\\s*\\d{1,2}/\\d{1,2}",
      ignore_case = TRUE
    )
  )[[1]]
  s_nums  <- as.integer(stringr::str_extract(single, "(?i)(?<=portfolio piece )\\d+"))
  s_dates <- stringr::str_extract(single, "\\d{1,2}/\\d{1,2}$")

  # Multi-piece lines like "Finish portfolio pieces 7 and 8 by Friday: 04/10"
  multi <- stringr::str_extract_all(
    md_text,
    stringr::regex(
      "Finish\\s*portfolio\\s*pieces?\\s*\\d+\\s+and\\s+\\d+ by\\s*[\\w\\s]+:?\\s*\\d{1,2}/\\d{1,2}",
      ignore_case = TRUE
    )
  )[[1]]

  m_nums <- list()
  m_dates <- character()
  for (line in multi) {
    pair <- as.integer(stringr::str_extract_all(line, "\\d+")[[1]])
    # Last two numbers before the date are the piece numbers
    date_str <- stringr::str_extract(line, "\\d{1,2}/\\d{1,2}$")
    piece_nums <- pair[!pair %in% as.integer(stringr::str_extract_all(date_str, "\\d+")[[1]])]
    for (p in piece_nums) {
      m_nums <- c(m_nums, p)
      m_dates <- c(m_dates, date_str)
    }
  }

  all_nums  <- c(s_nums, unlist(m_nums))
  all_dates <- c(s_dates, m_dates)
  if (length(all_nums) == 0) return(tibble::tibble(num = integer(), due_date = as.Date(character())))

  tibble::tibble(
    num = as.integer(all_nums),
    due_date = map_vec(all_dates, parse_md_date)
  ) |>
    dplyr::arrange(num)
}
