
#' Construct Slide URL
#'
#' Constructs a URL for slides from a dataframe based on the title and optional specific slide number.
#' @param df_url A dataframe containing columns 'link' and 'title'.
#' @param title Title of the slide to match in 'df_url'.
#' @param slide Optional specific slide number to append to the URL.
#' @return Returns the full URL constructed from the dataframe for the specified title and slide.
#' @examples
#' url <- slide_url(df_slides, "Introduction", 1)
#' @export
slide_url <- function(df_url, title, slide = NULL) {
  var_url <- paste0(df_url$link[df_url$title == title], slide)
  return(var_url)
}

#' Try Including a Tweet
#'
#' Attempts to include a tweet into a knitr document, handling failures quietly.
#' @param tweet_url URL of the tweet to include.
#' @param plain Logical indicating whether to display tweet in plain format.
#' @param ... Additional arguments passed to tweetrmd::include_tweet.
#' @return Returns the result of attempting to include the tweet, or an error object silently.
#' @examples
#' tweet <- try_include_tweet("https://twitter.com/user/status/1234567890")
#' @export
try_include_tweet <- function(tweet_url, plain = FALSE, ...) {
  return(try(tweetrmd::include_tweet(tweet_url = tweet_url, plain = plain, ...),
             silent = TRUE))
}

#' Embed YouTube Video Alternatively
#'
#' Embeds a YouTube video or its thumbnail based on output format.
#' @param youtube_id YouTube video ID to embed.
#' @return Returns an iframe for the video if HTML output (excluding ePub) or an image if other formats.
#' @examples
#' video <- embed_youtube_alt("dQw4w9WgXcQ")
#' @export
embed_youtube_alt <- function(youtube_id) {
  if (knitr::is_html_output(excludes = "epub")) {
    url <- stringr::str_c("https://www.youtube.com/embed/", youtube_id)
    return(knitr::include_url(url))
  } else {
    # Download thumbnail and use that
    dir_path <- 'img/youtube'
    if (!dir.exists(dir_path)) dir.create(dir_path)
    file_path <- stringr::str_c(dir_path, '/', youtube_id, '.jpg')
    if (!file.exists(file_path)) webshot::webshot(str_c("https://img.youtube.com/vi/", youtube_id, "/mqdefault.jpg"), vwidth = 320, vheight=180, file = file_path)
    return(knitr::include_graphics(str_c(file_path)))
  }
}
