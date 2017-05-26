# Tweets Code and Includes
#' @importFrom rstudioapi getSourceEditorContext isAvailable
#' @import gistr
#' @importFrom rmarkdown render pdf_document
#' @importFrom pdftools pdf_render_page
#' @importFrom magick image_read image_trim image_write
#' @import httr
cf_snippet <- function(tweet_text, path = NULL, include_gist_url = FALSE,
                       highlight = "default", image_path = NULL){
  # call it "codefinch"
  # include results!?!?
  
  to_render <- tempfile(fileext = ".Rmd")
  preamble <- c("---",
                "output: pdf_document",
                "---",
                "\\pagenumbering{gobble}",
                "```{r eval=FALSE}")
  
  if(is.null(path)){ # Use RStudio Context
    if(!isAvailable()){
      stop("Please provide a path to an R code file.")
    } else {
      ctx <- getSourceEditorContext()
      path <- tempfile(fileext = ".R")
      raw_code <- ctx$contents
      writeLines(raw_code, path)
    }
  } else { # Use a file
    raw_code <- readLines(path)
  }
  
  writeLines(c(preamble, raw_code, "```"), to_render)
  
  g <- gist_create(path, browse = FALSE)
  
  if(include_gist_url){
    cat(sprintf("gist: [%s](%s)", g$url, g$url), file = to_render, append = TRUE)
  }
  
  temp_pdf <- tempfile(fileext = ".pdf")
  render(to_render, pdf_document(highlight = highlight), temp_pdf)
  code_in_pdf <- pdf_render_page(temp_pdf, dpi=144)
  code_image <- image_trim(image_read(code_in_pdf))
  
  if(is.null(image_path)){
    image_path <- tempfile(fileext = ".png")
  }
  
  image_write(code_image, image_path)
  
  app <- oauth_app("twitter",
                     key = "cpYmlaSCcBKFBf7icpDT9sJww",
                     secret = "x8pvQAfziT3vVb8A1cQQB1xSBYdZ2ZhuRs4hTzifwLtoQf2iRF"
  )
  
  twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), app)
  
  req <- POST("https://api.twitter.com/1.1/statuses/update_with_media.json",
              config(token = twitter_token), body = list(
                status = paste0(tweet_text, "\n", g$html_url),
                media = upload_file(image_path)
              ))
  
  stop_for_status(req)
  invisible()
}

# Returns path to code image
create_image <- function(path = NULL, include_gist_url = FALSE,
                         highlight = "default", image_path = NULL,
                         gist_obj = NULL){
  to_render <- tempfile(fileext = ".Rmd")
  preamble <- c("---",
                "output: pdf_document",
                "---",
                "\\pagenumbering{gobble}",
                "```{r eval=FALSE}")
  
  if(is.null(path)){ # Use RStudio Context
    if(!isAvailable()){
      stop("Please provide a path to an R code file.")
    } else {
      ctx <- getSourceEditorContext()
      path <- tempfile(fileext = ".R")
      raw_code <- ctx$contents
      writeLines(raw_code, path)
    }
  } else { # Use a file
    raw_code <- readLines(path)
  }
  
  writeLines(c(preamble, raw_code, "```"), to_render)
  
  if(include_gist_url){
    cat(sprintf("gist: [%s](%s)", gist_obj$html_url, gist_obj$html_url),
        file = to_render, append = TRUE)
  }
  
  temp_pdf <- tempfile(fileext = ".pdf")
  render(to_render, pdf_document(highlight = highlight), temp_pdf)
  code_in_pdf <- pdf_render_page(temp_pdf, dpi=144)
  code_image <- image_trim(image_read(code_in_pdf))
  
  if(is.null(image_path)){
    image_path <- tempfile(fileext = ".png")
  }
  
  image_write(code_image, image_path)
  
  image_path
}