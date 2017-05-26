#' @import shiny
#' @import miniUI
codefinch_app <- function(){
  ui <- miniPage(
    gadgetTitleBar("Codefinch: Tweet Your Code",
                   right = miniTitleBarButton("tweet_it", "Send Tweet", primary = TRUE)),
    miniContentPanel(
      fillRow(flex = c(2, 3),
        fillCol(
          uiOutput("status_box"),
          textOutput("chars"),
          checkboxInput("gist_url", "Include Gist URL in Image?"),
          selectInput("highlight", "Select Highlight", choices = c(
            "default", "tango", "pygments", "kate", "monochrome", 
            "espresso", "zenburn","haddock"
          )),
          actionButton("update_preview", "Update Code Preview")
        ),
        fillCol(imageOutput("code_png"))
      )
    )
  )
  
  server <- function(input, output){
    img_path <- tempfile(fileext = ".png")
    
    ctx <- getSourceEditorContext()
    path <- tempfile(fileext = ".R")
    raw_code <- ctx$contents
    writeLines(raw_code, path)
    
    gist_obj <- gist_create(path, browse = FALSE)
    
    observeEvent(input$update_preview, ignoreNULL = FALSE, {
      ctx <- getSourceEditorContext()
      raw_code <- ctx$contents
      writeLines(raw_code, path)
      
      gist_obj %>%
        update_files(path) %>%
        update()
      
      create_image(highlight = input$highlight, image_path = img_path, 
                   include_gist_url = input$gist_url,
                   gist_obj = gist_obj)
    })
    
    observeEvent(input$tweet_it, {
      create_image(highlight = input$highlight, image_path = img_path, 
                   include_gist_url = input$gist_url,
                   gist_obj = gist_obj)
      
      app <- oauth_app("twitter",
                       key = "cpYmlaSCcBKFBf7icpDT9sJww",
                       secret = "x8pvQAfziT3vVb8A1cQQB1xSBYdZ2ZhuRs4hTzifwLtoQf2iRF"
      )
      
      twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), app)
      
      if(!file.exists(img_path)){
        stop("WTF")
      }
      
      req <- POST("https://api.twitter.com/1.1/statuses/update_with_media.json",
                  config(token = twitter_token), body = list(
                    status = input$status_box,
                    media = upload_file(img_path)
                  ))
      
      stop_for_status(req)
      stopApp(returnValue = invisible(gist_obj$html_url))
    })
    
    # img <- reactive({
    #   create_image(highlight = input$highlight)
    # })
    
    output$status_box <- renderUI({
      textAreaInput("status", label = NULL, width = "100%", height = "100%",
                    resize = "both", value = gist_obj$html_url)  
    })
    
    output$code_png <- renderImage({
      input$update_preview
      list(src = img_path, width = "100%")
    })
    
    output$chars <- renderText({
      140 - nchar(input$status)
    })
  }
  
  runGadget(ui, server, viewer = paneViewer())
}
