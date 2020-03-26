#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "style.css",
                
                tags$style('.container-fluid {
                             background-color: #FDF2E9; }'),
                
                # Application title
                titlePanel("Talk to the bot"),
                
                fluidRow(
                    column(12,
                           htmlOutput("distConversation")
                    ), 
                    column(12,
                           textInput("text_entry", "", ""),
                           tags$head(
                               tags$style(HTML('#send_button{
                                               background-color: #2874A6;
                                               color: #FFF;
                                               font-weight: bold;}'))
                           ),
                           actionButton("send_button", "send ➣")
                    )
                ),
                br()
)


get_bot_reply <- function(script_df, this_message){
    whole_reply <- ''
    if (this_message != '') {
        this_message_parts <- strsplit(this_message, split = '\\!|\\,|\\?|\\.')[[1]]
        
        for (i in c(1:nrow(script_df))){
            expression <- script_df$message[i]
            for (message_part in this_message_parts) {
                if (grepl(expression, message_part, ignore.case = TRUE)){
                    this_reply <- script_df$reply[i]
                    rest_of_reply <- str_trim(gsub(expression, '', message_part,
                                                   ignore.case = TRUE))
                    this_reply <- gsub('<replace>', rest_of_reply, this_reply)
                    if (script_df$include_with_other[i]){
                        whole_reply <- paste(whole_reply, this_reply)
                    } else if (whole_reply == ''){
                        whole_reply <- paste(whole_reply, this_reply)
                    }
                }
            }
        }
        
    }
    
    if (whole_reply == '' & this_message != '') {
        whole_reply <- paste0("I'm sorry. I don't understand what you mean by ",
                             '"', this_message, '"')
    }
    
    return(whole_reply)
}

conversation_string <- "<bot_message>Hi, I'm a bot. How are?</bot_message>"
bot_script <- read_excel('bot_script.xlsx')

server <- function(input, output, session) {
    
    session$onSessionEnded(stopApp)
    
    output$distConversation <- renderUI({
        input$send_button
        new_message <- isolate(input$text_entry)
        new_reply <- get_bot_reply(bot_script, new_message)
        if (new_reply != ''){
            conversation_string <<- paste(conversation_string,
                                          '<br><user_message>', 
                                          new_message, 
                                          '</user_message><br><bot_message>',
                                          new_reply, '</bot_message>')
            isolate({
                updateTextInput(session, "text_entry", value = '')
            })
        }
       
        HTML(conversation_string)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
