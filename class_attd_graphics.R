library(shiny)
library(data.table)
library(stringr)
library(dplyr)
library(readxl)
#library(xlsx)

name_search <- function(str1,list1){
  L = length(list1)
  k = rep(0,L)
  
  for(i in 1:L){
    k[i] = as.integer(str_detect(list1[i],str1))
  }
  
  if(sum(k) == 1){
    return (1)
  }else{
    return (0)
  }
  
}


data_proc <-function(fName_attdn, fName_roster, actual_fname, str1){
  
  col_name = str_split(basename(actual_fname),".txt")[[1]][1]
  attd = readLines(fName_attdn, skipNul = TRUE)
  attd1 = paste0(attd,collapse = "")
  if(str_detect(attd1,str1) == FALSE)
    return(NULL)
  
  in_class_rid_tmp = attd[which(attd == "avatar")+1]
  if(str1 == ""){
    str1 = "Debajit Goswami"
  }
  in_class_rid = in_class_rid_tmp[-which(in_class_rid_tmp == str1)]
  
  #df = fread(fName_roster)
  df = readxl::read_excel(path = fName_roster, sheet=1)
  df$attdn1 = as.integer(unlist(lapply(df$`REGISTER NO`, function(x){name_search(x, toupper(in_class_rid))})))
  
  h = dim(df)[2]
  df1 = df %>% filter(attdn1 == 1) 
  
  colnames(df)[h] = col_name
  df1 = df1[, c(2,3)]
  
  out_basename = sprintf("modified_%s.csv", col_name)
  out_fName = file.path(dirname(fName_attdn), out_basename)

  #fwrite(df, file = out_fName, sep = ",")
  df_and_fname = list(df, df1, out_fName)
  
  return(df_and_fname)
}

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Update Attendance File"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("fileRoster", "Choose Class Roster (.xlsx)",
                multiple = FALSE,
                accept = c(".xlsx")),
      
      fileInput("fileAttdn", "Choose TeamLink Attendance file (.txt)",
                multiple = FALSE,
                accept = c(".txt")),
      
      textInput("hostName", "Please input your TeamLink Participant Name:"),
      
      #br(),
      
      # Horizontal line ----
      tags$hr(),
      
      p("Update the attendance"),
      actionButton("gen", "Generate Output File"),
      
      tags$hr(),
      downloadButton("downloadFile", "Download File")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h4(textOutput("roster_file")),
      h4(textOutput("attdn_file")),
      tags$hr(),
      
      h3(textOutput("listStr")),
      tags$hr(),
      tableOutput("attdn"),
      tags$hr(),
      
      h4(textOutput("fout_name"))
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  output$roster_file <- renderText({sprintf("Roster file: %s", input$fileRoster$name)})
  output$attdn_file  <- renderText({sprintf("Attendance file: %s", input$fileAttdn$name)})

  gen_output <- eventReactive(input$gen, {
    df_and_fname = data_proc(input$fileAttdn$datapath, input$fileRoster$datapath, 
                             input$fileAttdn$name, input$hostName)
    
    df = df_and_fname[[1]]
    df1 = df_and_fname[[2]]
    fout_name = df_and_fname[[3]]
    
    output$listStr = renderText({sprintf("Attendee: %d Students", dim(df1)[1])})
    
    output$downloadFile <- downloadHandler(
      filename = fout_name,
      content = function(file){fwrite(x=df, file, sep=",")}
    )
    
    return(df1)
  })
  
  gen_fout_name <- eventReactive(input$gen, {
    sprintf("Output file: modified_%s.csv", str_split(basename(input$fileAttdn$name),".txt")[[1]][1])
  })
  
  output$fout_name <- renderText({ 
    gen_fout_name()
  })
  
  output$attdn <- renderTable({
    req(input$fileRoster)
    req(input$fileAttdn)
    df1 <- gen_output()
    return(df1)
  })
  
}
# Run the app ----
attdn_app = shinyApp(ui, server)
runApp(attdn_app)
#runApp(attdn_app, host="127.0.0.1", port = 19579, launch.browser = TRUE)

