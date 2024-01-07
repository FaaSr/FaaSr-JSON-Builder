ui_1 <- function(input, output, session, json){
  # create a uuid for Invocation ID
  exampleid <- uuid::UUIDgenerate()
  
  # set the default value for Log folder name: FaaSrLog
  if (is.null(json$FaaSrLog)){
    json$FaaSrLog <- "FaaSrLog"
  }
  
  # no selection will return nothing
  if (is.null(input$select1))
    return()
  
  switch (input$select1,
          # if select1 is "FunctionList", give a text input for func_name
          # and it calls ui5
          "Functions" = list(
            textInput("func_name", "Action Name:", placeholder= "Action_1"),
            uiOutput("ui5"),
            fluidRow(
              column(6,
                     div(id="funcapp", 
                         actionButton("func_apply", label = "Apply", onclick = "Shiny.setInputValue('func_app', true, {priority:'event'});")
                     )),
              column(6,
                     div(style = "position:absolute;right:1em;", 
                         actionButton("func_delete", label = "Delete")))
            )
          ),
          # if select1 is "Data Server", give a text input for data_name
          # and it calls ui3
          "Data Server" = list(
            textInput("data_name", "Data Server Name:", placeholder = "Initial_data_server_name"),
            uiOutput("ui3"),
            fluidRow(
              column(6,
                     div(id="dataapp", 
                         actionButton("data_apply", label = "Apply", onclick = "Shiny.setInputValue('data_app', true, {priority:'event'});")
                     )),
              column(6,
                     div(style = "position:absolute;right:1em;", 
                         actionButton("data_delete", label = "Delete")))
            )
          ),
          # if select1 is "FaaS Server", give a text input for faas_name
          # and it calls ui4
          "FaaS Server" = list(
            textInput("faas_name", "FaaS Server Name:", placeholder = "Initial_faas_server_name"),
            uiOutput("ui4"),
            fluidRow(
              column(6,
                     div(id="faasapp", 
                         actionButton("faas_apply", label = "Apply", onclick = "Shiny.setInputValue('faas_app', true, {priority:'event'});")
                     )),
              column(6,
                     div(style = "position:absolute;right:1em;", 
                         actionButton("faas_delete", label = "Delete")))
            )
          ),
          # if select1 is "General Configuration", give text inputs and select inputs.
          "General Configuration" = list(
            selectInput("function_invoke", "First Function to be executed:", names(json$FunctionList), selected=json$FunctionInvoke),
            selectInput("logging_data_server", "Logging Data Server to leave logs:", c("None",names(json$DataStores)), selected=json$LoggingDataStore),
            selectInput("default_data_server", "Default Data Server to be used in put/get/arrow/log:", names(json$DataStores), selected=json$DefaultDataStore),
            textInput("faasr_log", "Log file name(Optional, default=FaaSrLog):", value = json$FaaSrLog, placeholder = "FaaSrLog"),
            textInput("invocation_id", "InvocationID(Optional, must follow UUID format):", value = json$InvocationID, placeholder = exampleid),
            fluidRow(
              column(6,
                     div(id="genapp", 
                         actionButton("gen_apply", label = "Apply", onclick = "Shiny.setInputValue('gen_app', true, {priority:'event'});")
                     )),
              column(6,
                     div(style = "position:absolute;right:1em;", 
                         actionButton("gen_delete", label = "Delete")))
            )
          )
  )
}


ui_2 <- function(input, output, session, json){
  return(
    list(
      selectInput("func_faas", "Function FaaS Server:", names(json$ComputeServers), selected = json$FunctionList[[input$func_name]]$FaaSServer),
      textAreaInput("func_args", "Function Arguments:", value = unretrieve(json$FunctionList[[input$func_name]]$Arguments), placeholder = "arg1=input1.csv,\narg2=input2.csv", height = "100px", resize = "vertical"),
      textInput("func_next", "Next Actions to Invoke:", value = unretrieve(json$FunctionList[[input$func_name]]$InvokeNext), placeholder = "Action_2, Action_3"),
      textInput("func_container", "Function's Action Container(Optional):", value= json$ActionContainers[[input$func_name]], placeholder = "faasr/github-actions-tidyverse"),
      textInput("func_gh_repo", "Repository/Path, where the function is stored:", value = unretrieve(json$FunctionGitRepo[[input$func_act]]), placeholder = "username/reponame, https://url.git"),
      textInput("func_gh_package", "Dependencies - Github Package for the function:", value = unretrieve(json$FunctionGitHubPackage[[input$func_act]]), placeholder = "username/package_reponame"),
      textInput("func_cran_repo", "Dependencies - Repository/Path for the function:", value = unretrieve(json$FunctionCRANPackage[[input$func_act]]), placeholder = "CRAN_package_name, dplyr")
    )
  )
}

ui_3 <- function(input, output, session, json){
  return(
    list(
      textInput("data_endpoint", "Data Server Endpoint(optional for s3):", value = json$DataStores[[input$data_name]]$Endpoint, placeholder = "https://play.min.io"),
      textInput("data_bucket", "Data Server Bucket:", value = json$DataStores[[input$data_name]]$Bucket, placeholder = "my_bucket"),
      textInput("data_region", "Data Server Region (optional for minio):", value = json$DataStores[[input$data_name]]$Region, placeholder = "us-east-1"),
      selectInput("data_writable", "Data Server Writable permission:", c(TRUE, FALSE), selected = json$DataStores[[input$data_name]]$Writable)
    )
  )
}

ui_4 <- function(input, output, session, json){
  return(
    list(
      selectInput("faas_type", "Select type:", c("GitHubActions", "OpenWhisk", "Lambda"), selected = json$ComputeServers[[input$faas_name]]$FaaSType),
      conditionalPanel(
        condition = "input.faas_type == 'GitHubActions'",
        textInput("faas_gh_user", "GitHubActions User name:", value = json$ComputeServers[[input$faas_name]]$UserName, placeholder = "user_name"),
        textInput("faas_gh_repo", "GitHubActions Action Repository name:", value = json$ComputeServers[[input$faas_name]]$ActionRepoName, placeholder = "repo_name"),
        textInput("faas_gh_ref", "GitHubActions Branch:", value = json$ComputeServers[[input$faas_name]]$Branch, placeholder = "main")
      ),
      conditionalPanel(
        condition = "input.faas_type == 'Lambda'",
        textInput("faas_ld_region", "Lambda Region:", value = json$ComputeServers[[input$faas_name]]$Region, placeholder = "us-east-1")
      ),
      conditionalPanel(
        condition = "input.faas_type == 'OpenWhisk'",
        textInput("faas_ow_end", "OpenWhisk Endpoint:", value = json$ComputeServers[[input$faas_name]]$Endpoint, placeholder = "https://00.00.00.00"),
        textInput("faas_ow_name", "OpenWhisk Namespace:", value = json$ComputeServers[[input$faas_name]]$Namespace, placeholder = "namespace_name"),
        textInput("faas_ow_region", "OpenWhisk Region:", value = json$ComputeServers[[input$faas_name]]$Region, placeholder = "us-west")
      )
    )
  )
}

ui_5 <- function(input, output, session, json){
  return(
    list(
      # it shows a text input for func_act
      # it also calls ui2
      textInput("func_act", "Function Name:", value= json$FunctionList[[input$func_name]]$FunctionName, placeholder = "function_1"),
      uiOutput("ui2")
    )
  )
}