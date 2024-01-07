library("shiny")
library("visNetwork")
library("shinyjs")
library("shinyWidgets")
library("jsonlite")

source("dependencies/visnetwork_drawing.R")
source("dependencies/visnetwork_dataframe.R")
source("dependencies/helper.R")
source("dependencies/render_ui_list.R")
source("dependencies/action_button_delete.R")
source("dependencies/action_button_apply.R")

server <- function(input, output, session) {
  
  
  
  ##################################################
  # JSON file upload/download/delete & Session close
  #
  ##################################################
  # json_data will be reactive
  json_data <- reactiveVal(NULL)  
  
  # this is an upload button implementation. 
  # if there's input, set the json_data with given json file.
  observe({
    if (!is.null(input$file1)) {
      json_path <- input$file1$datapath
      json <- readLines(json_path)
      if (!jsonlite::validate(json)){
        sendSweetAlert(
          title = "JSON invalid!",
          text = "Given JSON file is invalid",
          type = "error"
        )
        json_data(NULL)
      } else {
        json_data(json)
      }
    }
  })
  
  # this is a download button implementation. 
  # if user pushes download button, it will create a json file with current json_data
  output$downjson <- downloadHandler(
    filename = function() {
      "payload.json"
    },
    content = function(file) {
      json_result <- json_data()
      if (!is.null(json_result)) {
        writeLines(json_result, file)
      }
    }
  )
  
  # if user pushes file_del, it will remove json_data values
  observeEvent(input$file_del, {
    json_data(NULL)
  })
  
  # if user pushes exit, it will close the window and return json
  observeEvent(input$close, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    #if (input$close > 0) stopApp()  
  })
  
  # 
  ##########################################
  # renderUI configurations: repetitive UIs
  # source: "render_ui_list.R"
  ##########################################
  # ui1 is for select input for "FunctionList", "Data Server", "FaaS Server", and "General Configuration"
  output$ui1 <- renderUI({
    
    # bring the json_data if it's not empty
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    ui_1(input, output, session, json)
  })
  
  # ui2 is for FunctionList
  # Read the json data with given func_act, separating ui is required.
  output$ui2 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    ui_2(input, output, session, json)
  })
  
  # ui3 is for data servers.
  output$ui3 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    ui_3(input, output, session, json)
  })
  
  # ui4 is for faas servers
  output$ui4 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    ui_4(input, output, session, json)
  })
  
  # ui5 is for action name
  output$ui5 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    ui_5(input, output, session, json)
  })
  
  ##################################
  # Delete button configurations
  # source: "action_button_delete.R"
  ##################################
  # if user pushes func_delete button, it will remove that function.
  observeEvent(input$func_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else {
      json <- list()
    }
    json_pretty <- ob_func_delete(input, output, session, json)
    json_data(json_pretty)
  })
  
  # if user pushes faas_delete button, it will remove that faas server.
  observeEvent(input$faas_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else {
      json <- list()
    }
    json_pretty <- ob_faas_delete(input, output, session, json)
    json_data(json_pretty)
  })
  
  # if user pushes data_delete button, it will remove that data server.
  observeEvent(input$data_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else {
      json <- list()
    }
    json_pretty <- ob_data_delete(input, output, session, json)
    json_data(json_pretty)
  })
  
  # if user pushes gen_delete button, it will remove that general configurations.
  observeEvent(input$gen_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    json_pretty <- ob_gen_delete(input, output, session, json)
    json_data(json_pretty)
  })
  
  ##################################
  # Apply button configurations
  # source: "action_button_delete.R"
  ##################################
  # if user pushes func_apply button, it will save the selected and text inputs into the json.
  observeEvent(input$func_apply, {
    if (!is.null(test_func())){
      return()
    }
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    json_pretty <- ob_func_apply(input, output, session, json)
    json_data(json_pretty)
  })
  
  # if user pushes faas_apply button, it will save the selected and text inputs into the json.
  observeEvent(input$faas_apply, {
    if (!is.null(test_faas())){
      return()
    }
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    faas_name <- input$faas_name
    if (is.null(faas_name)){
      return()
    }
    json_pretty <- ob_faas_apply(input, output, session, json)
    json_data(json_pretty)
  })
  
  # if user pushes data_apply button, it will save the selected and text inputs into the json.
  observeEvent(input$data_apply, {
    if (!is.null(test_data())){
      return()
    }
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    json_pretty <- ob_data_apply(input, output, session, json)
    json_data(json_pretty)
  })
  
  # if user pushes gen_apply button, it will save the selected and text inputs into the json.
  observeEvent(input$gen_apply, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    json_pretty <- ob_gen_apply(input, output, session, json)
    json_data(json_pretty)
    
  })
  
  ##################################
  # Make visnetwork data.frames
  # source: "visnetwork.dataframe.R"
  ##################################
  # make data frames for drawing "Functions" figures
  graph_func_name <- reactiveVal(NULL)
  graph_func_edge <- reactiveVal(NULL)
  observe({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    vis_func_list <- vis_df_func(input, output, session, json)
    
    graph_func_name(vis_func_list$name)
    graph_func_edge(vis_func_list$edge)
  })
  
  # make data frames for drawing "Data server" figures
  graph_data_name <- reactiveVal(NULL)
  graph_data_edge <- reactiveVal(NULL)
  observe({ 
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    vis_data_list <- vis_df_data(input, output, session, json)
    
    graph_data_edge(vis_data_list$edge)
    graph_data_name(vis_data_list$name)
  })
  
  # make data frames for drawing "FaaS" figures
  graph_faas_name <- reactiveVal(NULL)
  graph_faas_edge <- reactiveVal(NULL)
  observe({ 
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    vis_faas_list <- vis_df_faas(input, output, session, json)
    
    graph_faas_name(vis_faas_list$name)
    graph_faas_edge(vis_faas_list$edge)
  })
  
  ################################
  # Draw visnetwork figures
  # source: "visnetwork.drawing.R"
  ################################
  # fsm_func is a Finite State Machine for functions
  output$fsm_func <- renderVisNetwork({
    if (!is.null(graph_func_edge)){
      fsm_func_edge <- graph_func_edge()
    } else{
      fsm_func_edge <- data.frame()
    }
    
    if (!is.null(graph_func_name)){
      fsm_func_name <- graph_func_name()
    } else{
      fsm_func_name <- data.frame()
    }
    
    vis_fig_func(fsm_func_name, fsm_func_edge)
    
  })
  
  # fsm_data is a drawing for data servers.
  output$fsm_data <- renderVisNetwork({
    
    if (!is.null(graph_data_name)){
      fsm_data_name <- graph_data_name()
    } else{
      fsm_data_name <- data.frame()
    }
    
    if (!is.null(graph_data_edge)){
      fsm_data_edge <- graph_data_edge()
    } else{
      fsm_data_edge <- data.frame()
    }
    
    vis_fig_data(fsm_data_name, fsm_data_edge)
    
  })
  
  # draw a graph for FaaS servers
  output$fsm_faas <- renderVisNetwork({
    
    if (!is.null(graph_faas_name)){
      fsm_faas_name <- graph_faas_name()
    } else{
      fsm_faas_name <- data.frame()
    }
    
    if (!is.null(graph_faas_edge)){
      fsm_faas_edge <- graph_faas_edge()
    } else{
      fsm_faas_edge <- data.frame
    }
    
    vis_fig_faas(fsm_faas_name, fsm_faas_edge) 
  })
  
  ############################################
  # Reactive visnetwork figures configurations
  # source: "visnetwork.drawing.R"
  #         "visnetwork.dataframe.R"
  ############################################
  # This is to implement user friendly functions.
  # If user clicks the drawings, it will show the data on the left.
  observe({
    # require function node click
    req(input$func_click)
    
    # get viz df
    if (!is.null(graph_func_edge)){
      fsm_func_edge <- graph_func_edge()
    } else{
      fsm_func_edge <- data.frame()
    }
    
    if (!is.null(graph_func_name)){
      fsm_func_name <- graph_func_name()
    } else{
      fsm_func_name <- data.frame()
    }
    
    # if no node is selected, call original figure
    if (length(input$func_click$nodes)==0){
      output$fsm_func <- renderVisNetwork({
        vis_fig_func(fsm_func_name, fsm_func_edge)
      })
      return("")
    } 
    
    # if a node is selected, update the values
    updateSelectInput(inputId="select1", selected="Functions")
    new_nodes <- input$func_click$nodes[[1]]
    updateTextInput(inputId="func_name", value=new_nodes)
    
    # edit groups
    fsm_func_name <- vis_df_select_func(fsm_func_name, new_nodes)
    
    # call "selected" function figure
    output$fsm_func <- renderVisNetwork({
      vis_fig_func_select(fsm_func_name, fsm_func_edge)
    })
  })
  
  observe({
    # require faas node click
    req(input$faas_click)
    # if no node is selected, do nothing
    if (length(input$faas_click$nodes)==0){
      return("")
    } 
    
    # if a node is selected, update "selected" figure
    updateSelectInput(inputId="select1", selected="FaaS Server")
    new_faas <- input$faas_click$nodes[[1]]
    updateTextInput(inputId="faas_name", value=new_faas)
    
    if (!is.null(graph_func_edge)){
      fsm_func_edge <- graph_func_edge()
    } else{
      fsm_func_edge <- data.frame()
    }
    
    if (!is.null(graph_func_name)){
      fsm_func_name <- graph_func_name()
    } else{
      fsm_func_name <- data.frame()
    }
    
    new_nodes <- fsm_func_name[fsm_func_name$server == new_faas, "id"]
    
    # edit groups
    fsm_func_name <- vis_df_select_func(fsm_func_name, new_nodes)
    
    output$fsm_func <- renderVisNetwork({
      vis_fig_func_select(fsm_func_name, fsm_func_edge)
    })
  })
  
  observe({
    # require data node click
    req(input$data_click)
    # if no node is selected, do nothing
    if (length(input$data_click$nodes)==0){
      return("")
    } 
    
    # if a node is selected, update the values
    updateSelectInput(inputId="select1", selected="Data Server")
    new_data <- input$data_click$nodes[[1]]
    updateTextInput(inputId="data_name", value=new_data)
  })
  
  ################################
  # Sweet warning configurations
  # source: 
  ################################
  # This is a helper function.
  # Because of "get", this should reside in the "server.R"
  alert_js_module <- function(type, msg, name, on=TRUE){
    func <- get(type)
    func(msg)
    if (on) {
      runjs(paste0("$('#",name,"').on('click.x', function(e){e.preventDefault();});"))
    } else {
      runjs(paste0("$('#",name,"').off('click.x');"))
    }
  } 
  
  alert_module <- function(title, text){
    sendSweetAlert(
      title = HTML('<div style="color: gray; font-size: 23px; text-align: center;">', title,
                   'Configuration incomplete!</div><br><div style="color: #FF5733 ;', 
                   'font-size: 15px; text-align: center;">Required:&nbsp</div>'),
      text = HTML('<span style="color: gray; font-size: 18px;">',text,'</span'),
      type = "error",
      html = TRUE
    )
  }
  
  # Check general configuration requirements
  test_gen <- reactiveVal("General Configuration incomplete")
  observe({
    gen_text <- NULL
    if (is.null(input$function_invoke) || input$function_invoke == ""){
      gen_text <- paste(gen_text, "First Function<br>")
    }
    if (is.null(input$default_data_server) || input$default_data_server == ""){
      gen_text <- paste(gen_text, "Default Data Server<br>")
    }
    
    if (is.null(gen_text)) {
      alert_js_module("test_gen", gen_text, "genapp", on=FALSE)
    } else {
      alert_js_module("test_gen", gen_text, "genapp")
    }
  })
  
  # Send general configuration warning message
  observeEvent(input[["gen_app"]], {
    if(!is.null(test_gen())){
      alert_module("General", as.character(test_gen()))
    }
  })
  
  # Check Data server requirements
  test_data <- reactiveVal("Data Server Configuration incomplete")
  observe({
    data_text <- NULL
    if (is.null(input$data_name) || input$data_name == ""){
      data_text <- paste(data_text, "Data Server Name<br>")
    } 
    if (is.null(input$data_bucket) || input$data_bucket == ""){
      data_text <- paste(data_text, "Data Server Bucket<br>")
    } 
    if (is.null(input$data_writable) || input$data_writable == ""){
      data_text <- paste(data_text, "Data Server Permission<br>")
    } 
    
    if (is.null(data_text)) {
      alert_js_module("test_data", data_text, "dataapp", on=FALSE)
    } else {
      alert_js_module("test_data", data_text, "dataapp")
    }
  })
  
  # Send data stores configuration warning message
  observeEvent(input[["data_app"]], {
    if(!is.null(test_data())){
      alert_module("Data Server", as.character(test_data()))
    }
  })
  
  # Check Functions requirements
  test_func <- reactiveVal("Function Configuration incomplete")
  observe({
    func_text <- NULL
    if (is.null(input$func_name) || input$func_name == ""){
      func_text <- paste(func_text, "Action Name<br>")
    } 
    if (is.null(input$func_act) || input$func_act == ""){
      func_text <- paste(func_text, "Function Name<br>")
    } 
    if (is.null(input$func_faas) || input$func_faas == ""){
      func_text <- paste(func_text, "Function's FaaS Server<br>")
    }
    
    if (is.null(func_text)) {
      alert_js_module("test_func", func_text, "funcapp", on=FALSE)
    } else {
      alert_js_module("test_func", func_text, "funcapp")
    }
  })
  
  # Send functions configuration warning message
  observeEvent(input[["func_app"]], {
    if(!is.null(test_func())){
      alert_module("Functions", as.character(test_func()))
    }
  })
  
  # Check FaaS server requirements
  test_faas <- reactiveVal("FaaS Server configuration incomplete")
  observe({
    faas_text <- NULL
    type <- input$faas_type
    
    if (is.null(input$faas_name) || input$faas_name == ""){
      faas_text <- paste(faas_text, "FaaS Name<br>")
    } 
    if (is.null(input$faas_type) || input$faas_type == ""){
      faas_text <- paste(faas_text, "FaaS Type<br>")    
    } else{
      if (type=="GitHubActions"){
        if (is.null(input$faas_gh_user) || input$faas_gh_user == ""){
          faas_text <- paste(faas_text, "Github User name<br>")
        } 
        if (is.null(input$faas_gh_repo) || input$faas_gh_repo == ""){
          faas_text <- paste(faas_text, "Github Repo name<br>")
        } 
      } else if (type == "OpenWhisk"){
        if (is.null(input$faas_ow_end) || input$faas_ow_end == ""){
          faas_text <- paste(faas_text, "OpenWhisk Endpoint<br>")
        } 
        if (is.null(input$faas_ow_name) || input$faas_ow_name == ""){
          faas_text <- paste(faas_text, "Openwhisk Namespace<br>")
        }
      } else if (type == "Lambda"){
        if (is.null(input$faas_ld_region) || input$faas_ld_region == ""){
          faas_text <- paste(faas_text, "Lambda Region<br>")
        } 
      }
    }
    
    if (is.null(faas_text)) {
      alert_js_module("test_faas", faas_text, "faasapp", on=FALSE)
    } else {
      alert_js_module("test_faas", faas_text, "faasapp")
    }
    
  })
  
  # Send faas servers configuration warning message
  observeEvent(input[["faas_app"]], {
    if(!is.null(test_faas())){
      alert_module("FaaS Server", as.character(test_faas()))
    }
  })
  
  # Check JSON validations
  test <- reactiveVal("JSON format incomplete")  
  observeEvent(json_data(), {
    json <- jsonlite::fromJSON(json_data())
    json_text <- NULL
    
    if (is.null(json$FunctionInvoke) || json$FunctionInvoke == ""){
      json_text <- paste(json_text, "First Function<br>")
    } 
    if (is.null(json$DefaultDataStore) || json$DefaultDataStore == ""){
      json_text <- paste(json_text, "Default Data Server<br>")
    } 
    if (length(names(json$ComputeServer))==0){
      json_text <- paste(json_text, "At least one FaaS Server<br>")
    } 
    if (length(names(json$DataStore))==0){
      json_text <- paste(json_text, "At least one Data Server<br>")
    } 
    if (length(names(json$FunctionList))==0){
      json_text <- paste(json_text, "At least one Function<br>")
    }
    
    if (is.null(json_text)) {
      alert_js_module("test", json_text, "dwnbutton", on=FALSE)
    } else {
      alert_js_module("test", json_text, "dwnbutton")
    }
    
  })
  
  # Send download warning message
  observeEvent(input[["dwnClicked"]], {
    if(!is.null(test())){
      alert_module("JSON", as.character(test()))
    }
  })
}


