
ob_func_apply <- function(input, output, session, json){
  func_name <- retrieve(input$func_name)
  if (is.null(func_name)){
    return()
  }
  json$FunctionList[[func_name]]$FunctionName <- retrieve(input$func_act)
  json$FunctionList[[func_name]]$FaaSServer <- retrieve(input$func_faas)
  json$FunctionList[[func_name]]$Arguments <- retrieve(input$func_args)
  json$FunctionList[[func_name]]$InvokeNext <- retrieve(input$func_next)
  json$ActionContainers[[func_name]] <- retrieve(input$func_container)
  json$FunctionCRANPackage[[input$func_act]] <- retrieve(input$func_cran_repo)
  json$FunctionGitHubPackage[[input$func_act]] <-retrieve(input$func_gh_package)
  json$FunctionGitRepo[[input$func_act]] <- retrieve(input$func_gh_repo)
  
  json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
  json_pretty <- jsonlite::prettify(json_source)
  
  return(json_pretty)
}

ob_faas_apply <- function(input, output, session, json){
  faas_name <- retrieve(input$faas_name)
  if (is.null(faas_name)){
    return()
  }
  json$ComputeServers[[faas_name]]$FaaSType <- retrieve(input$faas_type)
  switch(json$ComputeServers[[faas_name]]$FaaSType,
         "GitHubActions"={
           json$ComputeServers[[faas_name]]$UserName <- retrieve(input$faas_gh_user)
           json$ComputeServers[[faas_name]]$ActionRepoName <- retrieve(input$faas_gh_repo)
           json$ComputeServers[[faas_name]]$Branch <- retrieve(input$faas_gh_ref)
         },
         "Lambda"={
           json$ComputeServers[[faas_name]]$Region <- retrieve(input$faas_ld_region)
         },
         "OpenWhisk"={
           json$ComputeServers[[faas_name]]$Endpoint <- retrieve(input$faas_ow_end)
           json$ComputeServers[[faas_name]]$Namespace <- retrieve(input$faas_ow_name)
           json$ComputeServers[[faas_name]]$Region <- retrieve(input$faas_ow_region)
         }
  )
  json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
  json_pretty <- jsonlite::prettify(json_source)
  
  return(json_pretty)
}


ob_data_apply <- function(input, output, session, json){
  data_name <- input$data_name
  if (is.null(data_name)){
    return()
  }
  json$DataStores[[data_name]]$Endpoint <- retrieve(input$data_endpoint)
  json$DataStores[[data_name]]$Bucket <- retrieve(input$data_bucket)
  json$DataStores[[data_name]]$Region <- retrieve(input$data_region)
  json$DataStores[[data_name]]$Writable <- retrieve(input$data_writable)
  json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
  json_pretty <- jsonlite::prettify(json_source)
  
  return(json_pretty)
}


ob_gen_apply <- function(input, output, session, json){
  json$FunctionInvoke <- retrieve(input$function_invoke)
  json$InvocationID <- retrieve(input$invocation_id)
  json$FaaSrLog <- retrieve(input$faasr_log)
  if (input$logging_data_server == "None"){
    json$LoggingDataStore <- NULL
  } else {
    json$LoggingDataStore <- retrieve(input$logging_data_server)
  }
  json$DefaultDataStore <- retrieve(input$default_data_server)
  json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
  json_pretty <- jsonlite::prettify(json_source)
  
  return(json_pretty)
}

