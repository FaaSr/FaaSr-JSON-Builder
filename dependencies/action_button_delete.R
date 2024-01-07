

ob_func_delete <- function(input, output, session, json){
  func_name <- input$func_name
  if (is.null(func_name) || func_name == ""){
    return(jsonlite::toJSON(json, auto_unbox=TRUE, pretty=TRUE))
  }
  if (!func_name %in% names(json$FunctionList)){
    return(jsonlite::toJSON(json, auto_unbox=TRUE, pretty=TRUE))
  }
  if (func_name == json$FunctionInvoke){
    json$FunctionInvoke <- NULL
  }
  func_act <- json$FunctionList[[func_name]]$FunctionName
  json$FunctionList[[func_name]] <- NULL
  json$ActionContainers[[func_name]] <- NULL
  func_set <- NULL
  for (func in names(json$FunctionList)){
    if (func != func_name){
      func_set <- unique(c(func_set, json$FunctionList[[func]]$FunctionName))
      if (func_name %in% json$FunctionList[[func]]$InvokeNext){
        json$FunctionList[[func]]$InvokeNext <- json$FunctionList[[func]]$InvokeNext[json$FunctionList[[func]]$InvokeNext != func_name]
      }
    }
  }
  if (!func_act %in% func_set){
    json$FunctionCRANPackage[[func_act]] <- NULL
    json$FunctionGitHubPackage[[func_act]] <-NULL
    json$FunctionGitRepo[[func_act]] <- NULL
  }
  json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
  json_pretty <- jsonlite::prettify(json_source)
  
  return(json_pretty)
}

ob_faas_delete <- function(input, output, session, json){
  faas_name <- input$faas_name
  if (is.null(faas_name) || faas_name == ""){
    return(jsonlite::toJSON(json, auto_unbox=TRUE, pretty=TRUE))
  }
  if (!faas_name %in% names(json$ComputeServers)){
    return(jsonlite::toJSON(json, auto_unbox=TRUE, pretty=TRUE))
  }
  json$ComputeServers[[faas_name]] <- NULL
  json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
  json_pretty <- jsonlite::prettify(json_source)
  
  return(json_pretty)
}

ob_data_delete <- function(input, output, session, json){
  data_name <- input$data_name
  if (is.null(data_name) || data_name == ""){
    return(jsonlite::toJSON(json, auto_unbox=TRUE, pretty=TRUE))
  }
  if (!data_name %in% names(json$DataStores)){
    return(jsonlite::toJSON(json, auto_unbox=TRUE, pretty=TRUE))
  }
  data_name <- input$data_name
  if (data_name == json$DefaultDataStore){
    json$DefaultDataStore <- NULL
  }
  if (data_name == json$LoggingDataStore){
    json$LoggingDataStore <- NULL
  }
  json$DataStores[[data_name]] <- NULL
  json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
  json_pretty <- jsonlite::prettify(json_source)
  
  return(json_pretty)
}

ob_gen_delete <- function(input, output, session, json){
  json$FunctionInvoke <- NULL
  json$InvocationID <- NULL
  json$FaaSrLog <- NULL
  json$LoggingDataStore <- NULL
  json$DefaultDataStore <- NULL
  json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
  json_pretty <- jsonlite::prettify(json_source)
  
  return(json_pretty)
}

