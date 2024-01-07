vis_df_func <- function(input, output, session, json){
  
  graph <- list()
  
  # build the graph indicating adjacent nodes, e.g., "F1":["F2","F3"], so on.
  for (func in names(json$FunctionList)) {
    graph[[func]] <- json$FunctionList[[func]]$InvokeNext
  }
  
  pre <- list()
  for (func1 in names(graph)){
    for (func2 in graph[[func1]]){
      pre[[func2]] <- c(pre[[func2]], func1)
    }
  }
  
  dfs <- function(start, target, i){
    i=i
    # find target in the graph's successor. If it matches, there's a loop
    if (target %in% pre[[start]]) {
      len_val <<- c(len_val, i)
      sendSweetAlert(
        title = HTML('<div style="color: gray; font-size: 23px; text-align: center;">',
                     'Loop Found!</div><br><div style="color: #FF5733;font-size: 15px;">Check the node:</div>'),
        text = HTML('<span style="color: gray; font-size: 18px;">',target,'</span'),
        type = "error",
        html = TRUE
      )
    } else {
      i <- i+1
      if (length(pre[[start]])==0){
        len_val <<- c(len_val, i)
      } else {
        # add start, marking as "visited"
        stack <<- unique(c(stack, start))
      }
      
      # set one of the successors as another "start"
      for (func in pre[[start]]) {
        
        # if new "start" has been visited, do nothing
        if (func %in% stack) {
          NULL
          # if not, keep checking the DAG.
        } else {
          
          dfs(func, target, i)
        }
      }
    }
    
  }
  
  fsm_func_edge <- data.frame()
  fsm_func_name <- data.frame()
  nb <- length(names(json$FunctionList))
  
  if (nb==0){
    return(list(name=fsm_func_name, edge=fsm_func_edge))
  }
  
  for (funcname in names(json$FunctionList)) {
    stack <- list()
    len_val <- list()
    dfs(funcname, funcname, 0)
    level <- max(unlist(len_val))
    type <- json$ComputeServers[[json$FunctionList[[funcname]]$FaaSServer]]$FaaSType
    
    
    
    if (!is.null(json$FunctionInvoke) && funcname == json$FunctionInvoke){
      if (is.null(type)){
        new_node <- data.frame(id=funcname, label=paste0(funcname, "\n", json$FunctionList[[funcname]]$FunctionName),
                               group="undef", level=level, server="undef")
        fsm_func_name <- rbind(fsm_func_name, new_node)
      } else if (type =="GitHubActions"){
        new_node <- data.frame(id=funcname, label=paste0(funcname, "\n", json$FunctionList[[funcname]]$FunctionName),
                               group="gh_first", level=level, server=json$FunctionList[[funcname]]$FaaSServer)
        fsm_func_name <- rbind(fsm_func_name, new_node)
      } else if (type == "OpenWhisk"){
        new_node <- data.frame(id=funcname, label=paste0(funcname, "\n", json$FunctionList[[funcname]]$FunctionName),
                               group="ow_first", level=level, server=json$FunctionList[[funcname]]$FaaSServer)
        fsm_func_name <- rbind(fsm_func_name, new_node)
      } else if (type == "Lambda"){
        new_node <- data.frame(id=funcname, label=paste0(funcname, "\n", json$FunctionList[[funcname]]$FunctionName),
                               group="ld_first", level=level, server=json$FunctionList[[funcname]]$FaaSServer)
        fsm_func_name <- rbind(fsm_func_name, new_node)
      }
    } else{
      if (is.null(type)){
        new_node <- data.frame(id=funcname, label=paste0(funcname, "\n", json$FunctionList[[funcname]]$FunctionName),
                               group="undef", level=level, server="undef")
        fsm_func_name <- rbind(fsm_func_name, new_node)
      } else if (type =="GitHubActions"){
        new_node <- data.frame(id=funcname, label=paste0(funcname, "\n", json$FunctionList[[funcname]]$FunctionName),
                               group="gh", level=level, server=json$FunctionList[[funcname]]$FaaSServer)
        fsm_func_name <- rbind(fsm_func_name, new_node)
      } else if (type == "OpenWhisk"){
        new_node <- data.frame(id=funcname, label=paste0(funcname, "\n", json$FunctionList[[funcname]]$FunctionName),
                               group="ow", level=level, server=json$FunctionList[[funcname]]$FaaSServer)
        fsm_func_name <- rbind(fsm_func_name, new_node)
      } else if (type == "Lambda"){
        new_node <- data.frame(id=funcname, label=paste0(funcname, "\n", json$FunctionList[[funcname]]$FunctionName),
                               group="ld", level=level, server=json$FunctionList[[funcname]]$FaaSServer)
        fsm_func_name <- rbind(fsm_func_name, new_node)
      }
    }
    for (next_func in json$FunctionList[[funcname]]$InvokeNext){
      if (next_func %in% names(json$FunctionList)){
        new_edge <- data.frame(from=funcname, to=next_func, width=1)
        fsm_func_edge <- rbind(fsm_func_edge, new_edge)
      }
    }
  }
  
  return(list(name=fsm_func_name, edge=fsm_func_edge))
}


vis_df_data <- function(input, output, session, json){
  fsm_data_name <- data.frame()
  fsm_data_edge <- data.frame()
  nb <- length(names(json$DataStores))
  
  if (nb==0){
    return(list(name=fsm_data_name, edge=fsm_data_edge))
  }
  
  for (dataname in names(json$DataStores)) {
    if (!is.null(json$DefaultDataStore) && dataname == json$DefaultDataStore){
      new_node <- data.frame(id=dataname, label=dataname, group="data_first")
      fsm_data_name <- rbind(fsm_data_name, new_node)
    } else {
      new_node <- data.frame(id=dataname, label=dataname, group="data")
      fsm_data_name <- rbind(fsm_data_name, new_node)
    }
  }
  
  if (length(json$DataStores)!=0 || length(json$DataStores)!=1){
    for (ind in 1:length(json$DataStores)){
      if (ind == length(json$DataStores)){
        new_edge <- data.frame(from=names(json$DataStores)[[ind]], to=names(json$DataStores)[[1]])
        fsm_data_edge <- rbind(fsm_data_edge, new_edge)
      } else {
        new_edge <- data.frame(from=names(json$DataStores)[[ind]], to=names(json$DataStores)[[ind+1]])
        fsm_data_edge <- rbind(fsm_data_edge, new_edge)
      }
    }
  }
  
  return(list(name=fsm_data_name, edge=fsm_data_edge))
}


vis_df_faas <- function(input, output, session, json){
  
  fsm_faas_name <- data.frame()
  fsm_faas_edge <- data.frame()
  nb <- length(names(json$ComputeServers))
  
  if (nb==0){
    return(list(name=fsm_faas_name, edge=fsm_faas_edge))
  }
  
  for (faasname in names(json$ComputeServers)) {
    type <- json$ComputeServers[[faasname]]$FaaSType
    if (type =="GitHubActions"){
      new_node <- data.frame(id=faasname, label=faasname, group="gh")
      fsm_faas_name <- rbind(fsm_faas_name, new_node)
    } else if (type == "OpenWhisk"){
      new_node <- data.frame(id=faasname, label=faasname, group="ow")
      fsm_faas_name <- rbind(fsm_faas_name, new_node)
    } else if (type == "Lambda"){
      new_node <- data.frame(id=faasname, label=faasname, group="ld")
      fsm_faas_name <- rbind(fsm_faas_name, new_node)
    }
  }
  
  if (length(json$ComputeServers)!=0 || length(json$ComputeServers)!=1){
    for (ind in 1:length(json$ComputeServers)){
      if (ind == length(json$ComputeServers)){
        new_edge <- data.frame(from=names(json$ComputeServers)[[ind]], to=names(json$ComputeServers)[[1]])
        fsm_faas_edge <- rbind(fsm_faas_edge, new_edge)
      } else {
        new_edge <- data.frame(from=names(json$ComputeServers)[[ind]], to=names(json$ComputeServers)[[ind+1]])
        fsm_faas_edge <- rbind(fsm_faas_edge, new_edge)
      }
    }
  }
  
  return(list(name=fsm_faas_name, edge=fsm_faas_edge))
}


vis_df_select_func <- function(fsm_func_name, new_nodes){
  
  for (ids in fsm_func_name$id){
    if (ids %in% new_nodes){
      group_name <- fsm_func_name[fsm_func_name$id == ids, "group"]
      if (group_name =="ow" || group_name == "ow_first"){
        fsm_func_name[fsm_func_name$id == ids, "group"] <- "ow_selected"
      } else if (group_name =="gh" || group_name == "gh_first"){
        fsm_func_name[fsm_func_name$id == ids, "group"] <- "gh_selected"
      } else if (group_name =="ld" || group_name == "ld_first"){
        fsm_func_name[fsm_func_name$id == ids, "group"] <- "ld_selected"
      } else if (group_name =="undef"){
        fsm_func_name[fsm_func_name$id == ids, "group"] <- "undef_selected"
      }
    } else {
      group_name <- fsm_func_name[fsm_func_name$id == ids, "group"]
      if (group_name =="ow" || group_name == "ow_first"){
        fsm_func_name[fsm_func_name$id == ids, "group"] <- "ow_not"
      } else if (group_name =="gh" || group_name == "gh_first"){
        fsm_func_name[fsm_func_name$id == ids, "group"] <- "gh_not"
      } else if (group_name =="ld" || group_name == "ld_first"){
        fsm_func_name[fsm_func_name$id == ids, "group"] <- "ld_not"
      } else if (group_name =="undef"){
        fsm_func_name[fsm_func_name$id == ids, "group"] <- "undef_not"
      }
    }
  }
  
  return(fsm_func_name)
}
