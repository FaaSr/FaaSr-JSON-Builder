#path_to_images <- "https://raw.githubusercontent.com/spark0510/FaaSr-JSON-Builder/branch4/www/"
path_to_images <- ""
ow_logo_black <- "ow_dark.png"
ow_logo_white <- "ow.png"

vis_fig_func <- function(fsm_func_name, fsm_func_edge){

  visNetwork(fsm_func_name, fsm_func_edge, width="150%", height="150%",
             main = list(text = "<b>Workflow<br></b>",
                         style = "color:black;font-size:15px;text-align:center;")) %>%
    visGroups(groupname = "gh", shape = "icon", 
              icon = list(code = "f092", size = 65, color="skyblue")) %>%
    visGroups(groupname = "ow", shape = "image", 
              image=paste0(path_to_images, ow_logo_white), size=30, 
              color=list(border="skyblue", background="skyblue", highlight=list(background="skyblue", border="skyblue")),
              shapeProperties = list(useBorderWithImage = TRUE)) %>%
    visGroups(groupname = "ld", shape = "icon", 
              icon = list(code = "f375", size = 50, color="skyblue")) %>%
    visGroups(groupname = "undef", shape = "icon", 
              icon = list(code = "f127", size = 50, color="skyblue")) %>%
    visGroups(groupname = "gh_first", shape = "icon", 
              icon = list(code = "f092", size = 65, color="#24a0ed")) %>%
    visGroups(groupname = "ow_first", shape = "image", 
              image=paste0(path_to_images, ow_logo_white), size=30, 
              color=list(border="#24a0ed", background="#24a0ed", highlight=list(background="#24a0ed", border="#24a0ed")),
              shapeProperties = list(useBorderWithImage = TRUE)) %>%
    visGroups(groupname = "ld_first", shape = "icon", 
              icon = list(code = "f375", size = 50, color="#24a0ed")) %>%
    visEdges(arrows="to", color=list(color="skyblue", highlight="skyblue"), arrowStrikethrough = FALSE) %>%
    addFontAwesome() %>% 
    visHierarchicalLayout(direction = "LR") %>%
    visEvents(click = "function(fsm_func_name){
                  Shiny.onInputChange('func_click', fsm_func_name);
                  ;}"
    ) 
}

vis_fig_func_select <- function(fsm_func_name, fsm_func_edge){
  visNetwork(fsm_func_name, fsm_func_edge, width="150%", height="150%",
             main = list(text = "<b>Workflow</b>",
                         style = "color:black;font-size:15px;text-align:center;")) %>%
    visEdges(arrows="to", color=list(color="#cfcfcf", highlight="skyblue"), arrowStrikethrough = FALSE) %>%
    visGroups(groupname = "gh_selected", shape = "icon", 
              icon = list(code = "f092", size = 65, color="skyblue")) %>%
    visGroups(groupname = "ow_selected", shape = "image", 
              image=paste0(path_to_images, ow_logo_white), size=30, 
              color=list(border="skyblue", background="skyblue", highlight=list(background="skyblue", border="skyblue")),
              shapeProperties = list(useBorderWithImage = TRUE)) %>%
    visGroups(groupname = "undef_selected", shape = "icon", 
              icon = list(code = "f127", size = 50, color="skyblue")) %>%
    visGroups(groupname = "ld_selected", shape = "icon", 
              icon = list(code = "f375", size = 50, color="skyblue")) %>%
    visGroups(groupname = "gh_not", shape = "icon", 
              icon = list(code = "f092", size = 65, color="#cfcfcf")) %>%
    visGroups(groupname = "ow_not", shape = "image", 
              image=paste0(path_to_images, ow_logo_black), size=30, 
              color=list(border="#cfcfcf", background="#cfcfcf", highlight=list(background="#cfcfcf", border="#cfcfcf")),
              shapeProperties = list(useBorderWithImage = TRUE)) %>%
    visGroups(groupname = "ld_not", shape = "icon", 
              icon = list(code = "f375", size = 50, color="#cfcfcf")) %>%
    visGroups(groupname = "undef_not", shape = "icon", 
              icon = list(code = "f127", size = 50, color="#cfcfcf")) %>%
    addFontAwesome() %>% 
    visOptions(highlightNearest = TRUE) %>%
    visHierarchicalLayout(direction = "LR") %>%
    visEvents(click = "function(fsm_func_name){
                  Shiny.onInputChange('func_click', fsm_func_name);
                  ;}"
    )
}


vis_fig_faas <- function(fsm_faas_name, fsm_faas_edge){
  # fsm_faas is a drawing for faas servers.
  visNetwork(fsm_faas_name, fsm_faas_edge, width="100%",
               main = list(text = "<b>FaaS Servers</b>",
                           style = "color:black;font-size:15px;text-align:center;")) %>%
      visGroups(groupname = "gh", shape = "icon", 
                icon = list(code = "f092", size = 65, color="black")) %>%
      visGroups(groupname = "ow", shape = "image", 
                image=paste0(path_to_images, ow_logo_white), size=30, 
                color=list(border="#F5F5F5", background="#F5F5F5", highlight=list(background="#F5F5F5", border="#F5F5F5")),
                shapeProperties = list(useBorderWithImage = TRUE)) %>%
      visGroups(groupname = "ld", shape = "icon", 
                icon = list(code = "f375", size = 50, color="black")) %>%
      visEdges(color=list(color="#F5F5F5", highlight="#F5F5F5")) %>%
      addFontAwesome() %>%
      visEvents(click = "function(fsm_faas_name){
                  Shiny.onInputChange('faas_click', fsm_faas_name);
                  ;}"
      )
      
}

vis_fig_data <- function(fsm_data_name, fsm_data_edge){
  
  visNetwork(fsm_data_name, fsm_data_edge, width="100%",
             main = list(text = "<b>Data Servers</b>",
                         style = "color:black;font-size:15px;text-align:center;")) %>%
    visGroups(groupname = "data_first", shape = "icon", 
              icon = list(code = "f1c0", size = 65, color="#88cb9e")) %>%
    visGroups(groupname = "data", shape = "icon", 
              icon = list(code = "f1c0", size = 65, color="#b3c78c")) %>%
    visEdges(color=list(color="#F5F5F5", highlight="#F5F5F5")) %>%
    addFontAwesome() %>%
    visEvents(click = "function(fsm_data_name){
                  Shiny.onInputChange('data_click', fsm_data_name);
                  ;}"
    ) 
  
}




