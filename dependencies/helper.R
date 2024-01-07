
# this function is to convert the list into "a=b, c=d" form
unretrieve <- function(val){
  if (!is.null(val)){
    if (!is.list(val)){
      val_merged_char <- NULL
      for (val_key in val){
        if (val_key == val[length(val)]){
          val_merged_char <- paste0(val_merged_char, val_key)
        } else {
          val_merged_char <- paste0(val_merged_char, val_key,", ")
        }
      }
      return(val_merged_char)
    } else {
      val_merged <- NULL
      for (val_key in names(val)){
        if (val_key == names(val[length(val)])){
          val_merged <- paste0(val_merged, val_key, "=",val[[val_key]])
        } else{
          val_merged <- paste0(val_merged, val_key, "=",val[[val_key]], ",\n")  
        }
      }
      return(val_merged)
    }
  } else {
    return(val)
  }
}

# this function is to convert the "a=b, c=d" form into the list
retrieve <- function(val) {
  if (!is.null(val) && length(val)!=0 && val !="") {
    val_list <- unlist(strsplit(strsplit(val, '\n')[[1]], ','))
    text_list <- list()
    for (text in val_list){
      parts <- strsplit(text, '=')
      if (length(parts[[1]])<2){
        text_trim <- trimws(text)
        text_list <- unlist(c(text_list, text_trim))
      } else {
        re_input_key <- trimws(gsub('[\'"]', '', parts[[1]][1]))
        re_input_val <- trimws(gsub('[\'"]', '', parts[[1]][2]))
        text_list[[re_input_key]] <- re_input_val
      }
    }
    return(text_list)
  } else {
    return(NULL)
  }
}
