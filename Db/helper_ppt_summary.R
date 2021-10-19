#########################################
###       processor ppt file          ###
#########################################

getTitles <- function(f){
  ## Slide 1: just plain text name, timestamp, region id
  t <- f %>% filter(slide_id == 1 & id == "3")
  if( nrow(t) == 3 ){
    return( data.frame('acquisition_fullname' = t$text[1],
                       'title_date' = as.Date( str_split_fixed(t$text[2],' ',2)[1] , tryFormats = "%Y-%m-%d"),
                       'title_fullname'= t$text[3]) )
  } else {
    return( data.frame('acquisition_fullname' = t$text[1], 'title_date' = NA, 'title_fullname'= NA) )
  }
}

getMeta <- function(f){
  ## Slide 2
  t <-  f %>% filter(slide_id == 2 & content_type == 'table cell') %>% select(text,cell_id,row_id) %>% spread(cell_id,text) %>% filter(row_id > 1) %>% select(-row_id) %>% t() %>% as.data.frame()
  t <- data.frame(lapply(t, as.character), stringsAsFactors=FALSE)[]
  ExperimentMetadataTable <- t[2,]
  colnames(ExperimentMetadataTable) <- paste0("expmeta_",gsub(' ','_',trimws(gsub(' \\(µM\\)','',gsub("[\r\n]", "", gsub("[#\\/]",'',t[1,]))))))
  colnames(ExperimentMetadataTable) <- tolower(colnames(ExperimentMetadataTable))
  return(ExperimentMetadataTable)
}


getProcessing <- function(f){
  ## Slide 2
  t <-  f %>% filter(slide_id == 3 & content_type == 'table cell') %>% select(text,cell_id,row_id) %>% spread(cell_id,text) %>% filter(row_id > 1) %>% select(-row_id) %>% t() %>% as.data.frame()
  t <- data.frame(lapply(t, as.character), stringsAsFactors=FALSE)[]
  ProcessingParametersTable <- t[2,]
  colnames(ProcessingParametersTable) <-  paste0('procparams_',  gsub(' ','_',trimws(gsub(' \\(µM\\)','',gsub("[\r\n]", "", gsub("[#\\/]",'',t[1,]))))))
  colnames(ProcessingParametersTable) <- tolower(colnames(ProcessingParametersTable))
  return(ProcessingParametersTable)
}


getSummary <- function(f){
  ## Dynamicly collect Slides
  summarySlides <- f %>% filter(grepl("^Summary Table",text))
  #i = summarySlides$slide_id[1]
  allSummary <- data.frame()
  for(i in summarySlides$slide_id){
    t <- f %>% filter(slide_id == i & content_type == 'table cell') %>% select(text,cell_id,row_id) %>% spread(cell_id,text) 
    allSummary <- rbind(allSummary,t)
  }
  title <- allSummary %>% filter(row_id == 1) %>% slice(1)
  allSummary <- allSummary %>% filter(row_id > 1)
  colnames(allSummary) <-  tolower(gsub(' ','_',trimws(gsub(' \\(µM\\)','',gsub("[\r\n]", "", gsub("[%#\\/]",'', title[1,]))))))
  colnames(allSummary) <-  gsub('^95$','p95', gsub('σ','stdev', gsub('µ','mean', names(allSummary))))
  
  return(allSummary[,-c(1)]) # drop first column?
}

