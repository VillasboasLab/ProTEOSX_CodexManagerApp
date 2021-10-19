# Define server logic required to draw a histogram
`%nin%` <- Negate(`%in%`)

shinyServer(function(input, output, session) {
  serverSideValues <- reactiveValues()
  if(is.null(session$user)){ session$user <- "dev"}
  print(paste("Logged In As:",session$user ))
  
  ### This is the control mechanism for conditional panels, specifically for ADMIN views.
  userRoles <- get_UserRoles(session$user)
  print(paste("User Roles:", paste(userRoles,collapse = ", ") ))
  output$adminrole<-reactive({
    if('ADMIN' %in% userRoles$role)
      return(TRUE)
    else
      return(FALSE)
  })
  outputOptions(output, "adminrole", suspendWhenHidden = FALSE)

  data <- readRDS("database.rds") %>% rename(Tile_Size = `Tile_Size_(WxH)`)
  colnames(data) <- gsub('-','_', names(data))
  
  
  
  
  #############################################################
  #####                Home - DB Metrics                  #####
  ############################################################# 
  output$menu1 <- renderText({ paste("Signed In As:",session$user) })
  output$menu2 <- renderText({ paste("Role(s):",paste(userRoles,collapse = ", ")) })
  output$project_count <- renderText({ get_ProjectCount()  })
  output$acque_count <- renderText({  paste(get_AcquisitionCount(),get_ROICount(),sep="/") })
  output$slide_count <- renderText({ get_ImageCount() })
  output$qcflag_count <- renderText({ get_QCFailFlagCount() })
  
  output$missingMetricPieChart <- renderPlot({
    t2 <- getLoadedAquisitionsTable() %>% distinct() %>% mutate(hasMetrics = if_else(is.na(reg),'No','Yes'))
    pie2 <- as.data.frame(table(t2$hasMetrics)) %>%
      mutate(total = sum(Freq)) %>%
      group_by(Var1) %>%
      mutate(perc = (Freq/total))
    pie2 <- data.frame(Var1 = c('No','Yes'), perc = c(0.99,0.01))
    ggplot(pie2, aes(x = '', y = perc, fill = Var1 )) + 
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = scales::percent(perc)),size=5, position = position_stack(vjust = 0.5) )+
      coord_polar("y", start = 0)+
      theme_minimal() + labs(fill='') +
      scale_fill_brewer( type = "div" , palette = "Paired", direction = -1 )+
      theme(axis.title.x = element_blank(), axis.text.x=element_blank(),
            axis.title.y = element_blank(),axis.ticks = element_blank(),
            panel.grid  = element_blank(), legend.position='bottom',
            strip.text.x = element_text(size = 14),
            plot.title = element_text(size = 14, face = "bold")) +
      ggtitle("Total Uploaded Metrics")
  })
  
  output$missingFieldsChart <- renderPlot({
    dTmp <- getAllAquisitionsRois()
    tmp <- as.data.frame(colSums(is.na(dTmp))) %>% 
      rename(cnt = `colSums(is.na(dTmp))`) %>% 
      rownames_to_column(var = "field") %>% filter(!grepl('id',field)) %>% filter(!grepl('\\.',field)) %>%
      arrange(desc(cnt)) %>% mutate( field = gsub('_',' ',str_to_title(field))) %>%
      mutate(total = nrow(dTmp)) %>% mutate(perc = cnt/total)
    
    ggplot(tmp,aes(x=reorder(field,-perc), y=perc, label = scales::percent(perc,accuracy=0.1) ))+geom_bar(stat = "identity", fill="red") + ylim(0,1)+
      geom_text(position = position_dodge(width = .9), hjust = -0.5, size = 4) +
      coord_flip()+
      theme_bw(base_size = 15)+ylab('')+ xlab("Data Fields")+ggtitle("Acquisition & ROI Fields Missing-ness")+
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 25,hjust = 1, size=12) )
  })
  
  
  
  #############################################################
  #####              Entry Page Side Nav                  #####
  #############################################################
  output$ProjectPicker <- renderUI({
    list <- get_OpenProjectNames()$id
    names(list) <- get_OpenProjectNames()$name
    selectInput("new_entry_project_id", label = "Project Name:", choices = list)
  })
  
  ### SIDE BAR BUTTON & MODALS = NEW PROJECT ###
  observeEvent(input$newProjectModal, {
    showModal( newProjectModal(session$ns) )
  })
  observeEvent(input$Save_newProject, {
    rez <- add_NewProject(input$project_name_new,input$project_description,input$project_primary_contact)
    if( rez ){
     showNotification(paste("Project:",input$project_name_new,"Successfully Added!"), duration = 100, type = "message")
    } else {
     showNotification("Error: Project NOT in Database!", duration = 100, type = "error")
    }
    removeModal()
    ## Update Select Dropdown
    list <- get_OpenProjectNames()$id
    names(list) <- get_OpenProjectNames()$name
    print(list)
    updateSelectInput(session, 'new_entry_project_id', choices = list )
  })
  #################################
  
  ### SIDE BAR BUTTON & MODALS = CLOSE PROJECT ###
  observeEvent(input$closeOutProjectModal, {
    showModal(closeProjectModal(session$ns))
  })
  observeEvent(input$new_close_project, {
    updateTextAreaInput(session, 'tmpPX123', value = get_ProjectDescriptionById(input$new_close_project) )
  })
  observeEvent(input$Update_closeProject, {
    close_ProjectById(input$new_close_project, input$tmpPX123)
    removeModal()
    ## Update Select Dropdown
    list <- get_OpenProjectNames()$id
    names(list) <- get_OpenProjectNames()$name
    updateSelectInput(session, 'new_entry_project_id', choices = list )
  })
  #################################

  #### SIDE BAR BUTTON & MODALS = NEW BARCODE
  output$barcodeTbl = renderDataTable( getAllBarcodes(), selection = 'none', rownames = F, editable = F, 
                                       options = list(pageLength = 5, searching = FALSE) )
  observeEvent(input$newBarcodeModal, {
    showModal(newBarcodeModal(session$ns))
  })
  observeEvent(input$save_new_barcode, {
    ## Update Select Dropdown
    list <- getAllBarcodes()$id
    names(list) <- getAllBarcodes()$name
    updateSelectInput(session, 'new_entry_barcode_id', choices = list )
    removeModal()
  })
  #################################

  ### SIDE BAR BUTTON & MODALS = Tissue & Diagnosis Append ###
  observeEvent(input$newTissueModal, {
    showModal(newTissueModal(session$ns))
  })
  observeEvent(input$update_tissue_dropdown, {
    list <- c(input$tmpTissueX456, na.omit(get_DistinctTissues()))
    updateSelectInput(session, 'new_roi_tissue_type', choices = list, selected = input$tmpTissueX456)
    removeModal()
  })
  
  
  observeEvent(input$newDiagnosisModal, {
    showModal(newDiagnosisModal(session$ns))
  })
  observeEvent(input$update_diagnosis_dropdown, {
    list <- c(input$tmpDiagnosisX789, na.omit(get_DistinctDiagnosis()))
    updateSelectInput(session, 'new_roi_diagnosis', choices = list, selected = input$tmpDiagnosisX789)
    removeModal()
  })
  
  
  
  #############################################################
  #####                    Entry Page                     #####
  #############################################################
  
  ####   MULTIPLEX APPENDING TABLE  ####
  output$BarcodePicker <- renderUI({
    list <- get_Barcode()$id
    names(list) <- get_Barcode()$name
    selectInput("new_entry_barcode_id", label = "Barcode:", choices = list)
  })
  
  output$TissuePicker <- renderUI({
    list <- na.omit(get_DistinctTissues())
    selectInput("new_roi_tissue_type", label = "Tissue Type:", choices = list)
  })
  
  output$DiagnosisPicker <- renderUI({
    list <- na.omit(get_DistinctDiagnosis())
    selectInput("new_roi_diagnosis", label = "Diagnosis:", choices = list)
  })
  
  serverSideValues$multiplexDf <- getEmptyMultiplexDataFrame()
  output$recentMultiplexTable = renderDT(serverSideValues$multiplexDf, selection = 'none', rownames = F, editable = F, options = list(paging = FALSE, searching = FALSE))
  observeEvent(input$add_multiplex_row, {
    bcContent <- get_BarCodeValues(input$new_entry_barcode_id)
    tmp <- data.frame(
      `Cycle`	= input$new_muliplex_cycle,
      `Antibody`	= input$new_muliplex_antibody,
      `Vendor`		= input$new_muliplex_vendor,
      `AB Clone`		= input$new_muliplex_ab_clone,
      `AB Concentration`	= input$new_muliplex_ab_conc,
      `Barcode`		= bcContent$name,
      `Channel`		= bcContent$channel,
      `Reporter Fluorphore`	= bcContent$reporter_fluorphore,
      `Exposure Time`		= input$new_muliplex_exposure
    )
    isolate(serverSideValues$multiplexDf <- bind_rows(serverSideValues$multiplexDf, tmp))
    updateNumericInput(session, "new_muliplex_cycle", value = nrow(serverSideValues$multiplexDf)+1 ) 
    updateTextInput(session, "new_muliplex_antibody", value = "")
    updateTextInput(session, "new_muliplex_ab_source", value = "")
    updateTextInput(session, "new_muliplex_ab_clone", value = "")
    updateNumericInput(session, "new_muliplex_ab_conc", value = 0.25 )
    updateTextInput(session, "new_muliplex_exposure", value = "")
  })
  
  ####   ROI APPENDING TABLE  ####
  serverSideValues$roiDf <- getEmptyRoiDataFrame()
  output$recentRoiTable = renderDT(serverSideValues$roiDf, selection = 'none', rownames = F, editable = F, options = list(paging = FALSE, searching = FALSE))
  observeEvent(input$add_roi_row, {
    tmp <- data.frame(
      `Name`	= input$new_roi_name,
      `TMA`	= input$new_roi_tma_source,
      `Specimen Id`		= input$new_roi_specimen_id,
      `Study ID`		= input$new_roi_study_id,
      `Lab Source`	= input$new_roi_lab_source,
      `Tissue Type`		= input$new_roi_tissue_type,
      `Diagnosis`		= input$new_roi_diagnosis,
      `Tile Size`	= input$new_roi_tile_size
    )
    isolate(serverSideValues$roiDf <- bind_rows(serverSideValues$roiDf, tmp))
    updateNumericInput(session, "new_roi_name", value = paste0("Reg", str_pad( nrow(serverSideValues$roiDf)+1 , 3, pad = "0") ) )
  })


  observeEvent(input$save_entry_form_to_database, {
    print(input$new_entry_project_id)
    aqDF <- data.frame(
      name = input$new_entry_acquisition_name,
      project_id = input$new_entry_project_id,
      thickness = input$new_entry_thickness,
      date_cut = input$new_entry_cut_date,
      date_stained = input$new_entry_stain_date,
      date_rendered = input$new_entry_render_date,
      date_processed = input$new_entry_process_date,
      h2o2_bleached = input$new_entry_h2o2_bleach,
      z_stacks = input$new_entry_zstack,
      machine_name = input$new_entry_machine_name,
      antigen_retrival = input$new_entry_antigen_retrival
    )
    dbWriteTable(fCon, "acquisitions", aqDF, append=TRUE)     
    aqRez <- dbGetQuery(fCon, "SELECT id,name FROM acquisitions ORDER BY id DESC LIMIT 1")
    print(aqRez)
    if(input$new_entry_acquisition_name == aqRez$name){
      showNotification(paste("Sucessfully Uploaded Acquisition", aqRez$name), duration = 80, type="message")
      ### Remove/Clear Inputs!
      updateTextInput(session, "new_entry_acquisition_name", value = "")
      updateNumericInput(session, "new_entry_thickness", value = 1 )
      updateNumericInput(session, "new_entry_section_to_image", value = 0.0 )
      updateTextInput(session, "new_entry_antigen_retrival", value = "")
      updateNumericInput(session, "new_entry_zstack", value = 0)
    } else{
      showNotification(paste("An Error Occured in Acquisition", input$new_entry_acquisition_name), duration = 120, type="error")
    }
    
    tmp <- serverSideValues$multiplexDf
    tmp$acquisition_id <- aqRez$id
    colnames(tmp) <- tolower(gsub('\\.','_',names(tmp)))
    print(tmp)
    dbWriteTable(fCon, "multiplex", tmp, append=TRUE)     
    serverSideValues$multiplexDf <- getEmptyMultiplexDataFrame()
    
    
    tmp <- serverSideValues$roiDf
    tmp$acquisition_id <- aqRez$id
    colnames(tmp) <- tolower(gsub('\\.','_',names(tmp)))
    print(tmp)
    dbWriteTable(fCon, "rois", tmp, append=TRUE)     
    serverSideValues$roiDf <- getEmptyMultiplexDataFrame()
    ## Wipe ROI FORM AFTER SAVE...
    updateTextInput(session, "new_roi_name", value = "")
    updateTextInput(session, "new_roi_tma_source", value = "")
    updateTextInput(session, "new_roi_specimen_id", value = "")
    updateTextInput(session, "new_roi_lab_source", value = "")
    updateTextInput(session, "new_roi_tissue_type", value = "")
    updateTextInput(session, "new_roi_diagnosis", value = "")
    updateTextInput(session, "new_roi_tile_size", value = "")
  })

  
  
  
  #############################################################
  #####           Entry - PPT Upload Tab                  #####
  #############################################################
  
  output$ProjectPickerSyncPlex <- renderUI({
    list <-  get_OpenProjectNames()$id
    names(list) <- get_OpenProjectNames()$name
    selectInput("ppt_find_project_id", label = "Project Name:", choices = list)
  })
  
  output$AquisitionPickerSyncPlex <- renderUI({
    req(input$ppt_find_project_id)
    rez <- get_AcquisitionDropdownByID(input$ppt_find_project_id)
    list <- rez$id
    names(list) <- rez$name
    selectInput("ppt_find_aquisition_id", label = "Acquisition Name:", choices = list)
  })
  
  output$ROIPickerSyncPlex <- renderUI({
    req(input$ppt_find_aquisition_id)
    rez <- get_RoiDropdownByID(input$ppt_find_aquisition_id)
    list <- rez$id
    names(list) <- rez$name
    selectInput("ppt_find_ROI_id", label = "ROI:", choices = list)
  })
  
  
  observeEvent(input$ppt_find_ROI_id, {
  output$selectedMultiplexMetricsTable = renderDT(
    getMetericsTableCondensed(input$ppt_find_aquisition_id, input$ppt_find_ROI_id), 
    selection = 'none', rownames = F, editable = F, options = list(searching = FALSE)
  )
  })
  
  ### Add a warning if database already has PPT uploaded for this ROI/AQui
  ### When File is UPLOADED
  observeEvent(input$pptCodexProcessorFile, {
    print(input$pptCodexProcessorFile$datapath)
    show_modal_spinner(spin = "fading-circle", text="Reading & Formatting Data/Images") # show the modal window
    start_time <- Sys.time()
  
    #fh <- "/bigdata/bsi/immunology/s215555.Melanoma_MxIF/RShinyCodexTracking/CodexProjectTracking/data/2020-03-25_132626-TMA_AKOYA ab 3X_16MARCH2020-reg001.pptx"
    f <- pptx_summary(input$pptCodexProcessorFile$datapath)
    #f <- pptx_summary(fh)
    runInfo <- cbind(getTitles(f),getMeta(f),getProcessing(f))
    ## send output to user to confirm before loading to DB?? Could be too difficult right now....Save then show...delete later?
    runInfo$acquisition_id <- input$ppt_find_aquisition_id
    runInfo$roi_id <- input$ppt_find_ROI_id
    dbWriteTable(fCon, "processor_configurations", runInfo, append=TRUE)
    
    multiplexMetrics <- getSummary(f)
    ## Cannot to tie to Multiplex Table...it is possible there could be differences...need to handle that in the app.
    print(paste("aquisition_id:", input$ppt_find_aquisition_id, "ROI_id:", input$ppt_find_ROI_id))
    multiplexMetrics$acquisition_id <- input$ppt_find_aquisition_id
    multiplexMetrics$roi_id <- input$ppt_find_ROI_id
    dbWriteTable(fCon, "multiplex_metrics", multiplexMetrics, append=TRUE)
    metricsTable <- getMetericsTable(input$ppt_find_aquisition_id, input$ppt_find_ROI_id)
    print(paste("multiplexMetrics N rows: ",nrow(metricsTable) ))
    
    doc <- read_pptx(fh)
    summarySlides <- f %>% filter(grepl("^Summary Table",text)) %>% arrange(desc(slide_id))
    stSlide <- summarySlides[1,]$slide_id+1 ## Detailed images start after this...
    cnt=0
    # i=15
    for(i in stSlide:max(f$slide_id)){
      cnt=cnt+1
      print(paste("Slide #",i)) 
      slideAB <- f %>% filter(slide_id == i & content_type == 'paragraph') %>% tail(1)
      timg <- f %>% filter(slide_id == i & content_type == 'image')
      
      media_file <- timg[1,]$media_file ### 1 = histogram
      png_file <- tempfile(fileext = ".png")
      media_extract(doc, path = media_file, target = png_file)
      pp <- readPNG(png_file,info=TRUE)
      histoDimensions <- paste(unlist(dim(pp))[1:2],collapse = "x")
      ## read temp file as a binary string
      #histogram_binary <- paste(readBin(png_file, what="raw", n=1e6), collapse="")
      histogram64 <- base64Encode(readBin(png_file, "raw", file.info(png_file)[1, "size"]), "txt")
      #class(readBin(png_file, what="raw", n=1e6))
      #length(readBin(png_file, what="raw", n=1e6))
      
      media_file <- timg[2,]$media_file ### 2 = full size AB slide image
      
      png_file2 <- tempfile(fileext = ".png")
      media_extract(doc, path = media_file, target = png_file2)
      ## Resize this PNG image to save space...
      imgH <- image_read(png_file2)
      nX <- as.integer( image_info(imgH)$width / 2.5 )
      imgH <- image_scale(imgH, nX)
      image_write(imgH, path = png_file2)
      ## Now read-back manipulated image
      pp <- readPNG(png_file2,info=TRUE)
      rawDimensions <- paste(unlist(dim(pp))[1:2],collapse = "x") # RAW: ~ "2016x2688"
      ## read temp file as a binary string
      #raw_binary <- paste(readBin(png_file2, what="raw", n=1e6), collapse="")
      raw64 <- base64Encode(readBin(png_file2, "raw", file.info(png_file2)[1, "size"]), "txt")
      #class(raw_binary)
      singleEntry <- data.frame('title' = slideAB[1,]$text, 'histogram' = as.character(histogram64), 'histogram_dimensions' = histoDimensions,
                 'raw_image'=as.character(raw64), 'raw_dimensions'=rawDimensions,  'multiplex_metrics_id'= metricsTable[cnt,]$id) 
      
      dbWriteTable(fCon, "metric_images", singleEntry, append=TRUE)
    }
    
    end_time <- Sys.time()
    remove_modal_spinner() # remove it when done
    print(paste("PPT Preparations:",round(difftime(end_time, start_time, units = "mins"),3),"mins"))
  })
  
  
  
  
  
  
  
  #############################################################
  #####       Review Marks - JC Comments Tab              #####
  #############################################################
  output$ProjectPickerReviewer <- renderUI({
    list <- get_OpenProjects_WithMetricsUploaded()$id
    names(list) <- get_OpenProjects_WithMetricsUploaded()$name
    selectInput("review_find_project_id", label = "Project Name:", choices = list)
  }) 
  
  output$AquisitionPickerReviewer <- renderUI({
    req(input$review_find_project_id)
    rez <- get_AcquisitionDropdownByID_WithMetricsUploaded(input$review_find_project_id)
    list <- rez$id
    names(list) <- rez$name
    selectInput("review_find_aquisition_id", label = "Acquisition Name:", choices = list)
  })
  
  output$ROIPickerReviewer <- renderUI({
    req(input$review_find_aquisition_id)
    rez <- get_RoiDropdownByID_WithMetricsUploaded(input$review_find_aquisition_id)
    list <- rez$id
    names(list) <- rez$name
    selectInput("review_find_ROI_id", label = "ROI:", choices = list)
  })
  
  
  observeEvent(input$review_find_ROI_id, {
    output$ReviewableParameters = renderDT(
      getSummaryParametersTransposed(input$review_find_aquisition_id, input$review_find_ROI_id), 
      selection = 'none', rownames = F, editable = F, options = list(pageLength = 6,searching = FALSE)
    )
    output$ReviewableMetrics = renderDT(
      getMetericsTableCondensed(input$review_find_aquisition_id, input$review_find_ROI_id), 
      selection = 'none', rownames = F, editable = F, options = list(pageLength = 6,searching = FALSE)
    )
    
  })
  
  output$MultiplexRoundPickerReviewer <- renderUI({
    req(input$review_find_ROI_id)
    rez <- getMetericsTable(input$review_find_aquisition_id, input$review_find_ROI_id)
    list <- rez$id
    names(list) <- paste(rez$marker," :    cyc",rez$cyc," ch",rez$ch)
    selectInput("review_find_multiplex_metric_id", label = "ROI:", choices = list)
  })
  
  observeEvent(input$fetchMultiplexImage, {
    req(input$review_find_multiplex_metric_id)
    print(paste("Fetch MM id:",input$review_find_multiplex_metric_id))
    rez <- getMetricsImagesByMultiplexMetricID(input$review_find_multiplex_metric_id)
    #rez <- getMetricsImagesByMultiplexMetricID(1)
    #output$ReviewHistogram <- renderImage({ list(src = histPNG_fh, contentType = 'image/png',alt = "This is alternate text") }, deleteFile = TRUE)
    output$ReviewHistogram <-  renderUI({
      div(class = "mypptImage",
      tags$img(src = paste0("data:image/png;base64,",rez$histogram), alt="Histogram Base64") ) 
    })
    output$ReviewBigRaw <-  renderUI({
      div(class = "mypptImage",
          tags$img(src = paste0("data:image/png;base64,",rez$raw_image), alt="Raw PPT Base64") ) 
    })
  })
  
  observeEvent(input$openModelForMultiplexComments, {
    showModal(newReviewer1Modal(session$ns))
  })
  
  observeEvent(input$add_new_review, {
    req(input$review_find_multiplex_metric_id, input$review_find_ROI_id)
    #addNewQCReview(input$new_review_qc_grade, input$new_review_qc_comment)
    singleComment <- data.frame('qc_grade' = input$new_review_qc_grade, 'comment' = input$new_review_qc_comment, 'created_on' = Sys.Date(),
                              'user'=session$user, 'roi_id'=input$review_find_ROI_id,  'multiplex_metrics_id'= input$review_find_multiplex_metric_id)
    dbWriteTable(fCon, "quality_comments", singleComment, append=TRUE)
    showNotification(paste("Review By:",session$user,"Successfully Added!"), duration = 100, type = "message")
    removeModal()
  })
  
  
  
  
  
  
  
  #############################################################
  #####              Home - Metics Tab                    #####
  #############################################################
  output$field_picker <- renderUI({
    selectInput(inputId = "feild_choice", label = "Color By", choices = sort(relavent_input_columns), selected = 'Tissue_Type' )
  })
  
  # output$metric_picker <- renderUI({
  #   lst <- as.character(grep('^Metrics',names(data),value = T),invert = T,value = T)
  #   lst <- lst[lst %nin% c("Metrics_1","Metrics_Marker")]
  #   names(lst) <- gsub('Metrics_','', lst)
  #   selectInput(inputId = "metric_choice", label = "Metric", choices = lst, selected = 'Metrics_SNR' )
  # })
  # 
  # output$field_strata_channels_plot <- renderPlot({
  #   t <- meta_columns[which(meta_columns$field == input$feild_choice),]$type
  #   clms <- c("Acquisition_Name","ROI","Antibody","Channel",input$feild_choice,input$metric_choice)
  #   subTbl <- data %>% select(clms) %>% distinct() %>% mutate(Antibody = if_else(is.na(Antibody),'Blank',Antibody)) %>% 
  #     filter(Channel != "1") %>% mutate(Channel = paste("Channel",Channel)) %>% filter(!is.na(.data[[input$metric_choice]] ))
  #   #print(subTbl)
  #   if(t == "character"){
  #     ggplot(subTbl,aes_string(x="Antibody",y=input$metric_choice,fill=input$feild_choice))+geom_boxplot()+
  #       facet_wrap(Channel ~ ., scales = "free", ncol = 1)+ theme_bw()+
  #       theme(axis.text.x = element_text(angle = 15,hjust = 1), legend.position = "bottom" )
  #     
  #     
  #   } else if (t == "numeric"){
  #     #stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")+
  #     dr <- (max(subTbl[,input$feild_choice],na.rm = T) - min(subTbl[,input$feild_choice],na.rm = T) )/50
  #     ggplot(subTbl,aes_string(x=input$feild_choice,y=input$metric_choice))+geom_jitter(width = dr)+
  #       facet_wrap(Channel ~ Antibody, scales = "free_y")+ theme_bw()+
  #       theme(axis.text.x = element_text(angle = 15,hjust = 1), legend.position = "bottom" )
  #     
  #   } else if (t == "POSIXt"){
  #     subTbl[,input$feild_choice] <- as.Date( subTbl[,input$feild_choice] )
  #     ggplot(subTbl, aes_string(x=input$feild_choice,y=input$metric_choice,group="Antibody", color="Antibody") )+geom_jitter(size=0.6)+geom_line()+
  #       scale_x_date(date_labels = "%b/%d/%y", date_breaks="6 weeks")+
  #       theme_minimal()+
  #       facet_wrap(Channel ~ ., scales = "free", ncol = 1)+ 
  #       theme(axis.text.x=element_text(angle=30, hjust=1),legend.position = 'bottom') 
  #   } else {
  #     print(paste("Dynamic Tab Error:",input$feild_choice,"vs",input$metric_choice))
  #     ggplot() + annotate("text", x = 4, y = 25, size=8, label = "ERROR: Un-handled Exception!", color="red") + theme_void()
  #   }
  # })
  # 
  
  
  
  
  
  #############################################################
  #####               Static Tab Filters                  #####
  #############################################################
  output$tissue_picker <- renderUI({
    selectInput(inputId = "home_tissue_type", label = "Tissue Type", choices = get_DistinctTissues()  )
  })
  
  output$channel_picker <- renderUI({
    selectInput(inputId = "home_channel_int", label = "Channel", choices = c('DAPI'='1','Cy2'='2','Cy3'='3','Cy5'='4'), selected = '2' )
  })
  
  output$static_metric_picker <- renderUI({
    # lst <- as.character(grep('^Metrics',names(data),value = T),invert = T,value = T)
    # lst <- lst[lst %nin% c("Metrics_1","Metrics_Marker")]
    # names(lst) <- gsub('Metrics_','', lst)
    lst <- c("placeholder")
    selectInput(inputId = "static_metric_choice", label = "Metric", choices = lst, selected = 'Metrics_95' )
  })
  
  # metricsDynamicTable <- reactive({
  #   req(input$home_tissue_type, input$home_channel_int)
  #   data %>% filter(Tissue_Type == input$home_tissue_type) %>% 
  #     filter(Channel == input$home_channel_int) %>%
  #     filter(input$home_date_stain_range[1] < Date_Stained & Date_Stained < input$home_date_stain_range[2]) %>%
  #     select(Acquisition_Name,ROI,Antibody,starts_with("Metrics_"),H2O2_bleached,contains("Date"))
  # })
  
  
  #############################################################
  #####                   Bleach Page                     #####
  #############################################################
  output$bleachPiePlot <- renderPlot({
    t <- getEntireAcquisitionField('h2o2_bleached') %>% 
      mutate(h2o2_bleached = if_else(h2o2_bleached == 0,"No","N/A")) %>% 
      mutate(h2o2_bleached = if_else(h2o2_bleached == 1,"Yes",h2o2_bleached))
    pie2 <- as.data.frame(table(t$h2o2_bleached)) %>%
      mutate(total = sum(Freq)) %>%
      group_by(Var1) %>%
      mutate(perc = (Freq/total))
    ggplot(pie2, aes(x = '', y = perc, fill = Var1 )) + 
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = scales::percent(perc)),size=5, position = position_stack(vjust = 0.5) )+
      coord_polar("y", start = 0)+
      theme_minimal() + labs(fill='') +
      scale_fill_brewer( type = "div" , palette = "BuPu", direction = -1 )+
      theme(axis.title.x = element_blank(), axis.text.x=element_blank(),
            axis.title.y = element_blank(),axis.ticks = element_blank(),
            panel.grid  = element_blank(), legend.position='bottom',
            strip.text.x = element_text(size = 14),
            plot.title = element_text(size = 14, face = "bold")) +
      ggtitle("H2O2 Bleached Samples In Whole Database")
  })
  
  output$bleachBarPlot <- renderPlot({
    t <- getEntireAcquisitionField('h2o2_bleached') %>% 
      mutate(h2o2_bleached = if_else(h2o2_bleached == 0,"No","N/A")) %>% 
      mutate(h2o2_bleached = if_else(h2o2_bleached == 1,"Yes",h2o2_bleached))
    
    t <- data %>% select(Acquisition_Name,ROI,Tissue_Type,H2O2_bleached) %>% 
      distinct() %>% mutate(Tissue_Type = if_else(is.na(Tissue_Type),'N/A',Tissue_Type))
    
    ggplot(t, aes(x=Tissue_Type,fill=H2O2_bleached))+geom_bar()+
      theme_minimal()+ggtitle("Frequency of H2O2 Bleached Samples")+ylab('')+
      theme(axis.text.x=element_text(angle=30, hjust=1),legend.position = 'bottom') 
  })
  
  output$bleachMetricsPlot <- renderPlot({
    t3 <- metricsDynamicTable() %>% select(Acquisition_Name,ROI,Antibody,H2O2_bleached,starts_with("Metrics_")) %>%
      filter(!is.na(Metrics_SNR)) %>% mutate(Slide = paste(Acquisition_Name,ROI,sep='-'))
    ggplot(t3, aes_string(x="Antibody", y=input$static_metric_choice , fill="H2O2_bleached"))+geom_boxplot()+
      theme_minimal()+ggtitle(paste("Codex Metrics:",input$home_tissue_type,"Ch",input$home_channel_int))+
      theme(axis.text.x = element_text(angle = 10,hjust = 1), legend.position = "bottom" )
  })
  
  
  
  #############################################################
  #####                     Time Page                     #####
  #############################################################
  
  output$cutdataPlot <- renderPlot({
    tt <- data %>% select(Acquisition_Name,ROI,contains("Date")) %>% distinct() %>% 
      gather(Event,date,-Acquisition_Name,-ROI) %>% group_by(Event,date) %>% summarise(cnt = n()) %>% 
      mutate(date = as.Date(date)) %>% filter(!is.na(date))
    ggplot(tt,aes(x=date,y=cnt, group=Event, color=Event))+geom_point(size=0.6)+
      geom_smooth(method = 'loess',se=F)+
      scale_x_date(date_labels = "%b/%d/%y", date_breaks="6 weeks")+
      theme_minimal()+ggtitle("Frequency of All Processing Events")+ylab('')+
      theme(axis.text.x=element_text(angle=30, hjust=1),legend.position = 'bottom') 
  })
  
  output$cutdataBarPlot <- renderPlot({
    tt <- metricsDynamicTable() %>% select(Acquisition_Name,ROI,contains("Date")) %>% distinct() %>% 
      gather(Event,date,-Acquisition_Name,-ROI) %>% group_by(Event,date) %>% summarise(cnt = n()) %>% 
      mutate(date = as.Date(date)) %>% filter(!is.na(date))
    ggplot(tt,aes(x=date,y=cnt, group=Event, fill=Event))+geom_bar(stat = "identity")+
      scale_x_date(date_labels = "%b/%d/%y", date_breaks="2 weeks")+
      theme_minimal()+ggtitle(paste("Frequency of Subset:",input$home_tissue_type))+ylab('# of Samples')+
      theme(axis.text.x=element_text(angle=30, hjust=1),legend.position = 'bottom') 
  })
  
  
  
  ########################################################################
  #####               Parameters Pie Chart  Page                     #####
  ########################################################################
  output$parameter_picker <- renderUI({
    aqFields <- c("thickness","h2o2_bleached","section_to_image_interval","z_stacks","machine_name","antigen_retrival")
    names(aqFields) <- gsub("_"," ",str_to_title(aqFields))
    selectInput(inputId = "processing_parameters_choice", label = "Processing Configurations", choices = aqFields )
  })
  
  output$pptParamsPiePlot <- renderPlot({
    t <- getEntireAcquisitionField(input$processing_parameters_choice)
    colourCount <- length(unlist(unique(t[,1])))
    pie2 <- as.data.frame(table(t[,1])) %>%
      mutate(total = sum(Freq)) %>%
      group_by(Var1) %>%
      mutate(perc = (Freq/total))
    ggplot(pie2, aes(x = '', y = perc, fill = Var1 )) + 
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = scales::percent(perc)),size=5, position = position_stack(vjust = 0.5) )+
      coord_polar("y", start = 0)+
      theme_minimal() + labs(fill='') +
      scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Accent"))(colourCount) )+
      theme(axis.title.x = element_blank(), axis.text.x=element_blank(),
            axis.title.y = element_blank(),axis.ticks = element_blank(),
            panel.grid  = element_blank(), legend.position='bottom',
            strip.text.x = element_text(size = 14),
            plot.title = element_text(size = 14, face = "bold")) +
      ggtitle(input$processing_parameters_choice)
  })
  
  output$expmetadata_picker <- renderUI({
    pptFields <- c("expmeta_number_of_cycles","expmeta_region_width","expmeta_region_height","expmeta_magnification","expmeta_aperture","expmeta_immersion",
                   "expmeta_xy_resolution","expmeta_z_pitch","expmeta_number_of_z_slices","expmeta_tile_overlap","procparams_version","procparams_time",
                   "procparams_background_subtraction","procparams_deconvolution","procparams_extended_depth_of_field","procparams_shading_correction",
                   "procparams_diagnostic_output")
    names(pptFields) <- gsub("_"," ",str_to_title(gsub("procparams_",'',pptFields)))
    selectInput(inputId = "experimentmetadata_choice", label = "Experiment Metadata", choices = pptFields )
  })
  
  output$pptExpMetaPiePlot <- renderPlot({
    t <- getEntirePPMetricsField(input$experimentmetadata_choice)
    colourCount <- length(unlist(unique(t[,1])))
    pie2 <- as.data.frame(table(t[,1])) %>%
      mutate(total = sum(Freq)) %>%
      group_by(Var1) %>%
      mutate(perc = (Freq/total))
    ggplot(pie2, aes(x = '', y = perc, fill = Var1 )) + 
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = scales::percent(perc)),size=5, position = position_stack(vjust = 0.5) )+
      coord_polar("y", start = 0)+
      theme_minimal() + labs(fill='') +
      scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(colourCount) )+
      theme(axis.title.x = element_blank(), axis.text.x=element_blank(),
            axis.title.y = element_blank(),axis.ticks = element_blank(),
            panel.grid  = element_blank(), legend.position='bottom',
            strip.text.x = element_text(size = 14),
            plot.title = element_text(size = 14, face = "bold")) +
      ggtitle(input$experimentmetadata_choice)
  })
  
   
  
  ########################################################################
  #####               Viewing and Review  Page                     #####
  ########################################################################
  
  output$TissuePickerReviewer <- renderUI({
    list <- na.omit(get_DistinctTissues())
    selectInput("review2_find_tissue_type", label = "Tissue Type:", choices = list)
  }) 
  
  output$AquisitionPickerReviewer2 <- renderUI({
    req(input$review2_find_tissue_type)
    rez <- get_AcquisitionDropdownByTissue(input$review2_find_tissue_type)
    list <- rez$id
    names(list) <- rez$name
    selectInput("review2_find_aquisition_id", label = "Acquisition Name:", choices = list)
  })
  
  output$ROIPickerReviewer2 <- renderUI({
    req(input$review2_find_aquisition_id)
    rez <- get_RoiDropdownByID_WithMetricsUploaded(input$review2_find_aquisition_id)
    list <- rez$id
    names(list) <- rez$name
    selectInput("review2_find_ROI_id", label = "ROI:", choices = list)
  })
  
  
  observeEvent(input$review2_find_ROI_id, {
    output$ReviewableParameters2 = renderDT(
      getSummaryParametersTransposed(input$review2_find_aquisition_id, input$review2_find_ROI_id), 
      selection = 'none', rownames = F, editable = F, options = list(pageLength = 6,searching = FALSE)
    )
    output$ReviewableMetrics2 = renderDT(
      getMetericsTableCondensed(input$review2_find_aquisition_id, input$review2_find_ROI_id), 
      selection = 'none', rownames = F, editable = F, options = list(pageLength = 6,searching = FALSE)
    )
    
  })
  
  
  
  
  
  
  
  
  #############################################################
  #####                     Data Page                     #####
  #############################################################
  output$ProjectPickerForData <- renderUI({
    list <- get_AnyProjectNames()$id
    names(list) <- get_AnyProjectNames()$name
    selectInput("for_data_pick_any_project", label = "Project Name:", choices = list)
  })

  #subData <- data %>% select(Acquisition_Name,Experiment_Number,Project,Specimen_Origin,Tissue_Type,Diagnosis,Exp_File_Name,pptTitle) %>% distinct()
  #output$FullDataTable <- renderDT(subData, selection = 'none', rownames = F, editable = F)
  
})
