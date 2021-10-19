#########################################
###          projects table           ###
#########################################
get_ProjectCount <- function(){
  return( as.numeric(dbGetQuery(fCon, "SELECT COUNT(DISTINCT id) from projects;")[[1]]) )
}

get_OpenProjectNames <- function(){
  return( dbGetQuery(fCon, "SELECT DISTINCT id, name from projects WHERE status = 'Open';")  )
}

get_AnyProjectNames <- function(){
  return( dbGetQuery(fCon, "SELECT DISTINCT id, name from projects;")  )
}


get_ProjectDescriptionById <- function(pid){
  return( dbGetQuery(fCon, paste0("SELECT description from projects WHERE id = '",pid,"';"))[[1]]  )
}

add_NewProject <- function(nom,desc,people){
  print("Function: add_NewProject()")
  if(nom == ""){
    return(FALSE)
  } else {
    print(paste(nom,desc,people))
    df <- data.frame('name'=c(nom), 'description'=c(desc), 'primary_contact'=c(people),
                     'created_on'=c(Sys.Date()), 'status'=c('Open'))
    dbWriteTable(fCon, "projects", df, append = TRUE)
    return(TRUE)
  }
}

close_ProjectById <- function(pid, txt){
  txt <- gsub("'","''",txt)
  dbExecute(fCon, paste0("UPDATE projects SET status = 'Closed', description = '",txt,"' WHERE id = '",pid,"';"))
}


get_OpenProjects_WithMetricsUploaded <- function(){
  return( dbGetQuery(fCon, "SELECT DISTINCT p.id, p.name FROM projects AS p INNER JOIN acquisitions aq ON (p.id = aq.project_id) INNER JOIN multiplex_metrics mm ON (aq.id = mm.acquisition_id) WHERE p.status = 'Open';")  )
}


get_UserRoles <- function(user){
  return( dbGetQuery(fCon, paste0("SELECT role FROM user_roles WHERE user = \"",user,"\"")) )
}


#########################################
###         barcodes table            ###
#########################################
getAllBarcodes <- function(){
  return( dbGetQuery(fCon, "SELECT name,channel,reporter_fluorphore FROM barcodes;")  )
}

get_Barcode <- function(){
  return( dbGetQuery(fCon, "SELECT DISTINCT id, name from barcodes;")  )
}


#########################################
###       acquisitions table          ###
#########################################
get_AcquisitionCount <- function(){
  return( as.numeric(dbGetQuery(fCon, "SELECT COUNT(DISTINCT id) from acquisitions;")[[1]]) )
}

get_AcquisitionDropdownByID <- function(pid){
  return( dbGetQuery(fCon, paste0("SELECT DISTINCT id, name from acquisitions WHERE project_id = ",pid,";") ))
}

get_AcquisitionDropdownByID_WithMetricsUploaded <- function(pid){
  return( dbGetQuery(fCon, paste0("SELECT DISTINCT aq.id, aq.name from acquisitions AS aq INNER JOIN multiplex_metrics mm ON (aq.id = mm.acquisition_id) WHERE project_id = ",pid,";") ))
}

get_AcquisitionDropdownByTissue <- function(tStr){
  return( dbGetQuery(fCon, paste0("SELECT DISTINCT aq.id, aq.name from acquisitions AS aq INNER JOIN rois r ON (aq.id = r.acquisition_id) WHERE tissue_type = '",tStr,"';") ))
}


get_RoiDropdownByID <- function(aid){
  return( dbGetQuery(fCon, paste0("SELECT DISTINCT id, name FROM rois WHERE acquisition_id = ",aid," ORDER BY name;") ))
}

get_RoiDropdownByID_WithMetricsUploaded <- function(aid){
  return( dbGetQuery(fCon, paste0("SELECT DISTINCT r.id, r.name from rois AS r INNER JOIN multiplex_metrics mm ON (r.id = mm.roi_id) WHERE r.acquisition_id = ",aid,";") ))
}

getLoadedAquisitionsTable <- function(){
  return( dbGetQuery(fCon,"SELECT a.id,a.name,r.id,r.name,mm.reg FROM acquisitions as a JOIN rois as r ON a.id = r.acquisition_id LEFT JOIN multiplex_metrics as mm ON a.id = mm.acquisition_id AND r.id = mm.roi_id;"))
}

getAllAquisitionsRois <- function(){
  return( dbGetQuery(fCon,"SELECT * FROM acquisitions as a JOIN rois as r ON a.id = r.acquisition_id;"))
}

getEntireAcquisitionField  <- function(field){
  return( dbGetQuery(fCon, paste0("SELECT ",field," FROM acquisitions;") ))
}



#########################################
###         multiplex table           ###
#########################################
get_MultiplexCount <- function(){
  return( as.numeric(dbGetQuery(fCon, "SELECT COUNT(DISTINCT id) from multiplex;")[[1]]) )
}

get_QCFailFlagCount <- function(){
  return( as.numeric(dbGetQuery(fCon, "SELECT COUNT(DISTINCT id) from quality_comments;")[[1]]) )
}

get_BarCodeValues <- function(bid){
  return( dbGetQuery(fCon, paste0("SELECT name,channel,reporter_fluorphore FROM barcodes WHERE id = '",bid,"';"))[1,] )
}

getEmptyMultiplexDataFrame <- function(){
  t <- data.frame(
    `Cycle`	= numeric(),
    `Antibody`	= character(),
    `Vendor`		= character(),
    `AB Clone`		= character(),
    `AB Concentration`	= numeric(),
    `Barcode`		= character(),
    `Channel`		= numeric(),
    `Reporter Fluorphore`	= character(),	
    `Exposure Time`		= numeric()
  )
  return(t)
}


#########################################
###            ROI  table             ###
#########################################
get_ROICount <- function(){
  return( as.numeric(dbGetQuery(fCon, "SELECT COUNT(DISTINCT id) from rois;")[[1]]) )
}

getEmptyRoiDataFrame <- function(){
  t <- data.frame(
    `Name`	= character(),
    `TMA`	= character(),
    `Specimen Id`		= character(),
    `Study ID`		= character(),
    `Lab Source`	= character(),
    `Tissue Type`		= character(),
    `Diagnosis`		= character(),
    `Tile Size`	= character()
  )
  return(t)
}

get_DistinctTissues <- function(){
  return( sort( dbGetQuery(fCon, "SELECT DISTINCT tissue_type from rois;")$tissue_type) )
}

get_DistinctDiagnosis <- function(){
  return( dbGetQuery(fCon, "SELECT DISTINCT diagnosis from rois;")$diagnosis )
}

#########################################
###        Metrics  tables            ###
#########################################

getMetericsTable <- function(aid,rid){
  return( dbGetQuery(fCon, paste0("SELECT id,marker,cyc,ch FROM multiplex_metrics WHERE acquisition_id = '",aid,"' AND roi_id = '",rid,"';")) )
}

getMetericsTableCondensed <- function(aid,rid){
  return( dbGetQuery(fCon, paste0("SELECT marker,cyc,ch,snr,min,max,median,mean,std_dev,threshold FROM multiplex_metrics WHERE acquisition_id = '",aid,"' AND roi_id = '",rid,"';")) )
}

get_ImageCount <- function(){
  return( as.numeric(dbGetQuery(fCon, "SELECT COUNT(DISTINCT id) from metric_images;")[[1]]) )
}

getEntirePPMetricsField  <- function(field){
  return( dbGetQuery(fCon, paste0("SELECT ",field," FROM processor_configurations;") ))
}


#aid=58,rid=451
getSummaryParametersTransposed <- function(aid,rid){
  return( as.data.frame(t(dbGetQuery(fCon, paste0("SELECT * FROM processor_configurations WHERE acquisition_id = '",aid,"' AND roi_id = '",rid,"';")))) %>%
    rename(Value = V1) %>% rownames_to_column("Parameter") %>% filter( !grepl('id',Parameter) ) )
}

getMetricsImagesByMultiplexMetricID <- function(mid){
  return( dbGetQuery(fCon, paste0("SELECT * FROM metric_images WHERE multiplex_metrics_id = ",mid,";")) )
}
