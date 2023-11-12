read_yara <- function(device_id,from_date,until_date,dest_file = NULL,...){
  url <- 'https://yara.zim-probe.com/datas/user_data/param:'
  yourZimProbeUsername <- 'CL-Universidad Mayor FZ'
  yourZimProbePassword <- '12hepok34'
  user_id <- '911'
  
  device_id <- paste(as.character(device_id),collapse = '","')
  
  query <- glue::glue('"username":"{yourZimProbeUsername}","password":"{yourZimProbePassword}","user_id":["{user_id}"],"device_id":["{device_id}"],"from_date":"{from_date}","until_date":"{until_date}"')
  
  if(is.null(dest_file)) file <- paste0(tempfile(),'.csv') else file <- dest
  
  download.file(paste0(url,'{',query,'}'),destfile = file,method = 'wget', ...)
  
  if (is.null(dest_file)) {
    return(readr::read_delim(file, delim = '\t'))
    unlink(dest_file)
  }
}