#' Title
#'
#' @param device_id 
#' @param from_date 
#' @param until_date 
#' @param dest 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
read_yara <- function(device_id,from_date,until_date,dest_file){
  url <- 'https://yara.zim-probe.com/datas/user_data/param:'
  yourZimProbeUsername <- 'CL-Universidad Mayor FZ'
  yourZimProbePassword <- '12hepok34'
  user_id <- '911'
  
  device_id <- paste(as.character(device_id),collapse = '","')

  query <- glue::glue('"username":"{yourZimProbeUsername}","password":"{yourZimProbePassword}","user_id":["{user_id}"],"device_id":["{device_id}"],"from_date":"{from_date}","until_date":"{until_date}"')
  
  encoded_url <- URLencode(paste0(url,'{',query,'}'))
  
  download.file(encoded_url,destfile = dest_file,method = 'auto')
  
}

