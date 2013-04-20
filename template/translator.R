#' Translate the data object to the format expected by current template
#'
#' @param data input data object
#' @param opts options of current template
#' @return The opts variable with the opts$data variable filled in
translate <- function(opts) {
  require(df2json)
  require(reshape2)
  
  #translate xts to time series to json with date and data
  #for this behavior will be more generic than the original
  #data will not be transformed, so template.rmd will be changed to reflect
  
  
  #convert to data frame
  data.df <- data.frame(cbind(format(index(opts$data),"%Y-%m-%d"),coredata(opts$data)))
  colnames(data.df)[1] = "date"
  data.melt <- melt(data.df,id.vars=1,stringsAsFactors=FALSE)
  colnames(data.melt) <- c('date','indexname','value')
  #remove periods from indexnames to prevent javascript confusion
  #these . usually come from spaces in the colnames when melted
  data.melt[,"indexname"] <- apply(matrix(data.melt[,"indexname"]),2,gsub,pattern="[.]",replacement="")
  
  return(df2json(na.omit(data.melt)))
  
}
