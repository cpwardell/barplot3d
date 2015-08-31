#' Barplot 3D
#'
#' Creates 3D barplots, with many configurable options. For detailed usage, please read the accompanying vignette.
#' 
#' @param z Numeric vector of the heights of each column
#' @param dimensions  Numeric vector of length two; the number of rows and columns of the base of the plot
#' @param scalexy     Numeric value used to scale the x and y coordinates (i.e. the "fatness" of the columns). Also scales the gap between columns
#' @param scalez      Numeric value used to scale the z input vector. Useful for playing with data, but not recommended
#' @param gap         Numeric value determining the gap between columns; this gap is identical in the x and y dimensions
#' @param alpha       Numeric value determining the transparency of all column sides. Ranges beween 0 (invisible) and 1 (opaque)
#' @param colors      Colour of columns.  Either a single colour or a character vector of colours, one for each column
#' @param linecol     Colour of edges of columns. A single colour.
#' @details Produces a 3D barplot using a numeric vector of values. The \code{\link{rgl}} package is used to draw the plots.
#' Further details are available in the accompanying vignette.
#' @return Draws 3D barplot; only the most recent object IDs will be returned.
#' @export
#' @import rgl
#' @author Christopher Wardell \email{r@@cpwardell.com}
#' @examples
#' ## The vignette provides far more in-depth explanation and examples ##
#' blah blah blah

## To do:
# y and x axes
# background mesh
# size of plot
# creation of png output
# theta, phi and fov
# theta, phi output?
# Note: fills from left to right, front to back 
# Check arguments are present and appropriate

## Calls singlecolumn repeatedly to create a barplot
## z is the heights of the columns and must be an appropriately named vector
barplot3d=function(z,dimensions,scalexy=1,scalez=1,gap=0.2,alpha=1,colors=NA,linecol="black"){
  
  ## These lines allow the active rgl device to be updated with multiple changes
  ## This is necessary to add each column sequentially
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))
  
  ## Scale column area and the gap between columns 
  y=seq(1,dimensions[1])*scalexy
  x=seq(1,dimensions[2])*scalexy
  gap=gap*scalexy
  
  ## Scale z coordinate
  z=z*scalez
  
  ## Set up colour palette if no colours supplied in arguments
  ## If no colour is supplied, the default is green
  ## If a single colour is supplied it will be applied to all columns
  if(length(colors)==1){
    if(is.na(colors)){
      colors=rep("#078E53",length(z))
    }else{
      colors=rep(color,length(z))
    }
  }
    
  ## Plot each of the columns
  for(i in 1:dimensions[1]){
    for(j in 1:dimensions[2]){
      it=(i-1)*dimensions[2]+j # Variable to work out which column to plot; counts from 1:96
      singlecolumn(c(gap+x[j],x[j]+scalexy),
                   c(-gap-y[i],-y[i]-scalexy),
                   z[it],
                   alpha=alpha,
                   topcol=colors[it],
                   sidecol=colors[it],
                   linecol=linecol)
    }
  }
  ## Set the viewpoint and add axes and labels
  rgl.viewpoint(theta=50,phi=40,fov=0)
  axes3d("y-+",labels=TRUE,col="black")
}
# Example:
# context3d(counts)



#### END OF FUNCTIONS

## Read in example data and cast to an appropriate vector
#rawdata=read.table("snvspermegabase.txt",header=TRUE)
#counts=as.numeric(rawdata)
#names(counts)=colnames(rawdata)

## Example plots
#context3d(counts)
#context3d(counts,alpha=0.4)

## Save your images to files if you wish
#rgl.snapshot(filename="example.png")