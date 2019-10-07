#' Adds a 3D bar plot to the current RGL scene
#' @param rows How many rows the plotting area should have, an integer, e.g. 5.
#' @param cols How many columns the plotting area should have, an integer, e.g. 5.
#' @param x The x dimensions of each 3D bar, a vector of length 2 e.g. c(0,1).
#' @param y The y dimensions of each 3D bar, a vector of length 2 e.g. c(0,1).
#' @param z The height of each 3D bar, a numeric vector, e.g c(2,3,5,2,9).
#' @param alpha The alpha channel (transparency) of the sides of 3D bars. Range 0-1.
#' @param scalexy Scaling factor for x and y coordinates; this constant can be used to make the plot "skinnier" or "fatter".
#' @param gap Gap between 3D bars (recommended values are 0 or 0.2).
#' @param topcolors The color of the top of each 3D bar. Numeric vector of hexadecimal RGB colors, like those returned by rgb() e.g. "#078E53".
#' @param sidecolors The color of the top of the bar. Should be a hexadecimal RGB color, like that returned by rgb() e.g. "#aaaaaa".
#' @param linecolors The color of the edges of the bar. Should be a hexadecimal RGB color, like that returned by rgb() e.g. "#aaaaaa".
#' @param theta Polar coordinate for viewing the 3D barplot; range 0 to 360 (rotates the plot).
#' @param phi Polar coordinate for viewing the 3D barplot; range -90 to 90 (-90 is directly below, 90 directly above).
#' @param gridlines Draw gridlines on the plot (TRUE or FALSE).
#' @param xlabels Labels for the x axis (must be a vector of names the same length as "cols" parameter).
#' @param ylabels Labels for the y axis (must be a vector of names the same length as "rows" parameter).
#' @param zlabels Labels for the z axis; add numeric scale to the vertical dimension of the plot (TRUE or FALSE).
#' @param xsub Descriptive label for the x axis.
#' @param ysub Descriptive label for the y axis.
#' @param zsub Descriptive label for the z axis.
#' @return Nothing is returned (invisibly returns NULL).
#' @examples
#' barplot3d(rows=3,cols=5,z=1:12,topcolors=rainbow(12),alpha=0.7,scalexy=10,
#' xlabels=c("One","Two","Three","Four","Five"),ylabels=LETTERS[1:3])
#' @export
barplot3d<-function(rows,cols,x,y,z,alpha=1,scalexy=1,gap=0.2,topcolors=c("#000000"),sidecolors=c("#aaaaaa"),linecolors=c("#000000"),
                    theta=50,phi=40,gridlines=TRUE,xlabels=FALSE,ylabels=FALSE,zlabels=TRUE,xsub=FALSE,ysub=FALSE,zsub=FALSE){

  ## Test to ensure that the data matches the dimensions given
  if(length(z)>(rows*cols)){
    stop("Too many data points for dimensions of plot. z must be equal to or less than rows*cols", call. = FALSE)
  }
  ## Test that (if provided) the xlabels match the length of the cols parameter
  if(xlabels[1]!=FALSE){
    if(length(xlabels)!=cols){
      stop("Number of xlabels must be equal to the cols parameter", call. = FALSE)
    }
  }
  ## Test that (if provided) the ylabels match the length of the rows parameter
  if(ylabels[1]!=FALSE){
    if(length(ylabels)!=rows){
      stop("Number of ylabels must be equal to the rows parameter", call. = FALSE)
    }
  }
  ## If only one topcolor is given, replicate it for all bars
  if(length(topcolors==1)){
    topcolors=rep(topcolors,length(z))
  }
  ## If only one sidecolor is given, replicate it for all bars
  if(length(sidecolors==1)){
    sidecolors=rep(sidecolors,length(z))
  }
  ## If only one linecolor is given, replicate it for all bars
  if(length(linecolors==1)){
    linecolors=rep(linecolors,length(z))
  }

  ## These lines allow the active rgl device to be updated with multiple changes
  ## This is necessary to add each column sequentially
  save <- rgl::par3d(skipRedraw=TRUE)
  on.exit(rgl::par3d(save))

  ## Define dimensions of the plot
  dimensions=c(rows,cols)

  ## Scale column area and the gap between columns
  y=seq(1,dimensions[1])*scalexy
  x=seq(1,dimensions[2])*scalexy
  gap=gap*scalexy

  ## Plot each of the columns
  for(i in 1:dimensions[1]){
    for(j in 1:dimensions[2]){
      it=(i-1)*dimensions[2]+j # Variable to work out which column to plot; counts from 1:length(z)
      if(it>length(z)){next} # Do not plot beyond the length of the input
      bar3d(c(gap+x[j],x[j]+scalexy),
            c(-gap-y[i],-y[i]-scalexy),
            z[it],
            alpha=alpha,
            topcol=topcolors[it],
            sidecol=sidecolors[it],
            linecol=linecolors[it])
    }
  }
  ## Set the viewpoint
  rgl::rgl.viewpoint(theta=theta,phi=phi,fov=0)

  ## If desired, add grid lines, axes labels and axes subtitles
  if(gridlines){
    rgl::grid3d("y",col="black",at=((1:cols)+0.4)*scalexy+gap) # this is the floor and matches the "cols" parameter
    rgl::grid3d("x",col="black") # this is the wall and matches the z vertical direction
    rgl::grid3d("z",col="black") # this is the wall and matches the z vertical direction
    rgl::grid3d("y",col="black",at=(((1:rows)+0.4)*scalexy+gap)*-1) # this is the floor and matches the "rows" parameter
  }
  if(xlabels[1]!=FALSE){
    rgl::axis3d("x-+", labels=xlabels,at=((1:cols)+0.4)*scalexy+gap) # this is the floor and matches the "cols" parameter
  }
  if(ylabels[1]!=FALSE){
    rgl::axis3d("z+", labels=ylabels,at=-1*((1:rows)+0.4)*scalexy-gap) # this is the floor and matches the "rows" parameter
  }
  if(zlabels){
    rgl::axis3d("y-+", labels=TRUE) # vertical numeric scale
  }
  if(xsub!=FALSE){
    rgl::mtext3d(xsub,edge="x-+",line=2)
  }
  if(ysub!=FALSE){
    rgl::mtext3d(ysub,edge="z+",line=2)
  }
  if(zsub!=FALSE){
    rgl::mtext3d(zsub,edge="y-+",line=2)
  }

  invisible()
}

