#' Adds a single 3D bar to the current scene
#'
#' @param x The x dimensions of the bar, a vector of length 2 e.g. c(0,1).
#' @param y The y dimensions of the bar, a vector of length 2 e.g. c(0,1).
#' @param z The height of the bar, a single number, e.g 3.
#' @param alpha The alpha channel (transparency) of the sides of the bar. Range 0-1.
#' @param topcol The color of the top of the bar. Text description or hexadecimal RGB color, like that returned by rgb() e.g. "red" or "#078E53"
#' @param sidecol The color of the sides of the bar. Text description or a hexadecimal RGB color, like that returned by rgb() e.g. "gray" or "#aaaaaa"
#' @param linecol The color of the edges of the bar. Text description or be a hexadecimal RGB color, like that returned by rgb() e.g. "black" or "#000000"
#' @return Nothing is returned (invisibly returns NULL).
#' @examples
#' \dontrun{
#' bar3d(c(0,1),c(0,1),3,alpha=0.6,topcol="#078E53",sidecol="#aaaaaa",linecol="#000000")
#' }
#' @export
bar3d<-function(x=c(0,1),y=c(0,1),z,alpha=1,topcol="#078E53",sidecol="#aaaaaa",linecol="#000000"){

  ## These lines allow the active rgl device to be updated with multiple changes
  ## This is necessary to draw the sides and ends of the column separately
  save <- rgl::par3d(skipRedraw=TRUE)
  on.exit(rgl::par3d(save))

  ## Determine the coordinates of each surface of the column and its edges
  x1=c(rep(c(x[1],x[2],x[2],x[1]),3),rep(x[1],4),rep(x[2],4))
  z1=c(rep(0,4),rep(c(0,0,z,z),4))
  y1=c(y[1],y[1],y[2],y[2],rep(y[1],4),rep(y[2],4),rep(c(y[1],y[2],y[2],y[1]),2))
  x2=c(rep(c(x[1],x[1],x[2],x[2]),2),rep(c(x[1],x[2],rep(x[1],3),rep(x[2],3)),2))
  z2=c(rep(c(0,z),4),rep(0,8),rep(z,8) )
  y2=c(rep(y[1],4),rep(y[2],4),rep(c(rep(y[1],3),rep(y[2],3),y[1],y[2]),2) )

  ## These lines create the sides of the column and its colored top surface
  rgl::rgl.quads(x1,z1,y1,col=rep(sidecol,each=4),alpha=alpha,lit=FALSE)
  rgl::rgl.quads(c(x[1],x[2],x[2],x[1]),rep(z,4),c(y[1],y[1],y[2],y[2]),
            col=rep(topcol,each=4),alpha=1,lit=FALSE)
  ## Add colored edges to the column
  rgl::rgl.lines(x2,z2,y2,col=linecol,lit=FALSE)
  invisible()
}
