#' A wrapper function to create a sequence context "legoplot"
#' @param contextdata A numeric vector of counts or frequencies of the 96 possible somatic mutations and trinucleotide contexts. These MUST be in the same order as in the example (see example and/or vignette).
#' @param alpha The alpha channel (transparency) of the sides of 3D bars. Range 0-1.
#' @param scalexy Scaling factor for x and y coordinates; this constant can be used to make the plot "skinnier" or "fatter".
#' @param gap Gap between 3D bars.
#' @param sixcolors The color scheme. "broad" for Broad Institute colors, "sanger" for Sanger Institute colors or a vector of six hexadecimal RGB colors.
#' @param theta Polar coordinate for viewing the 3D barplot; range 0 to 360 (rotates the plot).
#' @param phi Polar coordinate for viewing the 3D barplot; range -90 to 90 (-90 is directly below, 90 directly above).
#' @param gridlines Draw gridlines on the plot (TRUE or FALSE).
#' @param labels Include the default axis labels (TRUE or FALSE).
#' @param zlabels Labels for the z axis; add numeric scale to the vertical dimension of the plot (TRUE or FALSE).
#' @param zsub Descriptive label for the z axis.
#' @return Nothing is returned (invisibly returns NULL).
#' @examples
#' \dontshow{
#' options(rgl.useNULL=TRUE)
#' }
#' # Read in COSMIC signature probabilities
#' x=system.file("extdata", "signature_probabilities.txt", package = "barplot3d")
#' sigdata=read.table(x,header=TRUE,stringsAsFactors = FALSE)
#' # Plot signature 2 with Sanger colors and some transparency so we can see all bars
#' legoplot3d(contextdata=sigdata$Signature_2,labels=TRUE,scalexy=0.05,sixcolors="sanger",
#' alpha=0.4,zsub="Probability")
#' @export
legoplot3d<-function(contextdata,alpha=1,scalexy=1,gap=0.2,sixcolors="broad",
                    theta=50,phi=40,gridlines=TRUE,labels=FALSE,zlabels=TRUE,zsub=FALSE){

  ## Setup the 6 color regions
  set1=c(1:4,17:20,5:8,21:24,9:12,25:28,13:16,29:32)
  set2=set1+32
  set3=set1+64
  neworder=c(set1,set2,set3)
  if(sixcolors[1]=="broad"){
    sixcolors=c("#805D3F","#72549A","#5EAFB2","#3F4F9D","#F2EC3C","#74B655")
  }
  if(sixcolors[1]=="sanger"){
    sixcolors=c("#050708","#CBCACB","#1EBFF0","#EDC8C5","#E62725","#A1CF64")
  }
  mycolors=as.vector(sapply(sixcolors,rep,16))[neworder]

  ## Set label variables
  if(labels){
    xlabels=rep(c("A","C","G","T"),2)
    ylabels=rep(c("A","C","G","T"),3)
    xsub="Preceeding base"
    ysub="Succeeding base"
  }else{
    xlabels=FALSE
    ylabels=FALSE
    xsub=""
    ysub=""
  }

  ## Reorder input data to plotting order
  porder=order(c(33,34,35,36,41,42,43,44,49,50,51,52,57,58,59,60,1,2,3,4,9,10,11,12,17,18,19,20,25,26,27,28,65,66,67,68,73,74,75,76,81,82,83,84,89,90,91,92,5,6,7,8,13,14,15,16,21,22,23,24,29,30,31,32,69,70,71,72,77,78,79,80,85,86,87,88,93,94,95,96,37,38,39,40,45,46,47,48,53,54,55,56,61,62,63,64))

  barplot3d(rows=12,cols=8,z=contextdata[porder],alpha=alpha,scalexy=scalexy,gap=gap,topcolors=mycolors,sidecolors=mycolors,
                      theta=50,phi=40,gridlines=TRUE,xlabels=xlabels,ylabels=ylabels,zlabels=zlabels,xsub=xsub,ysub=ysub,zsub=zsub)
}
