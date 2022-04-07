MosaicRasters<-function(RasterLocation, CellFunction){ 
  
  # make list of file names that are rasters. Note to change the pattern argument if rasters are not .tif. Alternatively you can remove the pattern argument if the file location only stores relevant raster files to be mosaiced. 
  current.list <- list.files(path=paste(RasterLocation), full.names=TRUE, pattern = ".tif") 
  
  # read in raster files as a raster stack
  raster.list<- lapply(current.list, raster::stack) 
  
  # clear user defined names if present
  names(raster.list) <- NULL 
  
  # this tells the mosaic function to average any overlapping pixels
  raster.list$fun <- CellFunction
  
  # mosaic list using the mosaic function the raster package
  Mosaic <- do.call( raster::mosaic, raster.list)
  
}


ExtractRaster<-function(Mosaic,UOA,CellFunction){ 
  
  # match coordinate system of the mosaiced raster
  Mosaic<-projectRaster(Mosaic, crs = crs(UOA))
  
  # extract (join) raster values to points or polygons. 
  ex <- extract(Mosaic, 
                UOA, 
                fun=CellFunction, # e.g. take the mean, max etc. raster values
                na.rm=TRUE,
                df=TRUE)
  
  ex<-cbind(UOA,ex) #append extracted values to unit of analysis
  
  return(ex)
}


# Mosaic and Extract in one step
MosaicExtractRaster<-function(RasterLocation,UOA){ 
  
  # make list of file names that are rasters. Note to change the pattern argument if rasters are not .tif. Alternatively you can remove the pattern argument if the file location only stores relevant raster files to be mosaiced. 
  current.list <- list.files(path=paste(RasterLocation), full.names=TRUE, pattern = ".tif") 
  
  # read in raster files as a raster stack
  raster.list<- lapply(current.list, raster::stack) 
  
  # clear user defined names if present
  names(raster.list) <- NULL 
  
  # this tells the mosaic function to average any overlapping pixels
  raster.list$fun <- mean 
  
  # mosaic list using the mosaic function the raster package
  Mosaic <- do.call( raster::mosaic, raster.list)
  
  # ensure joining units have a coordinate system
  UOA<-st_transform(UOA, crs = 3857)
  
  # match coordinate system of the mosaiced raster
  Mosaic<-projectRaster(Mosaic, crs = crs(UOA))
  
  # extract (join) raster values to points or polygons. 
  ex <- extract(Mosaic, 
                UOA, 
                fun=mean, #take the mean raster values (change to sum or other function if desired)
                na.rm=TRUE,
                df=TRUE)
  
  ex<-cbind(UOA,ex)
  
  return(ex)
}