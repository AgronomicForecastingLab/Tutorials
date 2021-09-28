line_breaker <- function(title="title"){
  HTML(paste0(
    '<div style="width: 100%; height: 20px; border-bottom: 2px solid black; text-align: center">
      <span style="font-size: 25px; background-color: #FFFFFF; padding: 0 10px;"> ', title,' <!--Padding is optional-->
      </span>
      </div>'
  ))
}
# New_Camp_Maker(filename="Campaign.nc4",
#                lenght.x.y=5,
#                box.camp=c(-91, -90, 40, 41),
#                scen.len=8
#                )


#' New_Camp_Maker
#'
#' @param filename Character output file name
#' @param lenght.x.y double number of pixel in each direction - North/South - East/West
#' @param box.camp numeric with 4 elements. West, East, South and North
#' @param scen.len numeric number of scenarios
#'
#' @return
#' @export

New_Camp_Maker <- function(filename="Campaign.nc4",
                           lenght.x.y=5,
                           box.camp=c(-91, -90, 40, 41),
                           scen.len=12
) {
  
  tryCatch({
    #set the coordinates
    xvals <- seq(box.camp[1], box.camp[2], length.out = lenght.x.y) #longitude
    yvals <- seq(box.camp[3], box.camp[4], length.out = lenght.x.y) #latitude
    #
    nx <- length(xvals)
    ny <- length(yvals)
    
    lon1 <- ncdf4::ncdim_def("lon", "degrees_east", xvals)
    lat2 <- ncdf4::ncdim_def("lat", "degrees_north", yvals)
    
    time <- ncdf4::ncdim_def("scen","scenario", seq_len(scen.len), unlim=TRUE)
    mv <- -999 #missing value to use
    var_temp <- ncdf4::ncvar_def("test_var_temp", "celsius", list(lon1, lat2, time), longname="some random value", mv) 
    
    ncnew <- ncdf4::nc_create(filename, list(var_temp))
    
    # Some fake dataset based on latitude, to check whether the data are
    # written in the correct order
    data <- rep(yvals, each=nx)
    
    ncdf4::ncvar_put(ncnew, var_temp, data, start = c(1, 1, 1), count = c(nx, ny, 1))
    
    # Don't forget to close the file
    ncdf4::nc_close(ncnew)
  },error = function(e) {
    toastr_error(title = "Error reading var names", conditionMessage(e))
  })
}


#rotating a matrix
mirror.matrix<- function (x) {
  xx <- as.data.frame(x)
  xx <- rev(xx)
  xx <- as.matrix(xx)
  xx
}

# gets a raster - does a simple interpolation - No change in the raster size
my_interpolate <- function(prec) {

  tryCatch({
    ## taken from the interpolate help page
    xy <- data.frame(xyFromCell(prec, 1:ncell(prec)))
    v <- getValues(prec)
    tps <- fields::Tps(xy, v)
    p <- raster(prec)
    # use model to predict values at all locations
    p <- interpolate(p, tps)
    #
    return(p)
  },error = function(e) {
    toastr_error(title = "Error reading var names", conditionMessage(e))
  })
}

alert<-function(type='primary',
                title='Oh snap!',
                msg='msg'){
  HTML(paste0('
<div class="alert alert-dismissible alert-', type ,'">
  <button type="button" class="close" data-dismiss="alert"></button>
  <strong>', title, '</strong> <br> ',msg,'
</div>
     ')
       )
}


#' Edit_Campaign
#'
#' @param CampaignFile Input Campaign file
#' @param lat a vector of latitudes
#' @param lon a vector of longitudes
#' @param var a vector of variables to be changed
#' @param scen.id a vector of scenario ids
#' @param val a vector of values for replacment
#' @param OutFile a charcter string representing the output file name
#' @description https://github.com/grimbough/rhdf5
#'
#' @return

#' @export
#'
#' @examples
#' 
# Edit_Campaign(CampaignFile="Campaign.nc4",
#               lat=c(89.25,89.25),
#               lon=c(-179.25,-178.25),
#               scen.id=rep(1,2),
#               var=rep('pfday',2),
#               val=c(123,1234),
#               OutFile="testnew2.nc4")

Edit_Campaign <- function(CampaignFile="Campaign.nc4",
                          lat=40,
                          lon=-80,
                          var='pfday',
                          scen.id=4,
                          val=2,
                          OutFile="Campaign2.nc4") {
  
  #Getting the original path 
  original.dir <- dirname(CampaignFile)
  #temp name
  new.name <- paste0(sample(letters, 5) %>%
                       paste(collapse = ""), ".nc4")
  #temp path
  file.path.tmp <- file.path(tempdir(), new.name)
  
  if(file.exists( file.path(original.dir, OutFile))) file.remove( file.path(original.dir, OutFile))
  # make a temp copy of the file
  file.copy(
    from = CampaignFile,
    to = file.path.tmp
  )
  #open the file
  fid <- rhdf5::H5Fopen(file.path.tmp)
  
  
  purrr::pmap(list(lat, lon, var, scen.id, val),
              function(lat.v, lon.v, var.v, scen.id.v, val.v){
                # find lat long id
                lat.id <- which.min(abs(fid$'lat'-lat.v))
                lon.id <- which.min(abs(fid$'lon'-lon.v))
                
                # fid$'feamn_1'[lat.id,lon.vec]<- 987
                #fid$'pfday'[1, 1, scen.id]<- 987
                
                if (val.v=="") return(NULL)
                #Get the dimensions
                dims.var <- eval(expr = parse(text = paste0('dim(fid$', var.v, ')')))
                #browser()
                # replacing the values for arrays with scenario and without
                if(length(dims.var) == 3) {
                  if (is.null(scen.id))
                    stop('Your variable has 3 dimensions and scenario id is not provided')
                  eval(expr = parse(
                    text = paste0('fid$', var.v, '[', lat.id, ',', lon.id, ',', scen.id.v, ']<-', val.v)
                  ))
                } else if (length(dims.var) == 2) {
                  eval(expr = parse(text = paste0(
                    'fid$', var.v, '[', lat.id, ',', lon.id, ']<-', val.v
                  )))
                } else if (length(dims.var) == 1) {
                  eval(expr = parse(text = paste0(
                    'fid$', var.v, '[', scen.id, ']<-', val.v
                  )))
                }
              })
  
  
  
  # flusing and closing the file
  rhdf5::H5Fflush(fid)
  rhdf5::H5Fclose(fid)
  
  
  file.copy(
    from = file.path.tmp,
    to = file.path(original.dir, OutFile)
  )
  
  return(TRUE)
  
}
