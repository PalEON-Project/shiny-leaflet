##Opening Net cdf files
#need 32-bit R
#First I had to download .zip file with ncdf4 package to my computer
# the ncdf4 package and instructions can be found: http://lukemiller.org/index.php/2012/01/ncdf4-r-package-binaries/ and downloaded here : http://cirrus.ucsd.edu/~pierce/ncdf/
#then load package from zip file
#load ncdf4 library

#create nc object
nc<-nc_open("C:/Users/Kelly/Documents/Research_plans/MIP/CMIP5/landCoverFrac_Lmon_HadGEM2-CC_historical_r1i1p1_185912-188411.nc")

#how many variables does it have?

print(paste("The file has", nc$nvars,"variables"))


#this prints the data for variable 1, here it is the lat band
v1 <- nc$var[[1]]
data1 <- ncvar_get( nc, v1 )  # by default, reads ALL the data
print(paste("Data for var ",v1$name,":",sep=""))
print(data1)

# This shows how the shape of the read data is preserved
v2 <- nc$var[[2]]
data2 <- ncvar_get( nc, v2 )
print(paste("Var 2 has name",v2$name,"and is of shape",dim(data2),
            ". Here are the values:"))
print(data2)

# This illustrates how to read data one timestep at a time.  In this
# example we will elaborately show how to deal with a variable whose
# shape is completely unknown (i.e., how many dimensions, and what their
# sizes are).  We will also, for illustration of a common case, show how
# to read in the values of the time dimension at each timestep.
v3      <- nc$var[[3]]
varsize <- v3$varsize
ndims   <- v3$ndims
nt      <- varsize[ndims]  # Remember timelike dim is always the LAST dimension!
for( i in 1:nt ) {
  # Initialize start and count to read one timestep of the variable.
  start <- rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] <- i	# change to start=(1,1,1,...,i) to read timestep i
  count <- varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] <- 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  data3 <- ncvar_get( nc, v3, start=start, count=count )
  # Now read in the value of the timelike dimension
  timeval <- ncvar_get( nc, v3$dim[[ndims]]$name, start=i, count=1 )
  
  print(paste("Data for variable",v3$name,"at timestep",i,
              " (time value=",timeval,v3$dim[[ndims]]$units,"):"))
  print(data3)
}
data4<-ncvar_get(nc, v3)
#vegetation 
v4  <- nc$var[[4]]
v4$name
 v4$varsize
v4$ndims
data5 <- ncvar_get( nc, v4)  # by default, reads ALL the data
print(paste("Data for var ",v4$name,":",sep=""))
print(data5)
v5<-nc$var[[5]]

z1 = ncvar_get( nc, varid="Veg", start=c(1,1,1), count=c( 5,5,5))
# compare to 


z2 = ncvar_get( nc, "Veg", start=c(11,1,1), count=c( 5,5,5))
# compare to 
z[11:15,]
v5<-nc$var[[5]]
data5<-ncvar_get(nc, v5)
  