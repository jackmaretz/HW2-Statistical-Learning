library(wrassp) 
#library(help = wrassp)
library(tuneR)
#Read the .au file 

# read.AsspDataObj ------------------------------------------------------------------------

temp = list.files(pattern ="*.au")

create_empty_table <- function(num_rows, num_cols) {
  frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
  return(frame)
}


for (i in 1:length(temp)) {
  x = read.AsspDataObj(temp[i])
  y = Wave(as.numeric(x$audio), samp.rate = rate.AsspDataObj(x), bit = 16)
  assign(temp[i], y)
  }


# what to do with every au ------------------------------------------------


str(f1.au)
f1 = f1.au@left 
f1.dwn = downsample(f1.au,samp.rate = 11025)


# Feature extraction ------------------------------------------------------

suppressPackageStartupMessages( require(signal, quietly = TRUE) )

# setup of short time fourier transform -----------------------------------
fs = f1.dwn@samp.rate
winsize <- 2048
hopsize <- 512
nfft <- 2048
noverlap <- winsize - hopsize
#go
sp <-  specgram(x=f1.dwn@left,n=nfft,Fs=fs, window = winsize,overlap = noverlap)

#take a look
class(sp)
names(sp)
#sp
dim(sp$S)

