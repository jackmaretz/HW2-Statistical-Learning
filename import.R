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

# Feature extraction ------------------------------------------------------

suppressPackageStartupMessages( require(signal, quietly = TRUE) )

# setup of short time fourier transform -----------------------------------

f1 = f1.au@left 
