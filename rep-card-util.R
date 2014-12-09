## some utility functions to read data files from
## Illinois State Board of Education report card data found in various
## files
## http://www.isbe.net/assessment/report_card.htm
## An excel spreadsheet showing the layout of the data was converted
## to text by hand
## the big data files were not saved in the git repo but the layout was

## read the layout
data.dir <- "data2014/"
get.data.dir <- function() {
  data.dir
}

layout.fn <- "rc14-layout-2014.txt"
layout.fn <- paste(data.dir, layout.fn, sep="")
layout.d <- read.table(layout.fn, header=F, sep="\t", comment.char="")
colnames(layout.d) <- c("field.num","fac1","demo1",
                        "col.range", "col.width",
                        "field.name", "field.fmt",
                        "col.start", "col.end")

layout.d$field.num <- as.numeric( layout.d$field.num)

## what file
file.lookup.fn <- "file-lookup.txt"
file.lookup.fn <- paste(data.dir, file.lookup.fn, sep="")
file.lookup.d <- read.table( file.lookup.fn, sep=",")

field.2.file <- function(fl.d,
                         layout.d,
                         field="LOW-INCOME SCHOOL %",
                         fac1=NULL) {

  if( ! is.null( fac1)) { 
    layout.d <- layout.d[ as.character(layout.d$fac1)== fac1,]
  }
  
  split1 <- strsplit(as.character(fl.d[,4]),"-")
  last.col <- as.numeric(lapply( split1, function(x) as.numeric(x[2])))
  first.col <- as.numeric(lapply( split1, function(x) strsplit(x[1]," ")[[1]][3]))

  where <- grep( field, as.character(layout.d$field.name))
  
  field.num <- layout.d[where,"field.num"]
  which.file <- sum(last.col < field.num) + 1
  
  list(file=as.character(fl.d[which.file, 1]),
       field.num=field.num,
       first.col=first.col[which.file],
       last.col=last.col[which.file])
}

col.num <- function( f) {
  f$field.num - f$first.col + 2 - (f$first.col == 1)
}

fld.name.2.df <- function(fld.name, colname,
                          file.look.d=file.lookup.d,
                          lay.d=layout.d,
                          fac1=NULL) {
  
  # low.income.str <- "LOW-INCOME SCHOOL %"
  fld.file <- field.2.file( file.look.d, lay.d, fld.name,fac1=fac1)
  fn <- paste( get.data.dir(), fld.file$file, sep="")
  another.fld.d <- read.table( fn, sep=";", header=F, comment.char="")
  another.fld.d <- another.fld.d[,c(1,col.num( fld.file))]
  colnames(another.fld.d) <- c("id", colname)
  list(df=another.fld.d,
       str= fld.name,
       f=fld.file,
       fac1=fac1)
}

make.data.layout <-  function( data.dir, layout.fn) {
  ## read the layout
  ## data.dir <- "datadir2014/"
  ## layout.fn <- "rc14-layout-2014.txt"
  layout.fn <- paste(data.dir, layout.fn, sep="")
  layout.d <- read.table(layout.fn, header=F, sep="\t", comment.char="")
  colnames(layout.d) <- c("field.num","fac1","demo1",
                          "col.range", "col.width",
                          "field.name", "field.fmt",
                          "col.start", "col.end")
  layout.d$field.num <- as.numeric( layout.d$field.num)
  layout.d
}

file.column.lookup <- function( file.lookup.fn) {
  ## file.lookup.fn <- "file-lookup.txt"
  file.lookup.fn <- paste(data.dir, file.lookup.fn, sep="")
  file.lookup.d <- read.table( file.lookup.fn, sep=",")
  file.lookup.d
}

trim <- function (x){
  gsub("^\\s+|\\s+$", "", x)
}

kp <- function() {
  for( i in dev.list()) dev.off(i)
}
