# this file provides two functions: zTreeTables and zTreeSbj
                                        # author: Oliver Kirchkamp http://www.kirchkamp.de/
# 2015-04-16: handles non-z-Tree files more gracefully
# 2014-10-26: added remark regarding locale (thanks to Igor Asanov for the hint).
# 2013-05-14: added "zTree.silent" option
# 2013-05-10: made code robust against non-existent tables, caught (predictable) warnings
# 2012-12-19: corrected merging of sbj files
# 2012-11-23: made merging of tables faster (at the cost of needing the plyr library)
# 2012-03-19: fixed encoding to Latin1 (this becomes an issue once chats are saved in tables)
# 2011-03-21: fixed problem where the first table was ignored in zTreeSbj (thanks to David Hugh-Jones for the hint)
#----------------------------------------
# zTreeTables is an R function to read zTree .xls files
# The return value is a list of dataframes, one for each table
#
# Example:
#
# source("http://www.kirchkamp.de/lab/zTree.R")
# options(zTree.silent=TRUE) # <- less chatty
#
# read data from one session:
# zTT <- zTreeTables ( "090602_1746.xls" , "contracts" )
#
# read data from several sessions:
# zTT <- zTreeTables ( c("090602_1746.xls","090602_1912.xls"), c("globals","subjects", "contracts" ))
#
# read data from all sessions in directories below the current one:
# sessions<-list.files(".","[0-9]{6}_[0-9]{4}.xls",recursive=TRUE)
# zTT<-zTreeTables(sessions,c("globals","subjects","session","summary","contracts"))
#
# use the data:
# plot(Profit ~ Period,data=zTT$subjects)
#
# Remarks:
# As far as I understand z-Tree uses Latin1 encoding. You might need to set your locale
# to something related, like the following:
# > Sys.setlocale("LC_CTYPE","en_GB.UTF-8")
#

require(plyr)

options(zTree.silent=FALSE)

zTreeTables <- function(filelist,tables=c("globals","subjects"),sep = "\t") {
  splittable <- function(filename,tables=c("globals","subjects")) {
    getTable <- function(start, stop) {
      if (!is.na(stop) && !is.na(start)) {
        names<-aa2[[start]][-3]
        names[1]<-"Date"
        names[2]<-"Treatment"
        tab<-as.data.frame(matrix(nrow=stop-start-1,ncol=length(names)))
        colnames(tab)<-names
        for( i in 1:(stop-start-1)) {
          tab[i,] <- aa2[[start+i]][-3]
        }
        for (n in colnames(tab)) {
          if (is.character(tab[[n]])) {
            tab[[n]][tab[[n]]=="-"] <- NA
            mm<-tryCatch(mean(as.numeric(tab[[n]]),na.rm=TRUE),warning=function(x) NA)
            if (!is.na(mm)) {
              tab[[n]]<-as.numeric(tab[[n]])
            }
          }
        }
        tab
      }
    }
    
    getTables <- function(name) {
      tab<-NULL
      for (i in which ((splitname==name))) {
          new<-getTable(splitpoints[i],splitpoints[i+1])
          fail<-names(new)==""
          if (sum(fail)>0) warning(sprintf("*** %s contains empty cells. This is not a z-Tree file ***",name))
          new<-new[,!fail]
          if (length(new)>0) {
              if (is.null(tab)) {
                  tab<-new
              } else {
                  tab <- rbind.fill(tab,new)
              }
          }
      }
      tab
    }
    if(!getOption("zTree.silent")) cat("reading ",filename,"...\n")
    Tfile<-file(filename,"r",encoding="LATIN1")
    aa<-readLines(Tfile)
    close(Tfile)
    aa2<-strsplit(aa,sep)
    if(length(aa2[[1]])<3) 
        stop(sprintf("*** cells are not separated by '%s'. Proper z-Tree files use \\t as a separator. Use the \"sep\" option! ***",ifelse(sep=="\t","\\t",sep)))
    splitpoints<-array()
    splitname<-array()
    splittreat<-array()
    table(splitname)
    splitcols<-array()
    last<-0
    for (i in 1:length(aa2)) {
      if (last==0 || (aa2[[i]][3] != aa2[[i-1]][3])) {
        last<-last+1
        splitpoints[last]<-i
        splitname[last]<-aa2[[i]][3]
        splittreat[last]<-aa2[[i]][2]
        splitcols[last]<-length(aa2[[i]])
      }
      splitpoints[last+1]<-i+1
    }
    # cat(splitpoints)
    result<-list()
    do <- intersect(splitname,tables)
    miss <- setdiff(splitname,tables)
                                        #if (length(miss)>0)
    if(!getOption("zTree.silent")) cat ("Skipping:",miss,"\n")
    for (name in do) {
      if(!getOption("zTree.silent")) cat ("Doing:",name,"\n")
      aTable<-getTables(name)
      if (!is.null(aTable)) result[[name]]<-aTable
    }
    result
  }
  
  z<-splittable(filelist[1],tables)
  for (name in filelist[-1]) {
    if(!getOption("zTree.silent")) cat (sprintf("*** %s is file %d / %d ***\n",name,which(name==filelist),length(filelist)))
    a<-splittable(name,tables)
    for(t in tables) {
      if (!is.null(a[[t]])) # there is no such table
        z[[t]]<-rbind.fill(z[[t]],a[[t]])
    }
  }
  # try to convert characters to numbers if this does not introduce more NAs:
  for (t in tables)
    for(n in names(z[[t]]))
      if(typeof(z[[t]][[n]])=="character") 
        if(!is.null(q<-tryCatch(as.numeric(z[[t]][[n]]),warning=function(x) NULL))) z[[t]][[n]]<-q
  z
}
#
# zTreeSbj takes a vector of .sbj-files and returns a matrix
# Example:
# files <- list.files(pattern = "*.sbj$",recursive=TRUE)
# fname <- sub(".*/","",files)
# sbj <- zTreeSbj(aggregate(files,list(fname),function(x) x[1])$x)
#
zTreeSbj <- function(files,sep="\t") {
  sbj<-NULL
  for (filename in files) {
    if(!getOption("zTree.silent")) cat("reading ",filename,"...\n")
    Tfile<-file(filename,"r",encoding="LATIN1")
    aa<-readLines(Tfile)
    close(Tfile)
    aa2<-strsplit(aa,sep)
    N <- length(aa2[[2]])-1
    aa3<-as.data.frame(list(Date=rep(sub(".sbj$","",sub(".*/","",filename)),N)))
    lapply(aa2,function(x) if (length(x)==N+1) aa3[[x[1]]]<<- x[-1])
    if ( is.null(sbj) ) sbj<-aa3 else sbj<-merge(sbj,aa3,all=TRUE)
  }
  sbj
}
#
toLongDate <- function (shortDate) {
  sapply(as.character(shortDate),function(zz) {
    pre <- ifelse(substr(zz,1,2)<"80","20","19")
    if (nchar(zz)==8) {
#      hour  <- which(LETTERS==substr(zz,7,7))-1
      minute<- 60*which(LETTERS==substr(zz,7,7)) + (which(c(as.character(0:9),LETTERS)==substr(zz,8,8)))*2 - 21
      sprintf("%s%s-%02d:%02d",pre,substr(zz,1,6),minute%/%60,minute%%60)
    }
    else if (nchar(zz)==11) sprintf("%s%s-%s:%s",pre,substr(zz,1,6),substr(zz,8,9),substr(zz,10,11))
    else zz
  })
}
