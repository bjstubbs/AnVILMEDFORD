###############################
#AnVILMEDFORD - main file
#Version: .1
#
#
###############################

#' Function to output AnVIL owners in MEDFORD
#' @param docText text to convert to MEDFORD
#' @return MEDFORD text
#' @export
anvilMedfordContributor<-function(docText,type="owner"){
     print(paste("@Contributor", docText))
     print(paste("@Contributor-Role", type))
     print("")
 }

#' Function to get and output workspace authors as MEDFORD
#' @param wsns the workspace namespace of the workspace
#' @param ws the workspace name of the workspace
#' @return MEDFORD text
#' @export
docOwners<-function(wsns,ws){
  res=terra$getWorkspace(wsns,ws)
  resList=as.list(res)
  for(owner in resList$owners){
    anvilMedfordContributor(owner)
  }
}


#' Function to output AnVIL workspace MEDFORD
#' @return MEDFORD text
#' @export
anvilMedfordWorkspace<-function(docText,minorTag=NA){
      if(!is.na(minorTag)){
     print(paste0("@Workspace-", minorTag," ",docText))
   }else{print(paste0("@Workspace ",docText))}
 }

#' Function to get and output workspace creator as MEDFORD
#' @param wsns the workspace namespace of the workspace
#' @param ws the workspace name of the workspace
#' @return MEDFORD text
#' @export
docCreatedBy<-function(wsns,ws){
  res=terra$getWorkspace(wsns,ws)
  resList=as.list(res)
  anvilMedfordContributor(resList$workspace$createdBy,type="creator")
}


#' Function to document main workspace details
#' @return MEDFORD text
#' @export
docWorkspace<-function(wsns,ws){
  res=terra$getWorkspace(wsns,ws)
  resList=as.list(res)
  anvilMedfordWorkspace(ws)
  anvilMedfordWorkspace(ws,"workspace")
  anvilMedfordWorkspace(wsns,"namespace")
  anvilMedfordWorkspace(resList$workspace$billingAccount,"billingAccount")
}

#' Function to Extract csv file paths from medford bagit
#' @param zipFile zip file from BAGIT
#' @return a vector of filepaths
bagItavTables<-function(zipFile){
  flist=unzip(zipFile,list=TRUE)
  flist=grep(flist[,1],pattern="avtabs/\\w",value=TRUE,perl=TRUE)
  flist
}

#' Function to upload csv from BAGIT to AnVIL
#' @param dataFile csv filename from BAGIT
#' @return Boolean success
uploadBagitAvtable<-function(wsns,ws,zipFile,dataFile){
  temp=strsplit(dataFile,split="/")[[1]]
  temp=temp[length(temp)]
  obName=strsplit(temp,split=".",fixed=TRUE)
  obName=obName[[1]][1]
  dat=read.csv(unz(zipFile,dataFile))
  avtable_import(dat,entity=obName,name=ws,namespace=wsns)
}

#' Function to get broad ids from perturbagen name
#' @param pertName the name of the perturbagen
#' @param sigInfo sig file data
#' @param phase which phase of data are we using
#' @return a vector of broad ids
#' @export
