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
 }

#' Function to get and output workspace authors as MEDFORD
#' @param wsns the workspace namespace of the workspace
#' @param ws the workspace name of the workspace
#' @return MEDFORD text
#' @export
docOwners<-function(wsns,ws){
  require(AnVIL)
  terra=Terra()
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

 #' Function to output AnVIL data MEDFORD
 #' @return MEDFORD text
 #' @export
 anvilMedfordData<-function(docText,minorTag=NA){
       if(!is.na(minorTag)){
      print(paste0("@Data-", minorTag," ",docText))
    }else{print(paste0("@Data ",docText))}
  }


#' Function to get and output workspace creator as MEDFORD
#' @param wsns the workspace namespace of the workspace
#' @param ws the workspace name of the workspace
#' @return MEDFORD text
#' @export
docCreatedBy<-function(wsns,ws){
  require(AnVIL)
  terra=Terra()
  res=terra$getWorkspace(wsns,ws)
  resList=as.list(res)
  anvilMedfordContributor(resList$workspace$createdBy,type="creator")
}


#' Function to document main workspace details
#' @return MEDFORD text
#' @export
docWorkspace<-function(wsns,ws){
  require(AnVIL)
  terra=Terra()
  res=terra$getWorkspace(wsns,ws)
  resList=as.list(res)
  anvilMedfordWorkspace(ws)
  anvilMedfordWorkspace(ws,"workspace")
  anvilMedfordWorkspace(wsns,"namespace")
  anvilMedfordWorkspace(resList$workspace$billingAccount,"billingAccount")
  anvilMedfordWorkspace(resList$workspace$bucketName,"bucketName")
  anvilMedfordWorkspace(resList$workspace$createdBy,"createdBy")
  anvilMedfordWorkspace(resList$workspace$createdDate,"createdDate")
  anvilMedfordWorkspace(resList$workspace$lastModified,"lastModified")
}


#' Function to document  workspace Data details
#' @return MEDFORD text
#' @export
docWorkspaceData<-function(wsns,ws){
  require(AnVIL)
  terra=Terra()
  res=terra$getWorkspace(wsns,ws)
  resList=as.list(res)
  atts=resList$workspace$attributes
  for(i in 1:length(atts)){
    if(length(atts[[i]])==1){
      anvilMedfordData(names(atts)[i])
      anvilMedfordData("WorkspaceData","Role")
      anvilMedfordData(resList$workspace$attributes[[i]],"Value")
    }
  }
}

#' Function to document workspace Entity details
#' @return MEDFORD text
#' @export
docWorkspaceEntities<-function(wsns,ws){
  require(AnVIL)
  terra=Terra()
  temp=avtables(wsns,ws)
  for(i in 1:nrow(temp)){
      anvilMedfordData(temp[i,1])
      anvilMedfordData("Table","Role")
      anvilMedfordData(temp[i,1],"Value")
  }
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
  require(AnVIL)
  terra=Terra()
  temp=strsplit(dataFile,split="/")[[1]]
  temp=temp[length(temp)]
  obName=strsplit(temp,split=".",fixed=TRUE)
  obName=obName[[1]][1]
  dat=read.csv(unz(zipFile,dataFile))
  avtable_import(dat,entity=obName,name=ws,namespace=wsns)
}
