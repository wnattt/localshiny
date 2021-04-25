## ------------------------------------------------------------------
##    DEPLOY
## ------------------------------------------------------------------
deployApp <- function(username,
                      project = getwd(),
                      version = NULL,
                      name = NULL,
                      description = NULL,
                      os   = NULL) {
  
  if (!isStringParam(username))
    stop(stringParamErrorMessage("username"))  
  
  if (!isStringParam(project))
    stop(stringParamErrorMessage("project"))
  
  if(isDocumentPath(project)){
    deployDoc(username, 
              project, 
              version = version, 
              name = name, 
              description=description, 
              os=os )
  }else{
    deployDir(username, 
              project, 
              version = version, 
              name = name, 
              description=description, 
              os=os)
  }
  
}


deployDoc <- function(username, document, ... ){
  
  oldWD <- getwd()
  on.exit(setwd(oldWD), add = TRUE)
  setwd(dirname(document)) 
  
  appDir <- tempdir()
  on.exit(unlink(appDir, recursive=TRUE))
  file.copy(document, appDir) 
  
  name <- if(!is.null(name)) name else gsub(".R", "", document)
  
  deployDir(username,
            project = appDir,
            name = name,
            script = document, ...) 
  
}

deployDir <- function(username,
                      project,
                      version = NULL,
                      name = NULL,
                      description = NULL,
                      os   = NULL,
                      script = getOption("runShiny")){
  
  # normalize project path and ensure it contains files
  project <- normalizePath(project, mustWork = FALSE)
  if (!file.exists(project)) {
    stop(project, "contains no files")
  }   
  
  # get the path to the config file
  configDir   <- authConfigDir(username)
  configFile  <- file.path(configDir , "config.dcf")
  
  # read login information for username
  if(!file.exists(configFile)){
    stop("Unable to determine the login information for the account named '", username, "'!
          Please use loginAccount() function to log in first!")
  }
  configAuthInfo <- configuration(username, configFile)
  
  # create connect client
  serverInfo <- setServer()     
  client     <- connectClient(serverInfo$url)
  
  # if we snapshot a shiny application, write a script to run it
  if(identical(script, getOption("runShiny"))){
    script <- file.path(".", getOption("runShiny"))
    writeLines("library(shiny);runApp()", script)
    on.exit(unlink(script))
  }
  
  # zip up the project 
  appZipFile <- zipAppDeploy(project)
  on.exit(unlink(appZipFile))
  
  # get app information
  appInfo    <- infoAppDeploy(project, script, name, description, version, os)
  
  # upload app .tar file
  headers   <- list('Cookie'=configAuthInfo$session)
  uploadRequest  <- client$uploadApp(appInfo, appZipFile, headers)
  message(paste("***: ", jsonlite::fromJSON(uploadRequest$content)$description, sep=""))
  
  invisible(uploadRequest)
  
}

zipAppDeploy <- function(appDir){
  
  oldWD <- getwd()
  on.exit(setwd(oldWD), add = TRUE)
  setwd(appDir)  
  
  #capture the state of a project's R package dependencies and create a lockfile, "renv.lock".
  #The lockfile can be used to later restore these project's dependencies as required.
  renv::settings$package.dependency.fields(c("Imports", "Depends", "LinkingTo", "Suggests"))
  renv::snapshot(project = appDir, prompt = FALSE)

  # remove renv setting file
  if(dir.exists("renv")){
    unlink("renv", recursive=TRUE)
  }  

  # check snapshot results
  if(!file.exists("renv.lock")){
    stop("This project has not yet been snapshotted. Lockfile does not exist!")
  }

  # archive all files under the current directory
  appZipFile <- paste(basename(appDir), "zip", sep=".")
  zip::zipr(appZipFile, ".")
  
  normalizePath(appZipFile)
}

infoAppDeploy <- function(appDir, appRunScript, appName = NULL, appDesc=NULL, appVersion=NULL, appOs = NULL){
  
  pakname <- if(!is.null(appName)) appName else basename(appDir)
  version <- if(!is.null(appVersion)) appVersion else "1.0"
  pakdesc <- if(!is.null(appDesc)) appDesc 
              else paste( "This is an application named '", pakname, "' !",sep = "")
  os      <- if(!is.null(appOs)) appOs  
              else  paste(Sys.info()['sysname'], Sys.info()['release'], sep='')
  rversion <- paste(R.Version()['major'], R.Version()['minor'], sep='.')       
  
  list( pakname = pakname,
        version = version, 
        pakdesc = pakdesc, 
        os      = os,
        rversion = rversion,
        runcmd  = appRunScript )
}
