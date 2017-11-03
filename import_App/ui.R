####################################################################
# All packages required for running the program
options(warn = -1, message = F)
library(shiny)
library(shinyjs)
library(RODBC)
library(RMySQL)
library(shinyBS)
library(uuid)
library(XLConnect)
library(jsonlite)
library(rhandsontable)

###########################################################################
#
#                           SOURCE R-SCRIPTS
#
###########################################################################
# Source the R-Scripts placed in the App
dirR <- file.path(".", "www", "R")
pathnames <- list.files(pattern="[.]R$", path=dirR, full.names=TRUE)
sapply(pathnames, FUN=source)

#############################################################################
# SET TIMEZONE
Sys.setenv(TZ = "UTC")

################################################################################
# SET LOCAL SETTINGS
if (file.exists("../localSettings.rda")){
  load("../localSettings.rda")
  language <<- localSettings$language
  metService <- localSettings$metService
}else{
  language <<- "english"
  metService <- "other"
}

################################################################################
# SET LOCAL LANGUAGE
if (language == "portuguese"){
  Sys.setlocale("LC_COLLATE", "Portuguese_Portugal.1252")
  language_abbr <- "pt"
  documentation <- "Ajuda"
}else if (language == "german"){
  Sys.setlocale("LC_COLLATE", "German_Germany.1252")
  language_abbr <- "en"
  documentation <- "Help"
}else{
  Sys.setlocale("LC_COLLATE", "English_United Kingdom.1252")
  language_abbr <- "en"
  documentation <- "Help"
} 

################################################################################
# GET TEXT
textInfo <- get_language(language)
for (i1 in c(1:length(textInfo))){
  assign(names(textInfo[i1]),textInfo[[i1]], envir = .GlobalEnv)
}


################################################################################
# SET MET SERVICE

if (tolower(metService) == "inamet"){
  theme <- "inamet.css"
  logo <- "logoINAMET.png"
  logoSize <- "50%"
  dbase_choices <- sort(c("", 
                          textDbChoices01,  
                          textDbChoices02,
                          textDbChoices03))
}else if (tolower(metService) == "zmd"){
  theme <- "sasscal.css"
  logo <- "logoZMD.png"
  logoSize <- "50%"
  dbase_choices <- sort(c("", 
                          textDbChoices01,
                          textDbChoices03,
                          textDbChoices04)
                        )
}else if (tolower(metService) == "dms"){
  theme <- "sasscal.css"
  logo <- "DMS.png"
  logoSize <- "50%"
  dbase_choices <- sort(c("",
                          textDbChoices01,  
                          textDbChoices03))
}else{
  theme <- "sasscal.css"
  logo <- "logoNULL.png"
  logoSize <- "30%"
  dbase_choices <- sort(c("",
                          textDbChoices01,
                          textDbChoices02,
                          textDbChoices03,
                          textDbChoices04))
}

#############################################################################
# CREATE DOCUMENTATION
dirDoc <<- file.path(".", "www", "documentation", language_abbr)
rmarkdown::render(file.path(dirDoc, "documentation.Rmd"), encoding = 'UTF-8')

# Copy the rmarkdowns
newFolder <- file.path("..","docs", language_abbr)
dir.create(newFolder, showWarnings = T, recursive = T)
files_from <- grep(list.files(dirDoc), pattern = ".Rmd$", inv = T, value = T)
files_to <- file.path(newFolder, files_from)
file.copy(file.path(dirDoc,files_from), newFolder, recursive = F)
#############################################################################
# INTERFACE
shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    theme = file.path("bootstrap",theme),
    titlePanel(title = NULL,windowTitle = textTitlePanel),
    fluidRow(
      column(11),
      column(1, a(documentation, target="_blank", 
                  href=file.path("documentation",language_abbr, 
                                 "documentation.html")))
    ),
    fluidRow(
      column(2, div(img(src=file.path("images", logo), width = logoSize, 
                        height = logoSize),
                    style = "text-align: left;")),
      column(8, h1(textTitlePanel, align = "center")),
      column(2, div(img(src=file.path("images","logoSASSCAL.jpg"), 
                        width = "65%", height = "65%"),
                    style = "text-align: right;"))
    ),
    HTML('<hr>'),
    sidebarLayout(
      ####################################################################
      # SIDEBAR PANEL
      sidebarPanel(id="sidebar",
                   # Select database
                   selectInput("dbase",
                               label = h3(textSidePanel00),
                               choices = dbase_choices,
                               selected = ""
                   ),
                   # Other inputs
                   uiOutput("UIsSidePanel"),
                   # Action buttons
                   uiOutput("UIsActionButtons")
      ),
      
      ####################################################################
      mainPanel(
        uiOutput("UIsMainPanel")
      )
    )
  )
)
