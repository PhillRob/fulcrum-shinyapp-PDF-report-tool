# this is for the CM website
library(httr)
library(jsonlite)
library(shiny)
library(data.table)
library(dplyr)
library(sysfonts)
#font_import()
options(stringsAsFactors = FALSE)

baseurl  <- "https://api.fulcrumapp.com"
path <- "/api/v2/"
form <- "records.json?form_id="
formid <- "2d4b228d-aa6f-44df-85c0-ec02b6896a13"
XApiToken = "f5b3e34036628ad4acf99b8e31b31ef21a4962ccfd3433bb7cfa339a3ee15d3ab9c5ddd0aa701aef"

# function to load data
## how to check if there is new data? simply looking at the number of records won't work. 
loadData <- function(){
    req <-
        GET(
            url = paste0(baseurl, path, form,formid),
            add_headers(`X-ApiToken` = XApiToken),
            accept_json()
        )
    r.json <- fromJSON(content(req, type = "text", encoding = "utf-8"),flatten = T,simplifyVector=T)
    r.json.r <- as.data.frame(r.json$records)

    #Issue
    r.json.r$issue <-
        ifelse(
            test = (r.json.r$form_values.94e9.choice_values=="character(0)"),
            r.json.r$form_values.94e9.other_values,
            r.json.r$form_values.94e9.choice_values
        )
    r.json.r$issue<-gsub("NULL", NA, r.json.r$issue)
    r.json.r$issue<-iconv(r.json.r$issue, "latin1", "ASCII", sub="")
    r.json.r$issue<-gsub("[^a-zA-Z0-9]"," ", r.json.r$issue)
    r.json.r$issue<-gsub("c  ","", r.json.r$issue)
    r.json.r$issue<-gsub("       ",", ", r.json.r$issue)
    r.json.r$issue<-gsub("^\\s+|\\s+$", "",r.json.r$issue)
    
    #action
    r.json.r$action <-
        ifelse(
            test = (r.json.r$form_values.1594.choice_values=="list()"),
            r.json.r$form_values.1594.other_values,
            r.json.r$form_values.1594.choice_values
        )
    r.json.r$action<-gsub("NULL", NA, r.json.r$action)
    #status
    r.json.r$status<-iconv(r.json.r$status, "latin1", "ASCII", sub="")
    r.json.r$status<-gsub(" -","",r.json.r$status)
    
    #image
    r.json.r$photo_id <- lapply(lapply(lapply(r.json.r$form_values.ddc2, `$.data.frame`, "photo_id"),FUN = unlist),`[[`, 1)
    r.json.r$photos_url<-paste0("![", r.json.r$issue, "](https://web.fulcrumapp.com/shares/6fe8ca9a9a3e74d8/photos/",r.json.r$photo_id,"/thumbnail){ width=4.2cm, height=4.2cm }")
    
    #km
    r.json.r$km<-as.numeric(paste0(r.json.r$form_values.6970,".",r.json.r$form_values.0f86.choice_values))
   
    # r.json.r$img<- paste0('![', r.json.r$issue, '](', r.json.r$img, '.png){ width=90% }')
    # r.json.r$i<-gsub(pattern = "view?photos=",replacement = "",r.json.r$i,fixed =T)
    
    # r.json.r$ma<-lapply(
    # lapply(r.json.r$form_values.3f98, `$.data.frame`, "form_values.6b72.choice_values"),FUN = unlist)
    ##TODO: gsugub the brackets away
    # r.json.r$ma<-gsub("NULL", NA, r.json.r$ma)
    # r.json.r$ma<-gsub("NA", NA, r.json.r$ma)
    # r.json.r$ma<-gsub(1, "Action Required" , r.json.r$ma)
    # r.json.r$ma<-gsub(2, "No Action" , r.json.r$ma)
    # r.json.r$ma<-gsub(3, "Re-inspection" , r.json.r$ma)
    # r.json.r$ma<-gsub(4, "Request for approval" , r.json.r$ma)
    r.json.r<-r.json.r[,c("form_values.1551","km","status","issue","action","form_values.52b7","form_values.74e8.choice_values","created_at","updated_at","photos_url")]
    colnames(r.json.r)<-c("ID","KM","Status","Issue","Action","Comments", "Responsible","Created","Updated","photos_url")
    return(r.json.r)
}
