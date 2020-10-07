
### this is testing current git version control


#==========================================================================================
#
#
#                                Complaints Typing App
#                                Last update: SEP 2020
#                               NOTE: updated type 6 RMC & TW8.7; TW8.3 columns
#                                    version: 1.4
#
#==========================================================================================

#------------------------------------------------------------------------------------------
# Notes: Download function has to work in an external browser 
#------------------------------------------------------------------------------------------
#install.packages("shiny",type="binary")
#install.packages("readxl")
#install.packages("plyr")
#install.packages("DT")
#install.packages("writexl")
library(plyr)
library(shiny)
library(readxl)
library(DT)
library(writexl)

options(shiny.maxRequestSize = 30*1024^2)  #change uploading file's size
#------------------------------------------------------------------------------------------
#
#                                    Define UI 
#
#------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Complaint Typing"),
  
  tabsetPanel(
    
    
    tabPanel("Input/Output files", #tab title
             
             sidebarPanel(width=4,
                          helpText("Upload Input Files:"),
                          fileInput("records","Choose Records for Typing Export txt",multiple = FALSE,
                                    accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                          fileInput("lot","Choose Product Info Grid txt",multiple = FALSE,
                                    accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                          fileInput("tw8.3","Choose TW8.3 Ref txt",multiple = FALSE,
                                    accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                          fileInput("tw8.7t","Choose TW8.7 Table Ref txt",multiple = FALSE,
                                    accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                          fileInput("tw8.7g","Choose TW8.7 Grid Ref txt",multiple = FALSE,
                                    accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                          fileInput("otc","Choose OTC List",multiple = FALSE,
                                    accept = c(".xlsx")),
                          fileInput("CMCL","Choose Critical_MC_list",multiple = FALSE,
                                    accept = c(".xlsx")),
                          fileInput("exclusion","Choose CCC Exclusion List",multiple = FALSE,
                                    accept = c(".xlsx")),
                          fileInput("bags","Choose List of Bags",multiple = FALSE,
                                    accept = c(".xlsx")),
                          selectInput("Year","Choose Query Year", choices = c("2020","2021","2022","2023","2024")),
                          selectInput("Month","Choose Query Month", choices = c("Jan","Feb","Mar","Apr","May","Jun",
                                                                                   "Jul","Aug","Sep","Oct","Nov","Dec")),
                          selectInput("Day","Choose Query Day",choices = c("01","02","03","04","05","06","07","08","09","10",
                                                                              "11","12","13","14","15","16","17","18","19","20",
                                                                              "21","22","23","24","25","26","27","28","29","30","31")),
                          helpText("Download Typing Result:"), 
                          p("(download file name format:"),
                          p("typing_DDMMMYYYY.xlsx)"),
                          downloadButton("Typing", "Download")
                          #helpText("Download Type 6 Result:"), 
                          #downloadButton("Type6", "Download"),
                          #radioButtons("filetype", "File type:",choices = c(".csv", ".txt"))
             
             ),
             
             mainPanel(img(src="ConvaTec.png",height=120,width=400),
                       p("================================================================================"),
                       p("Copyright belong to ConvaTec Metrics Central Team", style = "color:blue"),
                       p("Developed & Supported by Yaxin Zheng on behalf Metric Central Team"),
                       p("R Shiny App Updated Date: September 2020"),
                       p("Version 1.4"),
                       span(textOutput("lib.day2"),style = "color:green"),
                       p("================================================================================"),
                       dataTableOutput("result")
             )
    ),
    
    tabPanel("TrackWise", 
             mainPanel( 
               h3("TrackWise Library",align = "left"),
               span(textOutput("lib.day1"),style = "color:green"),
               dataTableOutput("TW")

  )
    
    )
  )
)

#--------------------------------------------------------------------------------------------------
#
#                                     Define Server 
#
#--------------------------------------------------------------------------------------------------

  
  server <- function(input, output) {
    
#------------------------------------ Function to extract MC ---------------------------------------    
    # function to exract malfunction.code [NOTICE] AIR -... has a space behind it...
    f<-function(s)strsplit(as.character(s)," ")[[1]][1] # give value before 1st blank
    t<-function(s)strsplit(as.character(s)," ")[[1]][2] # give value between 1st and 2nd blanks  

#------------------------------------ Data cleaning ------------------------------------------------
    TW8.3<-reactive(
      {if(is.null(input$tw8.3))return(NULL)
        tw8.3<- read.csv(input$tw8.3$datapath, header = TRUE)
        # 
        tw8.3$Product.Lot.Number. = trimws(tw8.3$Product.Lot.Number., which="both")
        tw8.3$Serial.Number = ""
        colnames(tw8.3) = c("PR.ID","Date.Created","Business.Unit..Franchise.",
                            "Severity.Rating","Lot.Number","Malfunction.Code","PR.State","Serial.Number","Part.Number")
         tw8.3$lib.indicator = "8.3"
         tw8.3
      }
    )
    
    TW8.7 <- reactive(
      {if(is.null(input$tw8.7t) | is.null(input$tw8.7g))return(NULL)

         tw8.7t = read.table(input$tw8.7t$datapath, header = TRUE,sep = ",",quote = "\"")
         tw8.7g = read.table(input$tw8.7g$datapath, header = TRUE,sep = ",",quote = "\"")
         tw8.7g <- subset(tw8.7g, select = - c(Event.Part.Number))
         tw8.7g$Lot.Number = trimws(tw8.7g$Lot.Number, which="both")
         colnames(tw8.7g)[1]<-"PR.ID"
         colnames(tw8.7t)[1]<-"PR.ID"
         tw8.7 = merge(tw8.7t,tw8.7g,by="PR.ID")
         tw8.7.s = subset(tw8.7,select = c("PR.ID","Date.Created","Business.Unit..Franchise.",
                                           "Severity.Rating","Lot.Number","Malfunction.Code","PR.State","Serial.Number","Part.Number"))
         tw8.7.s$lib.indicator<-"8.7"
         tw8.7.s
      }
    )
    
    TW <- reactive(
      {if(is.null(input$tw8.7t) | is.null(input$tw8.7g) |is.null(input$tw8.3))return(NULL)
      tw<- rbind(TW8.3(),TW8.7())
      tw <- subset(tw,!grepl("CNO",tw$Lot.Number))
      tw <- subset(tw,!grepl("cno",tw$Lot.Number))
      tw <- subset(tw,PR.State!="Closed - Canceled" & PR.State!="Closed - Cancelled") #delete closed-canceled records in tw
      tw$MC = sapply(tw$Malfunction.Code, f)
      tw$MC = ifelse(tw$MC == "AIR",paste(tw$MC,tw$AIR<-sapply(tw$Malfunction.Code, t)),tw$MC)
      tw$Lot.Number<-toupper(tw[,"Lot.Number"])
      tw<-subset(tw ,select = c("PR.ID","Date.Created","Business.Unit..Franchise.",
                                        "Severity.Rating","Lot.Number","Malfunction.Code","PR.State","Serial.Number","Part.Number","lib.indicator","MC"))
      tw
      }
    )
    
    REC <- reactive({
      if(is.null(input$records))return(NULL)
      records = read.table(input$records$datapath, header = TRUE,sep = ",",quote = "\"",comment.char = "", fill = T)
      colnames(records)[1] = "PR.ID"
      records
    })
    
    LOT <- reactive(
      {if(is.null(input$lot))return(NULL)
        lot = read.table(input$lot$datapath, header = TRUE,sep = ",",quote = "\"")
        colnames(lot)[1]<-"PR.ID"
        lot <- subset(lot, select = - c(Event.Part.Number))
        lot$Lot.Number<-toupper(lot[,"Lot.Number"])
        lot$Lot.Number = trimws(lot$Lot.Number, which="both")
        lot
      }
    )
    
    ToT1 <- reactive({if(is.null(input$records)|is.null(input$lot))return(NULL)
      total1 <- merge(REC(),LOT(),by = "PR.ID")
      total1 <- subset(total1, select = c( "PR.ID"                   , "Date.Opened"               , "Part.Number"        ,  "Product.Name"              
                                           ,"Short.Description"      , "Manufacturing.Site."       , "Severity.Rating"    ,  "Malfunction.Code"          
                                           ,"Additional.Rationale"   , "Business.Unit..Franchise." , "PR.State"           ,  "Reported.Harm"             
                                           ,"Reported.Harm.Code"     , "Medical.Review.Approval.By", "Describe.Event.of.Problem" , "Cause.Analysis"            
                                           ,"Grid.ID"                , "Quantity.Nonconforming"    , "Lot.Number"         ,  "Serial.Number"             
                                           ,"Lot.Batch.Number"   ))
      
      total1$MC = sapply(total1$Malfunction.Code, f) #truncate MC 
      total1$MC = ifelse(total1$MC == "AIR",paste(total1$MC,total1$AIR <- sapply(total1$Malfunction.Code, t)),total1$MC) # replace AIR with AIR -PMCxxx
      
      if (dim(REC())[1]==dim(LOT())[1]){
        print("equal # PR.ID")
      }else{
        print("Stop! Total# of PR.ID is different in records & Grid files ") 
      }
      #total1[duplicated(total1$PR.ID),]
      validate(need(dim(REC())[1]==dim(LOT())[1],"WARNING: Total# of PR.ID is different in records & Grid files"))
      
      #if (dim(REC())[1]!=dim(LOT())[1]) quit(save="ask")
      
      if (length(unique(total1$PR.ID))==dim(total1)[1]){
        print("Great,no duplication!")
      }else{
        print("duplicated PR ID identified, please double check") 

      }
      total1.dup<-total1[duplicated(total1$PR.ID),]
      
      validate(need(length(unique(total1$PR.ID))==dim(total1)[1],"WARNING: duplicated PR ID identified, please double check your records & Grid files"))
      #if (length(unique(total1$PR.ID))!=dim(total1)[1]) quit(save="ask")
      
      total1
      
      }) # read and combine record files and clean data
    
    # total1 <- reactive({if(is.null(input$records)|is.null(input$lot))return(NULL)
    #   records = read.table(input$records$datapath, header = TRUE,sep = ",",quote = "\"")
    #   lot = read.table(input$lot$datapath, header = TRUE,sep = ",",quote = "\"")
    #   colnames(records)[1] = "PR.ID"
    #   colnames(lot)[1]<-"PR.ID"
    #   lot$Lot.Number<-toupper(lot[,4])
    #   lot$Lot.Number = trimws(lot$Lot.Number, which="both")
    #   total1 = merge(records,lot,by="PR.ID")
    #   total1$MC = sapply(total1$Malfunction.Code, f) #truncate MC 
    #   total1$MC = ifelse(total1$MC == "AIR",paste(total1$MC,total1$AIR <- sapply(total1$Malfunction.Code, t)),total1$MC) # replace AIR with AIR -PMCxxx
    #   total1}) # read and combine record files and clean data
    # 
    ccc_exc_NOdup <- reactive({if(is.null(input$exclusion)|is.null(input$bags))return(NULL)
      ccc_exc = read_excel(paste(input$exclusion$datapath), 1)
      bags = read_excel(paste(input$bags$datapath), 1)
      ccc_exc.new = merge(ccc_exc,bags,by.x ="Part Number",by.y = "Part Number", all = T )
      ccc_exc.new = ccc_exc.new[!is.na(ccc_exc.new$`Part Number`),]
      ccc_exc_NOdup = ccc_exc.new[order(ccc_exc.new$`Part Number`),]
      ccc_exc_NOdup = ccc_exc_NOdup[!duplicated(ccc_exc_NOdup$`Part Number`),]  
      ccc_exc_NOdup}) # read exclusion files         
    
    #=================================================================================================
    #                                  Phase 1 Typing Assignment
    #=================================================================================================
    
    #------------------------------------------- Type 6 ----------------------------------------------
    
    total1_type6 <- reactive({if(is.null(ToT1())|is.null(input$otc))return(NULL)
      total1 = ToT1()
      otc = read_excel(paste(input$otc$datapath), 1)
      total1$typing = ifelse((total1$Part.Number %in% as.character(otc$part) | (
        (!(total1$Lot.Number %in%"CNO"|total1$Lot.Number%in%"cno") & !(total1$Lot.Number%in%"PENDING") &  
           !(total1$Lot.Number%in%"") & !(total1$Lot.Number%in%"n/a") & !(total1$Lot.Number%in%NA) ) &
          (total1$Manufacturing.Site.%in%"Greensboro 3rd party" | total1$Manufacturing.Site.%in%"Deeside - 3rd Party" | 
             total1$Manufacturing.Site.%in%"Deeside 3rd party")
      )),6,NA)                                     
      total1$comment[total1$Part.Number%in%"421551"] = "part number is 421551, need to check serial number individually"
      total1})
    
    #------------------------------------------- Type 1 ----------------------------------------------
    
    total1_type1 <- reactive({if(is.null(total1_type6())|is.null(input$otc))return(NULL)
      total1 = total1_type6()
      otc = read_excel(paste(input$otc$datapath), 1)
      total1$typing[(total1$Lot.Number%in%"CNO"|total1$Lot.Number%in%"cno"| total1$Lot.Number%in%"cNo" | total1$Lot.Number%in%"cNO"| total1$Lot.Number%in%"cnO"
                     | total1$Lot.Number%in%"CNo" | total1$Lot.Number%in%"CnO" | total1$Lot.Number%in%"CNO." | total1$Lot.Number%in%"cno.") & 
                      !(total1$Lot.Number%in%otc$part)  & !(total1$typing%in%6)] = 1
      total1})
    #-------------------------------------------------------------------------------------------------
    #                                           BU = OST or WT
    #------------------------------------------------------------------------------------------------- 
    
    #------------------------------------------- Type 2 ----------------------------------------------
    
    total1_type2 <- reactive({if(is.null(total1_type1())|is.null(input$CMCL))return(NULL)
      total1 = total1_type1()
      CMCL = read_excel(paste(input$CMCL$datapath), 1)
      total1$typing[(total1$Business.Unit..Franchise.=="Ostomy" | 
                       total1$Business.Unit..Franchise.=="Wound Therapeutics") & 
                      (!(total1$Lot.Number %in%"CNO"|total1$Lot.Number%in%"cno") & !(total1$Lot.Number%in%"PENDING") &  
                         !(total1$Lot.Number%in%"") & !(total1$Lot.Number%in%"n/a") & !(total1$Lot.Number%in%NA) ) &
                      (total1$Severity.Rating==4 |total1$Severity.Rating==5 | 
                         total1$MC%in%CMCL$ostomy_MC )  &
                      (!(total1$typing%in%6) & !(total1$typing%in%1)) ] = 2
      total1})
    
    #----------------------------------------- Add Frequence (RMC) ----------------------------------
    
    Newdata <- reactive({if(is.null(total1_type2())|is.null(TW()))return(NULL)
      total1 = total1_type2()
      TW = TW()
      Newdata = merge(total1,TW,by.x=c("Lot.Number","MC"), by.y=c("Lot.Number","MC"))
      Newdata})
    
    total1_freq <- reactive({if(is.null(total1_type2())|is.null(Newdata()))return(NULL)
      total1 = total1_type2()
      Newdata = Newdata()
      counts = ddply(Newdata, .(Newdata$Lot.Number,Newdata$MC,Newdata$PR.ID.x), nrow)
      names(counts) = c("Lot.Number", "MC","PR.ID","FreqMC")
      total1 = merge(total1,counts,by.x=c("Lot.Number","MC","PR.ID"), by.y=c("Lot.Number","MC","PR.ID"),all.x = T)
      total1})
    
    #------------------------------------------- Type 3 ----------------------------------------------         
    
    total1_type3 <- reactive({if(is.null(total1_freq())|is.null(input$CMCL))return(NULL)
      total1 = total1_freq()
      CMCL = read_excel(paste(input$CMCL$datapath), 1)
      total1$typing[(total1$Business.Unit..Franchise.=="Ostomy" | 
                       total1$Business.Unit..Franchise.=="Wound Therapeutics") & 
                      (!(total1$Lot.Number %in%"CNO"|total1$Lot.Number%in%"cno") & !(total1$Lot.Number%in%"PENDING") &  
                         !(total1$Lot.Number%in%"") & !(total1$Lot.Number%in%"n/a") & !(total1$Lot.Number%in%NA) ) &
                      (total1$Severity.Rating==1 |total1$Severity.Rating==2 | total1$Severity.Rating==3)
                    & (total1$FreqMC<3) & !(total1$MC%in%CMCL$ostomy_MC) &
                      !(total1$typing%in%6) & !(total1$typing%in%1) & !(total1$typing%in%2)
                    & !is.na(total1$MC)
                    & !is.na(total1$Severity.Rating) ] = 3
      total1})
    
    #------------------------------------------- Type 4 ---------------------------------------------- 
    
    total1_type4 <- reactive({if(is.null(total1_type3())|is.null(input$CMCL))return(NULL)
      total1 = total1_type3()
      CMCL = read_excel(paste(input$CMCL$datapath), 1) 
      total1$typing[(total1$Business.Unit..Franchise.=="Ostomy" | 
                       total1$Business.Unit..Franchise.=="Wound Therapeutics") & 
                      (!(total1$Lot.Number %in%"CNO"|total1$Lot.Number%in%"cno") & !(total1$Lot.Number%in%"PENDING") &  
                         !(total1$Lot.Number%in%"") & !(total1$Lot.Number%in%"n/a") & !(total1$Lot.Number%in%NA) ) &
                      (total1$Severity.Rating==1 |total1$Severity.Rating==2 | total1$Severity.Rating==3)
                    & !(total1$MC%in%CMCL$ostomy_MC) &
                      (total1$FreqMC>=3) &
                      !(total1$typing%in%6) & !(total1$typing%in%1) & !(total1$typing%in%2) & !(total1$typing%in%3)
                    & !is.na(total1$MC)
                    & !is.na(total1$Severity.Rating)] = 4
      
      total1})
    
    #-------------------------------------------------------------------------------------------------
    #                                       BU = C or CC
    #------------------------------------------------------------------------------------------------- 
    
    #------------------------------------------- Type 2 ----------------------------------------------
    
    total1_type2c <- reactive({if(is.null(total1_type4())|is.null(ccc_exc_NOdup()))return(NULL)
      total1 = total1_type4()
      total1$typing[(total1$Business.Unit..Franchise.=="Continence & Critical Care") & 
                      !(total1$Part.Number%in%ccc_exc_NOdup()$`Part Number`)
                    &(!(total1$typing%in%6) & !(total1$typing%in%1) & !(total1$typing%in%2) & 
                        !(total1$typing%in%3) & !(total1$typing%in%4))] = 2    # case 1: part # not in ccc_exclusion list
      total1$typing[(total1$Business.Unit..Franchise.=="Continence & Critical Care") & 
                      (total1$Part.Number%in%ccc_exc_NOdup()$`Part Number`) &
                      (!(total1$Lot.Number %in%"CNO"|total1$Lot.Number%in%"cno") & !(total1$Lot.Number%in%"PENDING") &  
                         !(total1$Lot.Number%in%"") & !(total1$Lot.Number%in%"n/a") & !(total1$Lot.Number%in%NA) ) & 
                      (!(total1$typing%in%6) & !(total1$typing%in%1) & !(total1$typing%in%2) & 
                         !(total1$typing%in%3) & !(total1$typing%in%4)) &
                      (total1$Severity.Rating==4 |total1$Severity.Rating==5) ] = 2 # case 2: in exclusion list
      total1}) 
    
    #------------------------------------------- Type 3 ----------------------------------------------
    
    total1_type3c <- reactive({if(is.null(total1_type2c())|is.null(ccc_exc_NOdup()))return(NULL)
      total1 = total1_type2c()
      total1$typing[(total1$Business.Unit..Franchise.=="Continence & Critical Care") & 
                      (total1$Part.Number%in%ccc_exc_NOdup()$`Part Number`) &
                      (!(total1$Lot.Number %in%"CNO"|total1$Lot.Number%in%"cno") & !(total1$Lot.Number%in%"PENDING") &  
                         !(total1$Lot.Number%in%"") & !(total1$Lot.Number%in%"n/a") & !(total1$Lot.Number%in%NA) )  &
                      (total1$Severity.Rating==1 |total1$Severity.Rating==2 | total1$Severity.Rating==3)
                    & (total1$FreqMC<2)
                    &(!(total1$typing%in%6) & !(total1$typing%in%1) & !(total1$typing%in%2) & 
                        !(total1$typing%in%3) & !(total1$typing%in%4))
                    & !is.na(total1$MC)
                    & !is.na(total1$Severity.Rating)] = 3
      total1})
    
    #------------------------------------------- Type 4 ----------------------------------------------
    
    total1_type4c <- reactive({if(is.null(total1_type3c())|is.null(ccc_exc_NOdup()))return(NULL)
      total1 = total1_type3c()
      total1$typing[(total1$Business.Unit..Franchise.=="Continence & Critical Care") & 
                      (total1$Part.Number%in%ccc_exc_NOdup()$`Part Number`) &
                      (!(total1$Lot.Number %in%"CNO"|total1$Lot.Number%in%"cno") & !(total1$Lot.Number%in%"PENDING") &  
                         !(total1$Lot.Number%in%"") & !(total1$Lot.Number%in%"n/a") & !(total1$Lot.Number%in%NA) )  &
                      (total1$Severity.Rating==1 |total1$Severity.Rating==2 | total1$Severity.Rating==3)
                    & (total1$FreqMC>=2)
                    &(!(total1$typing%in%6) & !(total1$typing%in%1) & !(total1$typing%in%2) & 
                        !(total1$typing%in%3) & !(total1$typing%in%4))
                    & !is.na(total1$MC)
                    & !is.na(total1$Severity.Rating)] = 4
      total1})
    
    #------------------------------------------- Type 0 ----------------------------------------------
    
    total1_type0 <- reactive({if(is.null(total1_type4c()))return(NULL)
      total1 = total1_type4c()
      total1$typing[is.na(total1$typing)] = 0
      total1$typing <- ifelse((total1$typing==3 | total1$typing==4 ) & is.na(total1$Severity.Rating),0,total1$typing)
      total1$typing <- ifelse((total1$typing==3 | total1$typing==4 ) & is.na(total1$MC),0,total1$typing)
      total1$typing <- ifelse((total1$typing==3 | total1$typing==4 ) & is.na(total1$Business.Unit..Franchise.),0,total1$typing)
      total1})
    
    #=================================================================================================
    #                                 Phase 2 Observation Evidence
    #=================================================================================================  
    
    total1_phase2 <- reactive({if(is.null(total1_type0()))return(NULL)
      total1 = total1_type0()
      total1$RMC = ifelse((total1$typing%in%3|total1$typing%in%4|total1$typing%in%6),total1$FreqMC,NA)
      total1$QD8.7 = paste("TW8.7",input$Day, input$Month, input$Year,sep = "-")
      total1$QD8.3 = paste("TW8.3","10","MAY","2019",sep = "-")
      total1$statement = as.character(NA)
      total1})
    
    new.sub <- reactive({if(is.null(Newdata()))return(NULL)
      new.sub = subset(Newdata(),select = c(Lot.Number,MC,PR.ID.x,PR.ID.y,lib.indicator,Serial.Number.y))
      new.sub})
    
    #--------------------------------------- Re-typing 3 ---------------------------------------------
    #( added on June 2018)
    #(modified on Mar 2019)
    
    total1_retyping3 <- reactive({if(is.null(total1_phase2())|is.null(new.sub()))return(NULL)
      total1 = total1_phase2()
      new.sub = new.sub()
      total1.type3 = total1[total1$typing==3,]
      total1.other = total1[!total1$typing==3,]
      re_typing = total1[which(total1$Serial.Number=="Management Override"| total1$Serial.Number=="CHU Management Override"),]
      i = match(re_typing$PR.ID.x,total1.type3$PR.ID)
      total1.type3$typing[i] = 2
      total1.type3$statement[i] = "Management Override"
      total1 = rbind(total1.other,total1.type3)
      total1})
    
    #--------------------------------------------------------------------------------------------------    
    
    total1_phase2.new <- reactive({if(is.null(total1_retyping3())|is.null(new.sub()))return(NULL)
      total1 = total1_retyping3()
      new.sub = new.sub()
      total1$ref8.7 = NA
      total1$ref8.3 = NA
      total1 <- total1[order(total1$PR.ID),]
      new.sub <- new.sub[order(new.sub$PR.ID.x),]
      
      
      for (i in 1:length(total1$typing)){
        if (total1$typing[i]==3|total1$typing[i]==4|total1$typing[i]==6){
          temp.chr8.7=""
          for (j in 1: length(new.sub$PR.ID.x)){
            if (new.sub$PR.ID.x[j]==total1$PR.ID[i]&new.sub$lib.indicator[j]=="8.7"){
              temp.chr8.7=paste(temp.chr8.7,new.sub$PR.ID.y[j], sep = ",")
            } 
          }
          total1$ref8.7[i]= temp.chr8.7
          
          temp.chr8.3=""
          for (k in 1: length(new.sub$PR.ID.x)){
            if (new.sub$PR.ID.x[k]==total1$PR.ID[i]&new.sub$lib.indicator[k]=="8.3"){
              temp.chr8.3=paste(temp.chr8.3,new.sub$PR.ID.y[k], sep = ",")
            }
          }
          total1$ref8.3[i]= temp.chr8.3
        }
      }
      
      total1$ref8.7 = ifelse((total1$typing%in%3|total1$typing%in%4|total1$typing%in%6),total1$ref8.7,NA)
      total1$ref8.3 = ifelse((total1$typing%in%3|total1$typing%in%4|total1$typing%in%6),total1$ref8.3,NA)
      total1})
    
    #=================================================================================================
    #                                       Phase 3 Statement
    #================================================================================================= 
    
    total1_phase3 <- reactive({if(is.null(total1_phase2.new()))return(NULL)
      total1 = total1_phase2.new()
      
      for (i in 1: length(total1$typing)){
        if (total1$typing[i]==3){
          total1$statement[i]=paste("This complaint has been evaluated as a Type 3 case. A query was run on ",
                                    input$Day," ", input$Month," ", input$Year, " against Lot number ",total1$Lot.Number[i]," for malfunction code ",
                                    total1$MC[i]," which yielded ",total1$RMC[i]," occurrences against the lot.A detailed investigation or batch review is not required as the complaint is low severity and
                                    the occurrence of complaints with this malfunction and specific batch does not meet the batch limit criteria per WI-0358 version 7.0. If additional complaints occur with this batch and same malfunction code,
                                    these subsequent complaints shall be assessed against the batch limit criteria per WI-0358 version 7.0.
                                    This issue will be monitored through the Post Market Product Monitoring Review Process, SOP-000741.",sep="")
        }
        } 
      total1 = subset(total1,select = c("PR.ID","Date.Opened","Part.Number","Product.Name",
                                        "Manufacturing.Site.","Severity.Rating","Malfunction.Code",
                                        "Business.Unit..Franchise.","Reported.Harm","Reported.Harm.Code",
                                        "Medical.Review.Approval.By","Lot.Number","Serial.Number","typing","RMC","ref8.7","ref8.3","QD8.7","QD8.3","comment","statement"))
      total1})
    
    
    
    
    
    
    
#------------------------------------------ Output -----------------------------------------------            
    
    output$result <- renderDataTable(total1_phase3())
  
    output$TW <- renderDataTable(TW())
    
    output$Typing <- downloadHandler(filename = function() {paste("typing_", input$Day,input$Month, input$Year,".xlsx",input$filetype, sep = "")},
                                     content = function(file) 
                                       {
                                       #sep = switch(input$filetype, ".csv" = ",", ".txt" = "\t")
                                       #write.table(total1_phase3(), file, sep = sep, row.names = FALSE)
                                       writexl::write_xlsx(total1_phase3(),file)
                                     })
      # Title Page (Information Tab)
  output$Data_Coverage <- renderText({paste("Data Coverage: through", input$month)})
  output$lib.day1 <- renderText({paste("includes TrackWise 8.3 (data freezed:10MAY2019) & TrackWise 8.7 (as of dynamic date",input$Day,input$Month,input$Year,")")})
  output$lib.day2 <- renderText({paste("includes TrackWise 8.3 (data freezed:10MAY2019) & TrackWise 8.7 (as of dynamic date",input$Day,input$Month,input$Year,")")})
  
  
   }
 

  
  shinyApp(ui, server)
