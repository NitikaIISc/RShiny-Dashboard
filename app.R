#library(shiny)
library(dplyr)
library(tidyr)
#library(nycflights13)




######
##        https://mastering-shiny.org/basic-app.html      ##################


##rstudioapi::executeCommand('closeProject')<<to exit an R project

#Next, we’ll bring the outputs to life by defining them in the server function.

#Shiny uses reactive programming to make apps interactive. 
#You’ll learn more about reactive programming in Chapter 3, but for now, 
#just be aware that it involves telling Shiny how to perform a computation, not ordering Shiny to actually go do it. 
#It’s like the difference between giving someone a recipe versus demanding that they go make you a sandwich.



#setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/RShinyInteractive")
# data1<-read.csv("August25_TCFDCompCorrected_DataOutput1RecenctYearOnlyTruncatedAggregation.csv", header=T)
#setwd("C:/UCLAAnderson/10KfilingsTables/Data")
#data1=read.csv("FurtherCleaned_KellySentPartiallyIncorrectUpdatedData.csv", header=T,fileEncoding="latin1")
data1=read.csv("SecondEditToFixKellyMessyData.csv", header=T,fileEncoding="latin1")

colnames(data1)
packageVersion("shiny")
data=data1


##Average disclosure per pillar:

mean(data$Gov.Index, na.rm=TRUE)

library(ggplot2)
library(dplyr)
library(tidyr)


data2<-read.csv("August25_nonNACompCorrected_PivotedDataOutput2.csv", header=T)
colnames(data2)

##The tableau file has diffrent metric names as compared to raw data so 
######### Step 1: Replace ONLY the colnames of metrics that are used in the Tableau dashboard ################
unique(data2$Cleaned_names)
subset(data2, Cleaned_names=="Employee Ethnic Diversity")


variable_mapping<-unique(data2[,c("Variable_name", "Cleaned_names")]) ##creating a map between raw data colnames that correspond to metrics and cleaned names for Tableau 


colnames(data) <- variable_mapping[match(colnames(data1), variable_mapping[,1]), 2]

df=data1
df2=variable_mapping
####replacing colnames of metrics with Tableau cleaned metric names without hindering any other colnames or chning them to NA as match was doing

v <- colnames(df) %in% df2$Variable_name
w <- df2$Variable_name %in% colnames(df)
colnames(df)[v] <- df2$Cleaned_names[w]

head(df)
colnames(df)

library(shiny)
library(dplyr)
library(ggplot2)


##the function fluidPage to create a display that automatically adjusts to the dimensions of your user’s browser window. 
##You lay out the user interface of your app by placing elements in the fluidPage function.

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width=2,
                 selectInput(inputId = "sel_sector",
                             label = "Choose GICS Sector",
                             choices = c("All",unique(df$GICS.Sector)))),##Have an additional option of selecting all sectors
    #plotOutput("plot")
    mainPanel(#navlistPanel("Choose the metric of interest",tabPanel("Social metrics"),tabPanel("Governance metrics"),tabPanel("Planet metrics")),
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", fluidRow(column(12,
                                         splitLayout(plotOutput("Pillar_Disclosure_Rate",height = "500px"), 
                                                     plotOutput("AuditStatus",height = "500px")))), 
                 fluidRow(column(12,splitLayout(plotOutput("GHG_Disclosure_Rate",height = "500px"),plotOutput("EthnicityDisclosure",height = "500px")))), 
                                                fluidRow(column(12,splitLayout(plotOutput("GHG_MetricTons",height="500px"),plotOutput("EthnicGroupDisc", height="500px")))), 
                 fluidRow(column(12,splitLayout(plotOutput("womenPercentage",height="500px"),plotOutput("CEOmedian",height="500px"))))
                 
                 #, cellArgs = list(style = "vertical-align: bottom")
                 ),
        tabPanel(
          "Reference",
          tags$p(
            "There data were obtained from",
            tags$a("Center for Impact", href = "https://www.anderson.ucla.edu/about/centers/impactanderson"), "'s",
            tags$a("Open For Good Project", href = "https://www.anderson.ucla.edu/about/centers/impactanderson/open-for-good-transparency-index"), "."
          ),
          tags$p(
            "The data represent", #nrow(movies),
            "ESG data of S&P 500 companies between 2019 to 2022 in the United States."
          )),
        tabPanel("Methodology", tags$p("We calculated the ethnic disclosure rates and the breakdown of employees that were White, 
                                        Black, Hispanic and Asian based on the Equal Employment Opportunity breakdown. 
                                        The companies that did not follow this ethnic breakdown or only broke up employees as underrepresented
                                         or not were not considered to have fully diclosed their ethnic composition"))))))




#server
server <- function(input, output,session){
  # Adjust your server logic here
  # For example, if "All" is selected, you can use all sectors; otherwise, use the selected sector
  sel_sector <- reactive({
    req(input$sel_sector)
    if (input$sel_sector == "All") {
      unique(df$GICS.Sector)
    } else {
      input$sel_sector
    }
  })
  
  # # Define the sel_sector input variable as a reactive variable
  # sel_sector <- reactive({
  #   req(input$sel_sector)
  #   input$sel_sector
  # })
  
  #sales = read.csv("Sales_Sample.csv", header = TRUE,  sep = ",")
  
  
  # output$sel_sector<-reactive({
  #   req(input$sel_sector)
  
  
  output$Pillar_Disclosure_Rate <- renderPlot({
    
    req(input$sel_sector)### Ensure input$sel_sector is available and not NULL or empty
    #Summarize Data and then Plot
    
    
    #Selected_Sector<-subset(df, GICS.Sector==input$sel_sector)
    Selected_Sector <- subset(df, GICS.Sector %in% sel_sector())
    
    data_a<-as.data.frame(round(colMeans(Selected_Sector[,c("Gov.Index","Planet.Index","People.Index")], na.rm = TRUE),3)*100)
    rownames(data_a)<-c("Governance","Planet","People")
    colnames(data_a)<-("Average Disclosure rate by pillar")
    data_a$Pillar<-rownames(data_a)
    
    
    # lock in factor level order
    data_a$Pillar <- factor(data_a$Pillar, levels = data_a$Pillar)
    
    
    # Create the ggplot
    ggplot(data_a, aes(x = Pillar, y = `Average Disclosure rate by pillar`, fill = as.factor(Pillar), label = `Average Disclosure rate by pillar`)) +
      geom_bar(aes(y = `Average Disclosure rate by pillar`), stat = "identity") +
      geom_text(aes(label = paste(`Average Disclosure rate by pillar`, "%", sep = "")), vjust = 4.5, colour = "black", size = 8) +##change label colors here
      scale_fill_manual(values = c("#8BB8E8","#FFD100", "#2774AE")) +######THIS IS WHERE EDITS HAPPEN TO CHANGE COLORS OF BARS ETC.
      ylim(c(0,100))+
      #scale_fill_manual(values=group.colors)+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ## remove grids and background
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(text = element_text(size = 25)) +
      theme(axis.title.x = element_blank()) + # All font sizes
      theme(axis.text.x = element_text(size = 15, face = "bold", colour = "black")) +
      theme(axis.title.y = element_blank()) + # All font sizes
      theme(legend.position = "none") + ##remove all legends
      ggtitle(paste("Average Disclosure Rate by Pillar"," (N=",nrow(Selected_Sector),")",sep="")) +
      theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold")) +
      theme(axis.text = element_text(size = 20, face = "bold"), axis.title = element_text(size = 20, face = "bold"))
    
    
    
    #ggplotly(A1)
  })
  
  output$AuditStatus <- renderPlot({
    
    req(input$sel_sector)  # Ensure input$sel_sector is available and not NULL or empty
    
    
    #Selected_Sector<-subset(df, GICS.Sector==input$sel_sector)
    Selected_Sector <- subset(df, GICS.Sector %in% sel_sector())
    
    data_b<-(as.data.frame(table(Selected_Sector$Audited.Report)))
    colnames(data_b)<-c("AuditStatus","Freq")
    
    data_b<-data_b %>% 
      dplyr::mutate(across(c(AuditStatus), ~ recode(., "0" = "Not Audited", "0.5" = "Partially Audited", "1" = "Audited")))
    
    data_b$Proportion<-round((data_b$Freq/sum(data_b$Freq)),3)*100
    
    
    # Compute the position of labels
    data_b <- data_b %>% 
      arrange(desc(Freq)) %>%
      #mutate(Proportion = value / sum(data$value) *100) %>%
      mutate(ypos = cumsum(Proportion)-0.15*Proportion )
    
    
    # Basic piechart
    ggplot(data_b, aes(x="", y=Proportion, fill=AuditStatus)) +
      geom_bar(stat="identity", width=1, color="white") +
      scale_fill_manual(values = c("Not Audited" = "#000000",
                                   "Partially Audited" = "#005587",
                                   "Audited" = "#FFB81C"))+
      #scale_fill_manual(values = c("#000000", "#005587","#FFB81C"))+
      #geom_text(aes(y = ypos, label = Proportion), color = "white", size=6)+
      
      coord_polar("y", start=0) +
      #theme(axis.text = element_text(size = 20, face = "bold"), axis.title = element_text(size = 20, face = "bold"))
      # theme_void() # remove background, grid, numeric labels+
      ggtitle(paste("Audit Status"," (N=",nrow(Selected_Sector),")",sep=""))+
      theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold"))+
      geom_text(aes(label = paste(`Proportion`, "%", sep = "")), position = position_stack(vjust = 0.5),color = "white", size=6)+
      theme(axis.text=element_text(size=25), legend.text = element_text(size=20))+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),
            #axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),axis.ticks.y = element_blank(),panel.background = element_blank(),
            panel.grid  = element_blank()) 
    
    
    
  })
  
  
  
  output$GHG_Disclosure_Rate <- renderPlot({
    
    req(input$sel_sector)### Ensure input$sel_Company is available and not NULL or empty
    #Summarize Data and then Plot
    
    
    ##Selected_Sector<-subset(df, GICS.Sector==input$sel_sector)
    Selected_Sector <- subset(df, GICS.Sector %in% sel_sector())
    
    data_c<-as.data.frame(t(as.data.frame(cbind(Scope1=table(Selected_Sector$CM7a.GHG.Emissions.),Scope2=table(Selected_Sector$CM7b.GHG.Emissions.),Scope3=table(Selected_Sector$CM7c.GHG.Emissions.)))))
    #colnames(data_c)<-c("NotDisclosed","PartiallyDisclosed","Disclosed")
    data_c$Proportion<-round((data_c$`1`/rowSums(data_c)),3)*100
    colnames(data_c)
    
    
    
    # Create the ggplot
    ggplot(data_c, aes(x = rownames(data_c), y = `Proportion`, fill = as.factor(rownames(data_c)), label = `Proportion`)) +
      geom_bar(aes(y = `Proportion`), stat = "identity") +
      geom_text(aes(label = paste(`Proportion`, "%", sep = "")), vjust = 2.5, colour = "white", size = 8) +
      scale_fill_manual(values = c("#003B5C", "#005587", "#8BB8E8")) +
      #scale_fill_manual(values=group.colors)+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ## remove grids and background
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(text = element_text(size = 25)) +
      theme(axis.title.x = element_blank()) + # All font sizes
      theme(axis.text.x = element_text(size = 15, face = "bold", colour = "black")) +
      theme(axis.title.y = element_blank()) + # All font sizes
      theme(legend.position = "none") + ##remove all legends
      ggtitle(paste(paste("Greenhouse Gas Emissions","(GHG) Disclosure",sep="\n"),"(N=",nrow(Selected_Sector),")",sep="")) +
      #paste("Average Disclosure Rate by Pillar"," (N=",nrow(Selected_Sector),")",sep="")
      theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold")) +
      theme(axis.text = element_text(size = 20, face = "bold"), axis.title = element_text(size = 20, face = "bold"))
    
  })
  
  
  
  ######### Plot 4: % of women employees #################
  
  output$EthnicityDisclosure<-renderPlot({
    req(input$sel_sector)### Ensure input$sel_sector is available and not NULL or empty
    
    
    ##Selected_Sector<-subset(df, GICS.Sector==input$sel_sector)
    Selected_Sector <- subset(df, GICS.Sector %in% sel_sector())
    
    Selected_Sector$`Employee Ethnic Diversity`<-as.character(Selected_Sector$`Employee Ethnic Diversity`)
    
    dat_d<-as.data.frame(Selected_Sector[,c("GICS.Sector","Cleaned.Company.Name","Employee Ethnic Diversity")] %>%
                           dplyr::group_by(GICS.Sector,`Employee Ethnic Diversity`) %>%
                           dplyr::summarise(n = n()) %>%
                           dplyr::mutate(freq = round((n / sum(n)),3)*100))
    
    
    data_d<-subset(dat_d, `Employee Ethnic Diversity`==1)
    
    # Create the ggplot
    ggplot(data_d, aes(x = GICS.Sector, y = freq, fill = as.factor(GICS.Sector), label = freq)) +
     #geom_bar(aes(y = freq), stat = "identity") +
      geom_col(width = 0.75, position = position_dodge(width = 0.5)) +  # Adjust width as needed
      #scale_fill_brewer()+
      #scale_fill_viridis_c(option = "magma")+
      #scale_colour_gradientn(colours=c("#2774AE","#003B5C","#005587","#8BB8E8","#DAEBFE","#FFC72C","#FFFF00","#00FFFF","darkgreen","green","purple"))+#PuBuGn(length(unique(df$GICS.Sector))))+
      #scale_fill_manual(values = c("#003B5C", "#005587", "#8BB8E8")) +
      scale_fill_manual(values=rep("#2774AE", length(unique(df$GICS.Sector))))+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ## remove grids and background
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(text = element_text(size = 25)) +
      theme(axis.title.x = element_blank()) + # All font sizes
      theme(axis.text.x = element_text(size = 15, face = "bold", colour = "black")) +
      theme(axis.title.y = element_blank()) + # All font sizes
      theme(legend.position = "none") + ##remove all legends
      ggtitle(paste("Ethnic Diversity Disclosure"," (N=",nrow(Selected_Sector),")",sep="")) +
      theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold")) +
      theme(axis.text = element_text(size = 15, face = "bold"), axis.title = element_text(size = 20, face = "bold"))+
      coord_flip()+
      geom_text(aes(label = paste(freq, "%", sep = "")), hjust=0, colour = "black", size = 5)+
      ylim(c(0,100))+expand_limits(x= c(-1, length(unique(df$GICS.Sector)) + 1))##This allows to keep the X axis length limits consistent across sections of GICS. sectors
    
    ##To avoid resizing bars when All is not selcted in the drop-down:
    ##In this code, I replaced geom_bar with geom_col and set the width parameter to control the width of the bars.
    ##Adjust the width value as needed to achieve the desired appearance. 
    ##The position_dodge function is also used to separate the bars for different GICS sectors.
  })
  
  output$GHG_MetricTons <- renderPlot({
    
    req(input$sel_sector)### Ensure input$sel_Company is available and not NULL or empty
    #Summarize Data and then Plot
    
    
    ##Selected_Sector<-subset(df, GICS.Sector==input$sel_sector)
    Selected_Sector <- subset(df, GICS.Sector %in% sel_sector())
    
    GHGPerformaneData<-Selected_Sector[,c("Cleaned.Company.Name","GICS.Sector","CM7a.Response..in.me","CM7b.Scope.2..locati","CM7c.Response..metri")]
    #colnames(GHGPerformaneData)[-c(1,2)]<-c("Scope1","Scope2","Scope3")
    ##calculate N for each Scope
    SampleSize<-(as.data.frame(colSums(!is.na(GHGPerformaneData[,-c(1,2)]))))
    colnames(SampleSize)<-c("N")
    SampleSize$ScopeType<-rownames(SampleSize)
    # colnames(GHGPerformaneData)[-c(1,2)]<-c(paste("Scope 1 (N =",as.character(subset(SampleSize,ScopeType =="CM7a.Response..in.me")$N),")",sep="\n"),
    #                                         paste("Scope 2 (N =",as.character(subset(SampleSize,ScopeType =="CM7b.Scope.2..locati")$N),")",sep="\n"),
    #                                         paste("Scope 3 (N =",as.character(subset(SampleSize,ScopeType =="CM7c.Response..metri")$N),")",sep="\n"))
    # 
    # 
    colnames(GHGPerformaneData)[-c(1,2)]<-c(paste("Scope 1",paste("(N =",as.character(subset(SampleSize,ScopeType =="CM7a.Response..in.me")$N),")",sep=""),sep="\n"),
                                            paste("Scope 2",paste("(N =",as.character(subset(SampleSize,ScopeType =="CM7b.Scope.2..locati")$N),")",sep=""),sep="\n"),
                                            paste("Scope 3",paste("(N =",as.character(subset(SampleSize,ScopeType =="CM7c.Response..metri")$N),")",sep=""),sep="\n"))
    
    GHGPerformaneData[,c(colnames(GHGPerformaneData)[-c(1,2)])] <- sapply(GHGPerformaneData[,c(colnames(GHGPerformaneData)[-c(1,2)])],as.numeric)
    # # Clean up the column names and convert count information to character
    # colnames(GHGPerformaneData)[-c(1,2)] <- c(
    #   paste("Scope 1 (N =", as.character(subset(SampleSize, ScopeType == "CM7a.Response..in.me")$N), ")", sep = ""),
    #   paste("Scope 2 (N =", as.character(subset(SampleSize, ScopeType == "CM7b.Scope.2..locati")$N), ")", sep = ""),
    #   paste("Scope 3 (N =", as.character(subset(SampleSize, ScopeType == "CM7c.Response..metri")$N), ")", sep = "")
    # )
    # Use pivot_longer after modifying the column names
    #?pivot_longer
    data_e <- as.data.frame(GHGPerformaneData %>%
      tidyr::pivot_longer(
        cols = !c(Cleaned.Company.Name, GICS.Sector),
        names_to = "GHGScope",
        values_to = "Emissions"#,
        #values_ptypes = list(Emissions=character()
      ))
    
    # data_e<-GHGPerformaneData %>%
    #   pivot_longer(!(c(Cleaned.Company.Name,GICS.Sector)), names_to = "GHGScope", values_to = "Emissions")
    # #> # A tibble: 180 × 3
    data_e$MetricTons<-round(data_e$Emissions/1e6,0)
    
    library(ggplot2)
    library(plyr)
    # # calculate midpoints of bars (simplified using comment by @DWin)
    # Data_e <- ddply(data_e, .(GICS.Sector), 
    #               transform, pos = cumsum(MetricTons) - (0.5 * MetricTons)
    # )
    
    
    ggplot(data_e, aes(x = GICS.Sector, y = MetricTons, fill = GHGScope, label = MetricTons)) + 
      #geom_bar(stat = "identity") +
      geom_col(width = 0.95) +  # Adjust width as needed
      #geom_text(aes(label = MetricTons, y = pos), size = 3)+
      #scale_fill_brewer()+
      scale_fill_manual(values = c( "#003B5C","#FFB81C","#8BB8E8"))+#"#9E9AC8",
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ## remove grids and background
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(text = element_text(size = 25)) +
      #theme(axis.title.x = element_blank()) + # All font sizes
      theme(axis.text.x = element_text(size = 15, face = "bold", colour = "black")) +
      theme(axis.title.y = element_blank()) + # All font sizes
      theme(legend.position = "bottom") + ##remove all legends
      theme(legend.text=element_text(size=15))+theme(legend.key.size = unit(1.5,"line"))+
      theme(legend.title=element_blank())+
      ggtitle(paste("GHG Emissions","(Metric Tons)",sep="\n")) +
      theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold")) +
      theme(axis.text = element_text(size = 15, face = "bold"), axis.title = element_text(size = 20, face = "bold"))+
      coord_flip()+ylab("Emissions (Metric tons)")+expand_limits(x= c(0, length(unique(df$GICS.Sector))))##This allows to keep the X axis length limits consistent across sections of GICS. sectors
    
  })
  
  
  output$EthnicGroupDisc<-renderPlot({
    
    
    req(input$sel_sector)### Ensure input$sel_Company is available and not NULL or empty
    #Summarize Data and then Plot
    
    
    ##Selected_Sector<-subset(df, GICS.Sector==input$sel_sector)
    Selected_Sector <- subset(df, GICS.Sector %in% sel_sector())
    
    
    ###Employee ethnicity #######
    
    ethnicity<-Selected_Sector[,c("Cleaned.Company.Name","GICS.Sector","CM11c...White","CM11c...Black","CM11c...Hispanic","CM11c...Asian")]
    
    SampleSize<-(as.data.frame(colSums(!is.na(ethnicity[,-c(1,2)]))))
    colnames(SampleSize)<-c("N")
    SampleSize$EthnicGroup<-rownames(SampleSize)
    colnames(ethnicity)[-c(1,2)]<-c(paste("CM11c...White"," (N =",as.character(subset(SampleSize,EthnicGroup =="CM11c...White")$N),")",sep=""),
                                    paste("CM11c...Black"," (N =",as.character(subset(SampleSize,EthnicGroup =="CM11c...Black")$N),")",sep=""),
                                    paste("CM11c...Hispanic"," (N =",as.character(subset(SampleSize,EthnicGroup =="CM11c...Hispanic")$N),")",sep=""),
                                    paste("CM11c...Asian"," (N =",as.character(subset(SampleSize,EthnicGroup =="CM11c...Asian")$N),")",sep=""))
    
    
    library(tidyr)
    data_f<-ethnicity %>%
      pivot_longer(
        cols = starts_with("CM11c."),
        names_to = "ethnicity_cat",
        # names_pattern = "^(.*?)(\\d+)$",
        #names_prefix = "wk",
        values_to = "value",
        values_drop_na = FALSE
      )
    
    #ethnicity_pivot$ethnicity_cat <- factor(ethnicity_pivot$ethnicity_cat, levels=c('CM11c...White', 'CM11c...Black', 'CM11c...Hispanic', 'CM11c...Asian'))
    
    # ethnicity_pivot$axisLabel <- factor(ethnicity_pivot$axisLabel, levels=c('KBH', 'Selected Sectors', 'S&P 500'))
    
    ##Removing the "CM11c..." so that ggplot labels only have ethnicities
    data_f$ethnicity_cat<-gsub( "CM11c...", "", as.character(data_f$ethnicity_cat))
    
    data_f$ethnicity_cat <- factor(data_f$ethnicity_cat)
    
    # Create the ggplot
    ggplot(data_f, aes(x = ethnicity_cat, y = value, fill = ethnicity_cat, label = value)) +
      geom_bar(aes(y = value), stat = "identity") +
      geom_text(aes(label = paste(value, "%", sep = "")), vjust = 2.5, colour = "white", size = 8) +
      scale_fill_manual(values = c("#003B5C", "#8BB8E8","#2774AE","#FFD100")) +
      #scale_fill_manual(values=group.colors)+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ## remove grids and background
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(text = element_text(size = 25)) +
      theme(axis.title.x = element_blank()) + # All font sizes
      theme(axis.text.x = element_text(size = 15, face = "bold", colour = "black")) +
      theme(axis.title.y = element_blank()) + # All font sizes
      theme(legend.position = "none") + ##remove all legends
      ggtitle(paste("% of Employees by Ethnic Group")) +
      #paste("Average Disclosure Rate by Pillar"," (N=",nrow(Selected_Sector),")",sep="")
      theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold")) +
      theme(axis.text = element_text(size = 20, face = "bold"), axis.title = element_text(size = 20, face = "bold"))
    
    
    
    
  })
  
  
  output$womenPercentage<-renderPlot({
    
    
    
    req(input$sel_sector)### Ensure input$sel_Company is available and not NULL or empty
    #Summarize Data and then Plot
    
    
    ##Selected_Sector<-subset(df, GICS.Sector==input$sel_sector)
    Selected_Sector <- subset(df, GICS.Sector %in% sel_sector())
    
    #df$'% Employee gender diversity'
    dat_g<-as.data.frame(Selected_Sector[,c("GICS.Sector","% Employee gender diversity")] %>%
                           dplyr::group_by(GICS.Sector) %>% 
                           dplyr::summarize(Avg_EmployeeGenderDiversity = round(mean(`% Employee gender diversity`,na.rm=TRUE),0)))
    # dplyr::summarise(n = n()) %>%
    # dplyr::mutate(freq = round((n / sum(n)),3)*100))
    
    data_g<-dat_g[order(data_g$Avg_EmployeeGenderDiversity, decreasing = TRUE),]  
    data_g$GICS.Sector<-factor(data_g$GICS.Sector)
    #data_g<-subset(dat_g, `% Employee gender diversity`==1)
    
    # Create the ggplot
    # aes(x = reorder(Name, Number), Number))
    ggplot(data_g, aes(x = reorder(GICS.Sector, Avg_EmployeeGenderDiversity), Avg_EmployeeGenderDiversity, fill = as.factor(GICS.Sector), label = Avg_EmployeeGenderDiversity)) +
      #geom_bar(aes(y = freq), stat = "identity") +
      geom_col(width = 0.75, position = position_dodge(width = 0.5)) +  # Adjust width as needed
      #scale_fill_brewer()+
      #scale_fill_viridis_c(option = "magma")+
      #scale_colour_gradientn(colours=c("#2774AE","#003B5C","#005587","#8BB8E8","#DAEBFE","#FFC72C","#FFFF00","#00FFFF","darkgreen","green","purple"))+#PuBuGn(length(unique(df$GICS.Sector))))+
      #scale_fill_manual(values = c("#003B5C", "#005587", "#8BB8E8")) +
      scale_fill_manual(values=rep("#2774AE", length(unique(df$GICS.Sector))))+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ## remove grids and background
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(text = element_text(size = 25)) +
      theme(axis.title.x = element_blank()) + # All font sizes
      theme(axis.text.x = element_text(size = 15, face = "bold", colour = "black")) +
      theme(axis.title.y = element_blank()) + # All font sizes
      theme(legend.position = "none") + ##remove all legends
      ggtitle(paste("% Women Employees",paste("(N=",sum(is.na(df$'% Employee gender diversity')),")",sep=""),sep="\n")) +
      theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold")) +
      theme(axis.text = element_text(size = 15, face = "bold"), axis.title = element_text(size = 20, face = "bold"))+
      coord_flip()+
      geom_text(aes(label = paste(Avg_EmployeeGenderDiversity, "%", sep = "")), hjust=0, colour = "black", size = 5)+
      ylim(c(0,100))+expand_limits(x= c(-1, length(unique(df$GICS.Sector)) + 1))##This allows to keep the X axis length limits consistent across sections of GICS. sectors
    
  })
  
 output$CEOmedian<-renderPlot({
   
   
   req(input$sel_sector)### Ensure input$sel_Company is available and not NULL or empty
   #Summarize Data and then Plot
   
   
   ##Selected_Sector<-subset(df, GICS.Sector==input$sel_sector)
   Selected_Sector <- subset(df, GICS.Sector %in% sel_sector())
   
   data_h<-Selected_Sector
   data_h$`CEO to median employee compensation ratio`<-as.numeric(as.character(data_h$`CEO to median employee compensation ratio`))
   hist(as.numeric(as.character(data_h$`CEO to median employee compensation ratio`)))
   sample<-sum(!is.na(data_h$`CEO to median employee compensation ratio`))
   
   ggplot(data_h, 
          aes(x=`CEO to median employee compensation ratio`)) + 
     # geom_histogram(data=subset(plot5data, `CEO to median employee compensation ratio`!=gsub(":.*","",KBH$`CEO to median employee compensation ratio`)), color="white", aes(x=`CEO to median employee compensation ratio`), fill = "#005587",binwidth=50) +
     # geom_histogram(data=subset(plot5data, `CEO to median employee compensation ratio`==gsub(":.*","",KBH$`CEO to median employee compensation ratio`)), aes(x=`CEO to median employee compensation ratio`), fill="#FFD100", binwidth=50)+
     geom_histogram(binwidth=50, color="white",
                    aes(x=`CEO to median employee compensation ratio`,
                        #fill=factor(ifelse(`CEO to median employee compensation ratio`!=(subset(plot5data, ExternalReference=="KBH2022"))$`CEO to median employee compensation ratio`,"Normal","Highlighted")))
                        fill="#2774AE")) +
     scale_fill_manual(values="#2774AE")+
     # scale_fill_manual(name = "CEO to median employee compensation ratio", values=c("Normal"="#005587","Highlighted"="#FFD100")) +
     #scale_fill_manual(name = "CEO to median employee compensation ratio", values=c("TRUE"="#FFD100","FALSE"="#005587")) +
     xlim(NA, 2000)+
     ylab("Number of Companies")+
     xlab("Ratio of CEO compensation to Median Employee Wage")+
     theme(axis.title.y=element_blank())+ # All font sizes
     theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                       panel.background = element_blank(), axis.line = element_line(colour = "black"))+
     theme(axis.title.x = element_text(size=20, face="bold", colour = "black"))+
     theme(axis.title.y = element_text(size=20, face="bold", colour = "black"))+
     theme(axis.text.x = element_text(size=20, face="bold", colour = "black"))+
     theme(axis.text.y = element_text(size=20, face="bold", colour = "black"))+
     #ggtitle("CEO to Median Employee Compensation Ratio")+
     labs(title = paste("CEO to Median Employee Compensation Ratio",paste("(N=",sample,")",sep=""),sep="\n"),
          tag = paste("The values here represent the ratio of the", "CEO's total compensation to the median employee's compensation.", "For example, a value of 1000 indicates that the", "CEO's compensation is 1000 times more than their median employee's.",sep="\n")) +
     theme(plot.title = element_text(size=20,hjust = 0.5),
           plot.tag.position = "bottom")+
     theme(plot.title = element_text(size=25,hjust = 0.5, face="bold"))+
     theme(legend.position="none")
   
   
   
 }) 
}


shinyApp(ui = ui, server = server)

##turn off API access
