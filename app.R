rm(list = ls())
suppressPackageStartupMessages(library(shiny))
library(shinyhelper)
#library(shinyjs)
suppressMessages(library(DT))
library(readr)
suppressMessages(library(dplyr))
suppressMessages(library(forcats))
library(ggplot2)
suppressMessages(library(wordcloud))
library(RColorBrewer)
library("cowplot")
library("shinyWidgets")
suppressMessages(library("writexl"))

# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
# devtools::install_github("https://github.com/hrbrmstr/hrbrthemes")
library(ggradar)

# Sankey diagram
suppressMessages(library(viridis))
suppressMessages(library(patchwork))
suppressMessages(library(hrbrthemes))
suppressMessages(library(circlize))
suppressMessages(library(networkD3))
suppressMessages(library(htmlwidgets))

source("dragables.R")

### Constants  and (invariant) source codes ####
my.colors<-c('grey15','grey85','red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta',
             'limegreen','pink','darkorange3','aquamarine','beige','darkslategray','brown1','blueviolet','chocolate1' )

help_dir <-"helpfiles"
OS<- .Platform$OS.type  #operating system

#selectedAreas<-c('North Sea','Baltic Sea')


source("flsms.control.r",local=TRUE) # to handle file SMS.dat with options for running SMS in hindcast and producing data for forecast
source("flop.control.r",local=TRUE)

source('make_plots.R',local=TRUE)

source('do_OP.R',local=TRUE)

## end Constants #####

## Global
op.n<-0 #counter for calls to op.exe, used for tests only
area.change.no<-0


####  various functions #####
source('utils.R',local=TRUE)


#######################################
#####  update_environment

my.environment<-environment()
my.app.dir<-getwd()


load_ecoRegion(ar='North Sea')

selectedArea<-selectedAreas[1]
doRunModel<-TRUE           # flag for re-running the prediction model
doWriteOptions<-TRUE       # flag for writing option files for the prediction model
doWriteExplPattern<- FALSE # flag for writing exploitation pattern file (op_exploitation.in)

########################################

ui <- navbarPage(title = "SMS",
        tabPanel(title='ReadMe',
                 column(2,
                 radioButtons(inputId = 'language',label='Select language',choices=c('Danish','English')),
                 radioButtons(inputId = 'SMSarea',label='Select area',choices=selectedAreas)
                 ),
                 conditionalPanel("input.language=='English' & input.SMSarea=='North Sea'",includeMarkdown(file.path(help_dir, "SMS-intro.md"))),
                 conditionalPanel("input.language=='English' & input.SMSarea=='Baltic Sea'",includeMarkdown(file.path(help_dir, "SMS-intro-baltic.md"))),
                 conditionalPanel("input.language=='Danish'  & input.SMSarea=='North Sea'",includeMarkdown(file.path(help_dir, "SMS-intro_DK.md"))),
                 conditionalPanel("input.language=='Danish'  & input.SMSarea=='Baltic Sea'",includeMarkdown(file.path(help_dir, "SMS-intro-baltic_DK.md")))
        ),
        tabPanel(title='Simple predictions',
               tabsetPanel(id='simple_predict',
                   tabPanel(title = "Predictions",
                    column(3,
                          checkboxInput("effcontrolAll", "Same factor for all fleets?", value = TRUE) %>%
                            helper(colour = "green", type = "markdown",content = "SameFactor"),
                          conditionalPanel("input.effcontrolAll==1", sliderInput(inputId="F.all",label="F factor",
                                                       min = 0.5, max = 2.0, value = 1, step = 0.05)),
                          #conditionalPanel("input.effcontrolAll==0",sliders),
                          conditionalPanel("input.effcontrolAll==0 & input.SMSarea=='North Sea'",slidersNS),
                          conditionalPanel("input.effcontrolAll==0 & input.SMSarea=='Baltic Sea'",slidersBS),
                          br(),
                          downloadButton(outputId = "radarPlots1", label = "Download the plot")
                    ),
                    column(4,
                         plotOutput(outputId = "F_plot1") %>%
                           helper(colour = "green", type = "markdown",content = "radar",size = "l"),
                         plotOutput(outputId = "Yield_plot1")
                    ),
                    column(4,
                         plotOutput(outputId = "rec_plot1"),
                         plotOutput(outputId = "SSB_plot1")
                    )
                   ),
                   tabPanel(title='Change Baseline',
                    column(3,
                      br(),
                      radioButtons(inputId = 'bas_F_s',label='Baseline for F',choiceNames=bsF$Names,choiceValues = bsF$Values) %>%
                        helper(colour = "green", type = "markdown",content = "BaseLineF"),
                      radioButtons(inputId = 'bas_Rec_s',label='Baseline for recruitment',choiceNames=bsRec$Names,choiceValues = bsRec$Values),
                      radioButtons(inputId = 'bas_Yield_s',label='Baseline for yield',choiceNames=bsYield$Names,choiceValues = bsYield$Values),
                      radioButtons(inputId = 'bas_SSB_s',label='Baseline for SSB',choiceNames=bsSSB$Names,choiceValues = bsSSB$Values),
                      br(),
                      downloadButton(outputId = "radarPlots2", label = "Download the plot")),
                    column(4,
                           plotOutput(outputId = "F_plot3"),
                           plotOutput(outputId = "Yield_plot3")
                    ),
                    column(4,
                           plotOutput(outputId = "rec_plot3"),
                           plotOutput(outputId = "SSB_plot3")
                    )

                   ),
                   tabPanel(title='Table output',br(),DTOutput('tableOut1'))
              )
        ),
        tabPanel(title='Detailed predictions',
              tabsetPanel(id='detailed_predict',
                   tabPanel(title='Options',
                    column(4,
                        # for picture in pickerInput
                         tags$head(tags$style("
                           .jhr{
                           display: inline;
                           vertical-align: middle;
                           padding-left: 10px;
                           }")),
                          br(),
                        wellPanel(radioButtons(inputId='Option', label='Select option', choices=list('Final year','F model','Other predators','Exploitation pattern',"Recruitment")))%>%
                           helper(colour = "green", type = "markdown",content = "Option"),
                        conditionalPanel("input.Option=='Recruitment'",
                             wellPanel(
                                radioButtons(inputId = 'recDetSto',label='Recruitment variability ',choices=list('Determenistic','Stochastic')),
                             ) %>% helper(colour = "green", type = "markdown",content = "Recruitment"),
                             conditionalPanel("input.recDetSto=='Stochastic'",textOutput("stoch_explain"))
                        ),

                        conditionalPanel("input.Option=='F model'",
                          wellPanel(
                           selectInput(inputId="HCR.sp", label="Species",choices=VPA.spNames),
                           pickerInput(inputId = "HCR",label = "Harvest Control Rule",choices = hcr$val,choicesOpt = list(content = hcr$img)),
                           numericInput(inputId="target.F",label="Target F",value=Foption_tab[1,'target.F'],min=0,max=2,step=0.01),
                           splitLayout(
                            conditionalPanel("input.HCR!=' 1: Fixed F'",numericInput(inputId="T1",label=paste("T1",plotLabels['SSB']),value=Foption_tab[1,'T1'],min=0,step=1)),
                            conditionalPanel("input.HCR!=' 1: Fixed F'",numericInput(inputId="T2",label=paste("T2",plotLabels['SSB']),value=Foption_tab[1,'T2'],min=0,step=1))
                           )) %>% helper(colour = "green", type = "markdown",content = "Fmodel")
                          ),

                         conditionalPanel("input.Option=='Other predators'",
                                          selectInput(inputId="OtherSp", label="Other predator",choices=other.spNames),
                                          sliderInput(inputId="OtherFirst",label="First year for change",min = stq_year+1, max = termYear, value = stq_year+1, step =1,sep=''),
                                          sliderInput(inputId="OtherSecond",label="last year for change",min = stq_year+1, max = termYear, value = termYear, step =1,sep=''),
                                          numericInput(inputId="OtherFactor",label="Change factor per year, (e.g. 1.1 means a 10 % increase per year)",value=1 ,min=-2,step=0.01)
                         ),

                        conditionalPanel("input.Option=='Exploitation pattern'",
                                         selectInput(inputId="exSpecies", label="Species",choices=VPA.spNames)),


                         conditionalPanel("input.Option=='Final year'",sliderInput(inputId="finalYear",label="Final year in prediction",min = stq_year+1, max = stq_year+100, value = termYear, step =1,sep='')%>%
                                                                       helper(colour = "green", type = "markdown",content = "finalYear")),
                         conditionalPanel("input.Option=='F model'",actionButton(inputId="updateOptionTable", "Update option table")),
                         conditionalPanel("input.Option=='Other predators'",actionButton(inputId="updateOptionTableOther", "Update option table"))
                         ),
                    conditionalPanel("input.Option=='F model'",column(7,br(),tableOutput(outputId="HCRtable1"))),
                    conditionalPanel("input.Option=='Other predators'",column(7,br(),tableOutput(outputId="Othertable"))),
                    conditionalPanel("input.Option=='Other predators'",column(7,plotOutput(outputId = "other_plot"))),
                    conditionalPanel("input.Option=='Exploitation pattern'",column(7,
                                       br(),h3('Drag  the individual bar to change relativ F at age'),br(),DragableChartOutput("testdrag", width = "440px")))

                  ) ,
                  tabPanel(title='Results',
                           column(4,
                             br(),br(),

                           actionButton(inputId="doRunDetailed",label="Push to update prediction",icon("sync")),
                           br(),br(),br(),
                           downloadButton(outputId = "radarPlots3", label = "Download the plot")),

                           column(4,
                                  plotOutput(outputId = "F_plot2"),
                                  plotOutput(outputId = "Yield_plot2")
                           ),
                           column(4,
                                  plotOutput(outputId = "rec_plot2"),
                                  plotOutput(outputId = "SSB_plot2")
                           )
                  ),
                  tabPanel(title='Change Baseline',
                      column(4,
                          br(),
                          radioButtons(inputId = 'bas_F_d',label='Baseline for F',choiceNames=bsF$Names,choiceValues = bsF$Values),
                          radioButtons(inputId = 'bas_Rec_d',label='Baseline for recruitment',choiceNames=bsRec$Names,choiceValues = bsRec$Values),
                          radioButtons(inputId = 'bas_Yield_d',label='Baseline for yield',choiceNames=bsYield$Names,choiceValues = bsYield$Values),
                          radioButtons(inputId = 'bas_SSB_d',label='Baseline for SSB',choiceNames=bsSSB$Names,choiceValues = bsSSB$Values),
                          br(),
                          downloadButton(outputId = "radarPlots4", label = "Download the plot")
                      ),
                      column(4,
                             plotOutput(outputId = "F_plot4"),
                             plotOutput(outputId = "Yield_plot4")
                      ),
                      column(4,
                             plotOutput(outputId = "rec_plot4"),
                             plotOutput(outputId = "SSB_plot4")
                      )

                  ),
                 tabPanel(title='Table output',br(),
                          radioButtons(inputId = 'tabOpt',label='Select table',choices=c('Results','Options')),
                             conditionalPanel("input.tabOpt=='Results'",column(8,DTOutput('tableOut2'))),
                             conditionalPanel("input.tabOpt=='Options'",column(8,tableOutput(outputId="HCRtable2"))),
                 ),

                 tabPanel(title="Results by year",
                      column(2,br(),
                         selectInput(inputId="sumSpecies", label="Select Species:",choices=VPA.spNames),
                         sliderInput(inputId="firstY",label="First year output",value=stq_year+1,min=fy_year_hist,max=termYear,step=1,sep=''),
                         sliderInput(inputId="lastY",label="Last year output",value=termYear,min=fy_year_hist+5,max=termYear,step=1,sep=''),
                         radioButtons(inputId = 'inclRef',label='Include reference points',choices=c('yes','no')) %>%
                           helper(colour = "green", type = "markdown",content = "reference_points"),
                         br(), downloadButton(outputId = "downSumPlots", label = "Download the plot")
                       #  wellPanel(
                      #     sliderInput(inputId = 'pixx',label='Width plot', value=1200,min=100,max=2000,step=100),
                      #     sliderInput(inputId = 'pixy',label='Height plot', value=750,min=100,max=2000,step=100)
                      #   )
                        ),
                      column(10,  plotOutput(outputId = "summary_plot"))
                  ),
                 tabPanel(title = "Who eats whom",
                      column(3,
                          br(),
                          wellPanel(
                            selectInput(inputId="whoPred", "Select a predator:",'all predators'),
                            selectInput(inputId="whoPrey", "Select a prey:",'all preys')
                          ),
                          radioButtons(inputId="whoHuman",label='Include humans as "predator"',choices = c('Incl. catch','Excl. catch'),inline=TRUE),
                          radioButtons(inputId="whoResidual",label='Include "Residual mortality" (M1) as "predator"',choices = c('Incl. M1','Excl. M1'),inline=TRUE),
                         # radioButtons(inputId="whoOtherFood",label='Include "other foods"',choices = c('Incl. other','Excl. other')),
                          radioButtons(inputId="whoPredPrey",label='select value for stacking',choices = c('by prey','by predator'),inline=TRUE),
                          wellPanel(
                            sliderInput(inputId="firstYwho",label="First year output",value=stq_year+1,min=fy_year_hist,max=termYear,step=1,sep=''),
                            sliderInput(inputId="lastYwho",label="Last year output",value=termYear,min=fy_year_hist+5,max=termYear,step=1,sep='')
                          ),
                       ),
                      column(9,br(),plotOutput(outputId = "whoEats_plot")),
                      downloadButton(outputId = "downWhoEats", label = "Download the plot"),
                      downloadButton(outputId = "downWhoEatsExcel", label = "Download data for the plot")
                ),
                tabPanel(title = "Food web",
                         column(3,
                                br(),
                                 wellPanel(
                                   sliderInput(inputId="yearFoodWeb",label="Year",value=stq_year+1,min=fy_year_hist,max=termYear,step=1,sep=''),
                                 ),
                                checkboxGroupInput(inputId="foodWebSp",label='Select species',
                                  choices = pp_short$new,
                                  selected = filter(pp_short, group %in% c("Other predators","VPA.pred","VPA.prey"))$new,
                                  inline = FALSE
                                )
                         ),
                         column(9,br(),sankeyNetworkOutput(outputId ="FoodWeb_plot",height = "700px")),
                         downloadButton(outputId = "downFoodWeb", label = "Download the plot")
                )
            )),
        tabPanel(title='About',includeMarkdown(file.path(help_dir, "about.md")))
)

 server <- function(input, output, session) {

   res <- reactiveValues(rv = list(out=do_OP(readResSimple=TRUE,writeOption=doWriteOptions,source='init'),Fmulti=rep(F_mult,n.fleet),baseLine=do_baseLine()))

   # uses 'helpfiles' directory by default
   # in this example, we do not use the withMathJax parameter to render formulae
   observe_helpers(withMathJax = FALSE)


   output$F_plot1     <- renderPlot({ plot_one(res$rv,type='Fbar')     })
   output$Yield_plot1 <- renderPlot({ plot_one(res$rv,type='Yield')    })
   output$SSB_plot1   <- renderPlot({ plot_one(res$rv,type='SSB')      })
   output$rec_plot1   <- renderPlot({ plot_one(res$rv,type='Recruits') })

   output$F_plot2     <- renderPlot({ plot_one(res$rv,type='Fbar')     })
   output$Yield_plot2 <- renderPlot({ plot_one(res$rv,type='Yield')    })
   output$SSB_plot2   <- renderPlot({ plot_one(res$rv,type='SSB')      })
   output$rec_plot2   <- renderPlot({ plot_one(res$rv,type='Recruits') })

   output$F_plot3     <- renderPlot({ plot_one(res$rv,type='Fbar')     })
   output$Yield_plot3 <- renderPlot({ plot_one(res$rv,type='Yield')    })
   output$SSB_plot3   <- renderPlot({ plot_one(res$rv,type='SSB')      })
   output$rec_plot3   <- renderPlot({ plot_one(res$rv,type='Recruits') })

   output$F_plot4     <- renderPlot({ plot_one(res$rv,type='Fbar')     })
   output$Yield_plot4 <- renderPlot({ plot_one(res$rv,type='Yield')    })
   output$SSB_plot4   <- renderPlot({ plot_one(res$rv,type='SSB')      })
   output$rec_plot4   <- renderPlot({ plot_one(res$rv,type='Recruits') })

   output$other_plot   <- renderPlot({plot_other(sp=input$"OtherSp",firsty=input$"OtherFirst",lasty=input$"OtherSecond",chOther=input$"OtherFactor",firstYear=stq_year+1, finalYear=input$finalYear) })


   output$stoch_explain <- renderText({paste('Constant Fishing mortalities will probably not work for stochastic recruitment. You probably have to defined Harvest Control Rules in the "F-model" option above,',
                                             'starting with the default values')})
   output$summary_plot <-renderPlot({   if (res$rv$out$options$readResDetails) {sumPlot<<- plot_summary_new(res=res$rv,ptype=c('Yield','Fbar','SSB','Recruits','Dead','M2'),
                                                                                            years=c(input$firstY,input$lastY),species=input$sumSpecies,splitLine=FALSE,
                                                                                            incl.reference.points= (input$inclRef=='yes'));sumPlot}},
                                                             width = 1350, height=700,units = "px", pointsize = 25, bg = "white")

   output$whoEats_plot <- renderPlot({whoPlot<<-plot_who_eats(res$rv$out$detail_eaten,pred=input$whoPred,prey=input$whoPrey,predPrey=input$"whoPredPrey",
                                                     years=c(input$firstYwho,input$lastYwho),exclHumans=(input$whoHuman=='Excl. catch'),exclResidM1=(input$whoResidual=='Excl. M1'));whoPlot})

   output$FoodWeb_plot <- renderSankeyNetwork({foodWebPlot<<-FoodWeb_plot(res$rv$out$detail_eaten,year=input$yearFoodWeb,incl_sp=input$foodWebSp );foodWebPlot})

  # to get full functionality with respect to drag and click. does not improve much and I don't know how to solve it!
  # output$FoodWeb_plot <- renderSankeyNetwork({foodWebPlot<-FoodWeb_plot(res$rv$out$detail_eaten,year=input$yearFoodWeb,incl_sp=input$foodWebSp)
  # clickFun <-
  #   'function() {
  #        d3.selectAll(".node").on("mousedown.drag", null);
  #        d3.selectAll(".node").on("click",function(d) { Shiny.onInputChange("id", d.name); });
  #      }'
  #
  # onRender(foodWebPlot, clickFun)
  # })


   output$downSumPlots <- downloadHandler(
     filename =  function() {paste0("Summary_",input$sumSpecies,'.png')},
     content = function(file) {
         ggsave(file,plot=sumPlot,width = 25,height = 15,units='cm')
     }
   )

   output$downWhoEats <- downloadHandler(
     filename =  function() {paste0("Who_",input$whoPred,'_',input$whoPrey,'.png')},
     content = function(file) {
       ggsave(file,plot=whoPlot,width = 26,height = 15,units='cm')
     }
   )

   output$downWhoEatsExcel <- downloadHandler(
     filename =  function() {paste0("Who_",input$whoPred,'_',input$whoPrey,'.xlsx')},
     content = function(file) {
       writexl::write_xlsx(xxcel,path = file)
     }
   )

   output$downFoodWeb <- downloadHandler(
     filename =  function() {paste0("foodWeb_",input$yearFoodWeb,".html")},
     content = function(file) {
       saveWidget(foodWebPlot, file=file )
     }
   )



   ######### there must be a smarter way to do the same thing
   output$radarPlots1 <- downloadHandler(
     filename = "radar_myPlot.png",
     content = function(file) {
       png(filename=file,width = 700,height = 700,units='px')
       print(plot_radar_all(res$rv))
       dev.off()
     }
   )

   output$radarPlots2 <- downloadHandler(
     filename = "radar_myPlot.png",
     content = function(file) {
       png(filename=file,width = 700,height = 700,units='px')
       print(plot_radar_all(res$rv))
       dev.off()
     }
   )

   output$radarPlots3 <- downloadHandler(
     filename = "radar_myPlot.png",
     content = function(file) {
       png(filename=file,width = 700,height = 700,units='px')
       print(plot_radar_all(res$rv))
       dev.off()
     }
   )

   output$radarPlots4 <- downloadHandler(
     filename = "radar_myPlot.png",
     content = function(file) {
       png(filename=file,width = 700,height = 700,units='px')
       print(plot_radar_all(res$rv))
       dev.off()
     }
   )
   ###########

   output$tableOut1 <- renderDT(
     DT::datatable(makeResTable(res$rv),rownames=FALSE, filter ="none",options=list(pageLength = n.VPA))  %>%
       formatPercentage(columns=c(4,7,10),digits=1))

   output$tableOut2 <- renderDT(
       DT::datatable(makeResTable(res$rv),rownames=FALSE, filter ="none",options=list(pageLength = n.VPA))  %>%
         formatPercentage(columns=c(4,7,10),digits=1))

   output$HCRtableIn1 <- renderDT(
     DT::datatable(df_opt(),rownames=FALSE,filter ="none",editable = list(target = "row", disable = list(columns = c(1))),options=list(pageLength = n.VPA))
   )

   output$HCRtableIn2<- renderDT(
     DT::datatable(df_opt(),rownames=FALSE,filter ="none",editable = list(target = "row", disable = list(columns = c(1))),options=list(pageLength = n.VPA))
   )

   output$statusRun<-renderText(paste("doRunModel",doRunModel ,ifelse(doRunModel,'Prediction has not been updated','Up to data prediction')))


   output$testdrag <- renderDragableChart({
    as.vector( annExplPat[paste('age',as.character(first.age:last.age[input$exSpecies])),input$exSpecies])
    }, labels = rownames(annExplPat)
   )


   #result from testdrag ??
   observeEvent(input$rv, {
     annExplPat[paste('age',as.character(first.age:last.age[input$exSpecies])),input$exSpecies]<<-input$rv
     doWriteExplPattern<<-TRUE
     updateActionButton(session, inputId="doRunDetailed", label = 'Push to update prediction',icon = icon("sync"))
   })

  #####


  df <- eventReactive(input$updateOptionTable || input$Option=='F model', {
   if (input$Option=='F model') {
      b<-Foption_tab
      b[b$Species== input$HCR.sp,'HCR']<-input$HCR
      b[b$Species== input$HCR.sp,'target.F']<-input$target.F
      b[b$Species== input$HCR.sp,'T1']<-input$T1
      b[b$Species== input$HCR.sp,'T2']<-input$T2
      Foption_tab<<-b
      doWriteOptions<<-TRUE
      doRunModel<<-TRUE
      OP.trigger<<-put_op_Fmodel(b,OP.trigger) #update OP_trigger
      return(b)
   }
  })

  otherDf <- eventReactive(input$updateOptionTableOther || input$Option=='Other predators',{
   #cat(input$updateOptionTableOther,input$Option,input$OtherFactor,input$OtherFirst,input$OtherSecond,'\n')
     if (input$Option=='Other predators') {
       b<-other_predators
       b[b$Predator== input$OtherSp,'Total.change']<- input$OtherFactor**(input$OtherSecond-input$OtherFirst+1)

       b[b$Predator== input$OtherSp,'change']<-input$OtherFactor
       b[b$Predator== input$OtherSp,'First.year']<-input$OtherFirst
       b[b$Predator== input$OtherSp,'Last.year']<-input$OtherSecond
       other_predators<<-b
       doWriteOptions<<-TRUE
       doRunModel<<-TRUE
       OP<<-put_other_predators(b,OP) #update OP
       return(b)
     }
   })


   df2 <- eventReactive( input$tabOpt=='Options', {return(Foption_tab)})

   # make a new prediction with detailed output
   doUpdateDetails<-function(){
     OP@output<<-25  #both condensed and annual output
     res$rv$out<-do_OP(readResSimple=TRUE,readResDetails=TRUE,writeOption=TRUE, writeExplPat=doWriteExplPattern,source='push Detailed')
     updateActionButton(session, inputId="doRunDetailed", label = 'Prediction is updated',icon = character(0))
   }

   # make a new prediction with detailed output and partial M2
   doUpdateDetailsM2<-function(){
     showModal(modalDialog("Doing a complex prediction run, please wait a few seconds", footer=NULL))
     OP@output<<-26  #both condensed and annual and quarterly  output and M2
     res$rv$out<-do_OP(readResSimple=TRUE,readResDetails=TRUE,readResStom=TRUE,writeOption=TRUE,writeExplPat=doWriteExplPattern,source='push DetailedM2')
     removeModal()
     updateSelectInput(session,inputId="whoPred",choices=c('all predators',res$rv$out$pred))
     updateSelectInput(session,inputId="whoPrey",choices=c('all preys',res$rv$out$prey))
   }

   observeEvent(input$whoPred,{if (input$whoPred !='all predators') updateSelectInput(session,inputId="whoPrey",choices=c('all preys',res$rv$out$predPrey[[input$whoPred]])) })

   observeEvent(input$whoHuman,{if (input$whoHuman =='Excl. catch') updateSelectInput(session,inputId="whoPred",choices=c('all predators',res$rv$out$pred)) })




   observeEvent(input$detailed_predict,{
     if (input$detailed_predict=='Results by year') doUpdateDetails() else if (input$detailed_predict=='Who eats whom' | input$detailed_predict=="Food web") doUpdateDetailsM2()
    })

   observeEvent(input$doRunDetailed, {doUpdateDetails()})

   updateFoption_single<-function(sp){
     #sp<-input$HCR.sp
     updateNumericInput(session,inputId="target.F",value=Foption_tab[sp,'target.F'])
     updateNumericInput(session,inputId="T1",value=Foption_tab[sp,'T1'])
     updateNumericInput(session,inputId="T2",value=Foption_tab[sp,'T2'])
     updatePickerInput(session,inputId ="HCR",selected=Foption_tab[sp,'HCR'])
   }


  observeEvent(input$HCR.sp,{updateFoption_single(input$HCR.sp)})


  output$HCRtable1<- renderTable(df(),digits=3)
  output$HCRtable2<- renderTable(df2(),digits=3)

  output$Othertable<- renderTable(otherDf())

  observeEvent(input$F.all, {
     val <- input$F.all
     vals<-sapply(paste(ars, fleetNames,sep='_'), function(item) input[[item]])
      if (input$effcontrolAll) purrr::walk(paste(ars,fleetNames,sep='_'), function(id) updateSliderInput(session, id, value = val))
   },ignoreInit = TRUE)


  observeEvent(input$firstY,{
    updateSliderInput(session,inputId="lastY",min=input$firstY+5)
  })

  observeEvent(input$firstYwho,{
    updateSliderInput(session,inputId="lastYwho",min=input$firstYwho+5)
  })

  observeEvent(input$updateOptionTableOther,{
    updateActionButton(session, inputId="doRunDetailed", label = 'Push to update prediction',icon = icon("sync"))
  })

  observeEvent(input$updateOptionTable,{
    updateActionButton(session, inputId="doRunDetailed", label = 'Push to update prediction',icon = icon("sync"))
  })


  observeEvent(input$SMSarea,{
    #cat("area.change.no:",area.change.no,'\n')
    if (area.change.no>0) { # do not run it at the first time
      load_ecoRegion(input$SMSarea)

      selectedArea<<-input$SMSarea
      res$rv$out <- do_OP(readResSimple=TRUE,writeOption=doWriteOptions,source='SMS area change')
      res$rv$Fmulti<-rep(F_mult,n.fleet)
      res$rv$baseLine <-do_baseLine()

      # update simple prediction
      updateRadioButtons(inputId = 'bas_F_s',    choiceNames=bsF$Names,choiceValues = bsF$Values)
      updateRadioButtons(inputId = 'bas_Rec_s',  choiceNames=bsRec$Names,choiceValues = bsRec$Values)
      updateRadioButtons(inputId = 'bas_Yield_s',choiceNames=bsYield$Names,choiceValues = bsYield$Values)
      updateRadioButtons(inputId = 'bas_SSB_s',  choiceNames=bsSSB$Names,choiceValues = bsSSB$Values)
      updateCheckboxInput(inputId = "effcontrolAll", value = TRUE)

      updateSliderInput(inputId="F.all",value=1)
      purrr::walk(paste0(fleetNames), function(id) updateSliderInput(session, inputId=id, value = 1))


      # update detailed prediction
      updateSelectInput(inputId="sumSpecies",choices=VPA.spNames)
      updateSelectInput(inputId="exSpecies", choices=VPA.spNames)
      updateSelectInput(inputId="HCR.sp",choices=VPA.spNames)
      updateSelectInput(inputId="OtherSp",choices=other.spNames)

      updateSliderInput(inputId="yearFoodWeb",value=stq_year+1,min=fy_year_hist,max=termYear,step=1)
      updateSliderInput(inputId="OtherFirst", min = stq_year+1, max = termYear, value = stq_year+1, step=1)
      updateSliderInput(inputId="OtherSecond",min = stq_year+1, max = termYear, value = termYear, step=1)
      updateSliderInput(inputId="finalYear",  min = stq_year+1, max = stq_year+100, value = termYear, step=1)
      updateSliderInput(inputId="firstYwho",value=stq_year+1,min=fy_year_hist,max=termYear,step=1)
      updateSliderInput(inputId="lastYwho", value=termYear,min=fy_year_hist+5,max=termYear,step=1)
      updateSliderInput(inputId="firstY",value=stq_year+1,min=fy_year_hist,max=termYear,step=1)
      updateSliderInput(inputId="lastY",value=termYear,min=fy_year_hist+5,max=termYear,step=1)

      updateCheckboxGroupInput(inputId="foodWebSp",choices = pp_short$new,
                         selected = filter(pp_short, group %in% c("Other predators","VPA.pred","VPA.prey"))$new)

      } else area.change.no<<-area.change.no+1
  })


  observe({
    # simple predictions
   vals<-sapply(paste(ars,fleetNames,sep='_'), function(item) input[[item]])
   if (any(vals!=oldFvals)) {
      res$rv$Fmulti<-vals
      OP@output<<-20  # condensed output
      OP.trigger@Ftarget['init',]<<-vals*stqF
      res$rv$out<-do_OP(readResSimple=TRUE,writeOption=doWriteOptions,source='simple prediction')
      oldFvals<<-vals
   }

   #detailed predictions, change of terminal year
    if (input$finalYear != termYear){
       termYear<<-input$finalYear
       doWriteOptions<<-TRUE
       doRunModel<<-TRUE
       updateActionButton(session, inputId="doRunDetailed", label = 'Push to update prediction',icon = icon("sync"))
       OP@last.year<<-termYear
       OP.trigger@last.year<<-termYear
       updateSliderInput(session,inputId="lastY",max=input$finalYear)
       updateSliderInput(session,inputId="lastYwho",max=input$finalYear)
       updateSliderInput(inputId="OtherFirst",max=input$finalYear)
       updateSliderInput(inputId="OtherSecond",max=input$finalYear)

    }

   if (input$recDetSto != recruitMode) {
     doWriteOptions<<-TRUE
     doRunModel<<-TRUE
     updateActionButton(session, inputId="doRunDetailed", label = 'Push to update prediction',icon = icon("sync"))

     if (input$recDetSto=='Determenistic') {
       OP@stochastic.recruitment[1,]<<- rep(0,n.VPA)
       OP@recruit.adjust[1,]<<-hcr_ini$rec.adjust.single
       OP@recruit.adjust.CV[1,]<<- hcr_ini$rec.adjust.CV.single
       OP.trigger@HCR[1,]<<- 1

     } else if (input$recDetSto=='Stochastic') {
       OP@stochastic.recruitment[1,]<<- rep(1,n.VPA)
       OP@recruit.adjust.CV[1,]<<- rep(0,n.VPA)
       OP@recruit.adjust[1,]<<-hcr_ini$rec.adjust

       OP.trigger@HCR[1,]<<-hcr_ini$HCR
       OP.trigger@Ftarget['init',]<<-hcr_ini$Ftarget

       Foption_tab<<-get_op_Fmodel()
       updateFoption_single(input$HCR.sp)
     }
     recruitMode<<-input$recDetSto

   } #end observe



   if (input$Option>0)   updateRadioButtons(session=session,inputId = 'tabOpt',selected='Results')

   ###### change Baseline in plots
   baseline_handle<-function(h){
     v<-paste0(c("bas_F_","bas_SSB_","bas_Yield_","bas_Rec_"),h)
     vv<-sapply(v, function(item) input[[item]])
     if (vv[1] >"0") {
       if (vv[1]=="1") res$rv$baseLine[,'Fbar']<-stqF
       if (vv[1]=="2") res$rv$baseLine[,'Fbar']<-unlist(res$rv$out$a[,"Fbar"])
       updateRadioButtons(session=session,inputId = v[1],choiceNames=bsF$Names,choiceValues = bsF$Values)
     }

     if (vv[2] >"0")   {
       if (vv[2]=="1")  res$rv$baseLine[,'SSB']<-stqSSB
       if (vv[2]=="2")  res$rv$baseLine[,'SSB']<-unlist(res$rv$out$a[,"SSB"])
       updateRadioButtons(session=session,inputId = v[2],choiceNames=bsSSB$Names,choiceValues = bsSSB$Values)
     }
     if (vv[3] >"0")   {
       if (vv[3]=="1") res$rv$baseLine[,'Yield']<-stqYield
       if (vv[3]=="2") res$rv$baseLine[,'Yield']<-unlist(res$rv$out$a[,"Yield"])
       updateRadioButtons(session=session,inputId = v[3],choiceNames=bsYield$Names,choiceValues = bsYield$Values)
     }
     if (vv[4] >"0"   ) {
       if(vv[4]=="1") res$rv$baseLine[,'Recruits']<-stqRec
       if(vv[4]=="2") res$rv$baseLine[,'Recruits']<-unlist(res$rv$out$a[,"Recruits"])
       if(vv[4]=="3") res$rv$baseLine[,'Recruits']<-max_rec
       updateRadioButtons(session=session,inputId = v[4],choiceNames=bsRec$Names,choiceValues = bsRec$Values)
     }
   }


   if (input$simple_predict  =="Change Baseline") baseline_handle(h='s')   ###### change simple Baseline
   if (input$detailed_predict=="Change Baseline") baseline_handle(h='d')   ######  change detailed Baseline

  })


}

shinyApp(ui = ui, server = server)
