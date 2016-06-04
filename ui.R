library(visNetwork)
library(igraph)
library(shinydashboard)
library(bubbles)
library(zoo)
library(xts)
library(highcharter)
library(rCharts)
library(grDevices)
#setwd("C:/Users/IBM_ADMIN/Desktop/Projects/RSTUDY/cec/ceccorpinfo2")
#corpinfo=read.csv("./Data/corp_info.csv",header=T,sep=",",encoding="UFT-8",stringsAsFactors = F)
Sys.setlocale(category = "LC_ALL",locale = "CHS")
#corpinfo=read.csv("datacor.csv",header=T,sep=",",encoding="UFT-8",stringsAsFactors = F)


ui <- dashboardPage(
  dashboardHeader(title = "企业大数据分析平台"),
  dashboardSidebar(

    sidebarMenu(
      id = "tabs",
      menuItem("企业信息监控", tabName = "widgets0_zq", icon = icon("th")),
      menuItem("企业网络视图", tabName = "widgets1_zq", icon = icon("th")),
      menuItem("企业舆情分析", tabName = "widgets2_zq", icon = icon("th"))
    ),
    #selectInput("corpname", "选择企业", choices =corpinfo[1:30,2])
    uiOutput("selectcorp")

  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "widgets0_zq",
              fluidRow( box(
                width = 6,
                sliderInput("datecount","选择时间段",
                            min = 0, max = 30, value = 3, step = 1
                )),
                box(
                  width = 6, sliderInput("rateThreshold", "告警数量",
                                         min = 0, max = 50, value = 3, step = 1)
                )),
              fluidRow(
                #valueBoxOutput("riskrate"),
                valueBoxOutput("newscount2"),
                valueBoxOutput("newscount"),
                valueBoxOutput("newscount1")
                #valueBoxOutput("newscorp")
              ),
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "舆情热点",
                  bubblesOutput("key_plot", width = "100%", height = 450)
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "企业舆情Top5",
                  plotOutput("corp_plot", width = "100%", height = 450)
                )
              ),
              fluidRow(
                box(
                  width = 12, dataTableOutput("allnews234"))
              )
      ),
      tabItem(tabName = "widgets1_zq",
              h2("企业网络视图"),
              fluidRow(
                column(width =12,


                       fluidRow(  column(
                         width = 3,
                         hr(),
                         hr(),
                         #renderText("公司信息"),
                         tableOutput("view_id")
                       ),
                       column(width=9,
                              visNetworkOutput("visNetworkOutput2",height = "800px"))
                       #tabpanel2
                       #sliderInput("circleScale", "担保圈的长度 : ", min = 2, max = 5, value = 2)
                       )
                )
              )),


      tabItem(tabName = "widgets2_zq",
              h2("舆情分析"),

              fluidRow( column(width = 12,
                               fluidRow(  column(
                                 width =4,
                                 #renderText("公司信息"),
                                 box(width = NULL ,  bubblesOutput('newskeywordplot',width = "100%", height = 350))
                               ),
                               column(
                                 width =4,
                                 #renderText("公司信息"),
                                 box(width = NULL,    showOutput('trendplot',"Highcharts"))
                               ),
                               column(width=4,
                                      box(width = NULL,   showOutput('emotionsplot',"Highcharts"))
                               )),
                               fluidRow(  column(
                                 width =12, box(width = NULL,dataTableOutput("newsemotions"))))
              )))




    )    )
)
