library(shiny)
library(visNetwork)
library(igraph)
library(zoo)
library(xts)

library(shinydashboard)
library(bubbles)
library(highcharter)
library(rCharts)
library(grDevices)
Sys.setlocale(category = "LC_ALL",locale = "CHS")
#setwd("C:/Users/IBM_ADMIN/Desktop/Projects/RSTUDY/cec/ceccorpinfo2")

createDF <- function(data){
  howmany <- length(unique(c(data[,2],data[,3])))
  groups=aggregate(data[,4],by=list(as.character(data[,3])),min)
  id <- 1:(howmany)
  idnames <- c(data[1,2],groups$Group.1)
  #idnames=paste0("<p><b>",idname1,"</b><br></p>")
  to=match(data[,3],idnames)
  from=match(data[,2],idnames)
  nodesdf <- data.frame(id=1:(length(idnames)), label=idnames,group=c(1,groups$x))
  linksdf <- data.frame(from, to)
  return(list(nodes=nodesdf,links=linksdf))
  #把原始数据变成两个df，一个存node信息 一个存link信息
}


mycolors <- colorRampPalette(c("lightgreen", "yellow", "orangered"))(100)
mycolors2 <- colorRampPalette(c("blue", "white", "lightgreen", "lightyellow", "orangered"))(100)
#setwd("C:/Users/IBM_ADMIN/Desktop/Projects/RSTUDY/cec/ceccorpinfo2")


corpinfo=read.csv("./data/corp_info.csv",header=T,sep=",",encoding="UFT-8",stringsAsFactors = F)
#corpcor=read.csv(paste("try1.csv",sep=""),header=T,sep=",",encoding="UFT-8",stringsAsFactors = F)

emonos76=read.csv("./data/news_all1.csv",header=T,sep=",",encoding="UFT-8",stringsAsFactors = F)
news_time=strptime(as.character(emonos76$REPORT_TIME),"%Y-%m-%d")
score=emonos76$score
side=emonos76$side
score[is.na(score)]=0
side[is.na(side)]=0
side[side==2]=-1
emo_score=as.numeric(side)*(as.numeric(score))

server<-function(input, output) {
  
  output$selectcorp <- renderUI({
    selectInput("corpname", "选择企业", corpinfo[1:30,2])
  })
  
  
  output$newscount <- renderValueBox({
    a=(as.Date(max(news_time))-input$datecount)
    news_count=length(which(as.Date(news_time)>a))
    valueBox(
      value =news_count,
      subtitle = "总信息量",
      icon = icon("comments-o")
    )
  })
  output$newscount1 <- renderValueBox({
    a=(as.Date(max(news_time))-input$datecount)
    corp_count=length(unique(emonos76[which(as.Date(news_time)>a),"NEWS_SOURCE"]))
    valueBox(
      value =corp_count,
      subtitle =  "信息更新企业数量",
      icon = icon("users")
    )
  })
  
  output$newscount2 <- renderValueBox({
    a=(as.Date(max(news_time))-input$datecount)
    risk_count=length(which(emonos76[which(as.Date(news_time)>a),"side"]==2))
    valueBox(
      value = risk_count,
      subtitle = "风险信息",
      icon = icon("bell-o"),
      color = if (risk_count >= input$rateThreshold) "yellow" else "aqua"
    )
  })
  
  
  
  
  output$key_plot<- renderBubbles({
    a=(as.Date(max(news_time))-input$datecount)
    aa=which(as.Date(news_time)>a)
    att=unlist(lapply(as.character(emonos76$news_seg[aa]), function(x) strsplit(x,split=",")[[1]]))
    attta=table(att)
    top10=order(attta,decreasing = TRUE)[1:10]
    bubbles(attta[top10], names(attta)[top10], color = rainbow(10, alpha = NULL))
  })
  
  output$corp_plot <- renderPlot({
    
    a=(as.Date(max(news_time))-input$datecount)
    aa=which(as.Date(news_time)>a)
    side=emonos76[aa,"side"]
    side[is.na(side)]=0
    x=tapply(emonos76[aa,1], list(side,as.character(emonos76[aa,"NEWS_SOURCE"])), length)
    x[is.na(x)]=0
    #barplot(x[,order(colSums(x),decreasing = TRUE)[1:5]],horiz=T,col=c("lightgreen","lightblue","red"),width = c(2,2,2,2,2),legend=c("中性","正面","负面"))
    z=x[,order(colSums(x),decreasing = TRUE)[1:5]]
    zt=z[,order(colSums(z))]
    xplot<- barplot(zt,horiz=T,col=c("lightgreen","lightblue","red"),legend=c("中性","正面","负面"),args.legend=list(x="bottomright"),axisnames = FALSE)
    lbls<-paste(" ",a)
    text(xplot,colnames(zt),labels=lbls,cex=1,pos=1)
    
  })
  
  
  output$allnews234 <-renderDataTable({
    a=(as.Date(max(news_time))-input$datecount)
    aa=which(as.Date(news_time)>a)
    a=cbind(as.character(emonos76$TITLE[aa]),as.character(emonos76$author_new.NEWS_AUTHOR[aa]),as.character(emonos76$REPORT_TIME[aa]))
    colnames(a)=c("新闻","来源","时间")
    a=a[order(a[,3],decreasing = TRUE),]
    data.frame(a)
  },options = list(pageLength = 5)
  )
  
  
  data=read.csv(paste("./data/try1.csv",sep=""),header=T,sep=",",encoding="UFT-8",stringsAsFactors = F)
  
  
  output$visNetworkOutput2 <- renderVisNetwork({
    # minimal example
    # data=read.csv(paste("./Data/",input$corpname,"1.csv",sep=""),header=T,sep=",",encoding="UFT-8",stringsAsFactors = F)
    #data=corpcor
    circleOutput<-createDF(data)
    nodes <-circleOutput$nodes
    edges<-circleOutput$links
    visNetwork(nodes,edges) %>%
      visGroups(groupname = "1",  color = mycolors[90],size=40) %>%
      visGroups(groupname = "2",  color = mycolors[60],size=30) %>%
      visGroups(groupname = "3",  color = mycolors[30],size=30) %>%
      visGroups(groupname = "4",  color = mycolors[10],size=30) %>%
      visGroups(groupname = "5",  color = mycolors2[80],size=30) %>%
      visGroups(groupname = "6",  color = mycolors2[40],size=30) %>%
      visGroups(groupname = "7",  color =mycolors2[10],size=30) %>%
      visGroups(groupname = "21",  color = mycolors[50],size=20) %>%
      visGroups(groupname = "31",  color = mycolors[20],size=20) %>%
      visGroups(groupname = "41",  color = mycolors[5],size=20) %>%
      visGroups(groupname = "51",  color = mycolors2[60],size=20) %>%
      visGroups(groupname = "61",  color = mycolors2[50],size=20) %>%
      visGroups(groupname = "71",  color =mycolors2[20],size=20) %>%
      visExport() %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = list(enabled = TRUE, selected = "1"))
  })
  
  
  
  output$emotionsplot <- renderChart2({
    
    aa=which(as.character(emonos76$NEWS_SOURCE)==input$corpname)
    #x=aggregate(emo_score[aa],by=list(as.character(news_time[aa])),summary)
    summ0=function(x){ifelse(min(x,0)<0,min(x,0),max(x,0))}
    x=aggregate(emo_score[aa],by=list(as.character(news_time[aa])),summ0)
    candlecolors <- ifelse(x[,2] < 0, '#7cb5ec', '#434348')
    x[,1]=as.POSIXlt(x[,1])$year+1900
    h1 <- Highcharts$new()
    h1$chart(type = "spline")
    h1$series(name = '媒体情绪',data = c(x[,2]),color='#f7a35c')
    h1$xAxis(categories=x[,1],labels=list(rotation=-45,step= 30))
    h1$params$width=400
    h1$params$height=350
    return(h1)
    
  })
  
  
  
  output$trendplot <-renderChart2({
    aa=which(as.character(emonos76$NEWS_SOURCE)==input$corpname)
    x=aggregate(aa,by=list(as.character(news_time[aa])),length)
    #summ0=function(x){summary(c(x,0))}
    #xa=aggregate(emo_score[aa],by=list(as.character(news_time[aa])),summ0)
    x[,1]=as.POSIXlt(x[,1])$year+1900
    h1 <- Highcharts$new()
    h1$chart(type = "spline")
    h1$series(name = '媒体关注度',data = c(x[,2]))
    h1$xAxis(categories=x[,1],labels=list(rotation=-45,step= 30))
    h1$params$width=400
    h1$params$height=350
    
    return(h1)
  })
  
  
  
  
  
  output$newsemotions <-renderDataTable({
    aa=which(as.character(emonos76$NEWS_SOURCE)==input$corpname)
    a=cbind(as.character(emonos76$TITLE[aa]),as.character(emonos76$author_new.NEWS_AUTHOR[aa]),as.character(emonos76$REPORT_TIME[aa]))
    colnames(a)=c("新闻","来源","时间")
    a=a[order(a[,3],decreasing = TRUE),]
    data.frame(a)
  },options = list(pageLength = 5)
  )
  
  output$newskeywordplot<- renderBubbles({
    aa=which(as.character(emonos76$NEWS_SOURCE)==input$corpname)
    att=unlist(lapply(as.character(emonos76$news_seg[aa]), function(x) strsplit(x,split=",")[[1]]))
    attta=table(att)
    top10=order(attta,decreasing = TRUE)[1:10]
    bubbles(attta[top10], names(attta)[top10], color = rainbow(10, alpha = NULL))
  })
  
  
  
  output$view_id <- renderTable({
    #paste("Current node selection : ", input$visNetworkOutput2_selected)
    if(input$visNetworkOutput2_selected==1)
    {info=corpinfo$corpinfo[which(corpinfo$name==input$corpname)]
    info=strsplit(info,split="&&")[[1]]
    infom=sapply(info, function(x) strsplit(x,split=":")[[1]])
    return(t(infom))}
    else{return(input$visNetworkOutput2_selected)}
    
  },rownames = FALSE, colnames = FALSE,bordered = FALSE)
  
  
}




