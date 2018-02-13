databycentre<-reactive({
  
  form<-stcs[[crf()]]
  var<-as.character(input$variables)
  pp<-form[,var]
  
  if(type()=="1"){
    value<-dplyr::recode(as.character(pp), 
                         .default="Entered",
                         .missing="Missing", 
                         'Unknown' = "Unknown",
                         'Global consent refused' = "Global consent refused",
                         'Refused' = "Refused",
                         'Answer Refused'  = "Refused",
                         'Not applicable' = "Not applicable",
                         'Not Done' = "Not Done"
    )
  } 
  
  else if(type()=="2") {
    value<-dplyr::recode(as.character(pp), 
                         .default="Entered",
                         .missing="Missing", 
                         '7776-12-31' = "Unknown",
                         '5554-12-31' = "Global consent refused",
                         '8887-12-31' = "Refused",
                         '9998-12-31' = "Not applicable",
                         '6665-12-31' = "Not Done"
    )
  }
  
  else if(type()=="3") {
    
    
    value<-dplyr::recode(as.character(pp), 
                         .default="Entered",
                         .missing="Missing", 
                         'Unknown' = "Unknown",
                         'Global consent refused' = "Global consent refused",
                         'Refused' = "Refused",
                         'Answer Refused'  = "Refused",
                         'Not applicable' = "Not applicable",
                         'Not Done' = "Not Done"
    )
  }
  
  else if(type()=="4") {
    
    value<-dplyr::recode(as.character(pp), 
                         .default="Entered",
                         .missing="Missing", 
                         '-777' = "Unknown",
                         '-555' = "Global consent refused",
                         '-888' = "Refused",
                         '-999' = "Not applicable",
                         '-666' = "Not Done"
    )
  }
  
  
  back<-cbind(pp,value)
  back2<-cbind(back, form[,c("patid","refcentre")])
  back2 <- data.frame(as.matrix( filter(back2, value != "Global consent refused" & value != "Not applicable" )))
  back2$value<-factor(back2$value, levels = c("Entered","Missing","Not Done","Refused","Unknown"), ordered = F)
  
  sumcentre<- back2 %>%
    group_by(refcentre) %>%
    summarise(nobs=n())
  
  
  sumvalue<- back2 %>%
    group_by(refcentre, value) %>%
    summarise(count_value=n())
  
  
  sum<- merge(sumvalue,sumcentre)
  
  
  
  data.a <- mutate(sum, Percentage= count_value/nobs*100)
  data.a <- rename(data.a,centreid=refcentre)
  data.a<-complete_(data.a,c("centreid","value"),fill = list(nobs=0, count_value = 0, Percentage=0))
  
  data.b <- labels %>% 
    filter(crf==!!toupper(crf()),variable==!!input$variables)
  data.b<-data.frame(variable=rep(data.b[1,"variable"],dim(data.a)[1]),
                     figure_title=rep(data.b[1,"figure_title"],dim(data.a)[1]),
                     crf=rep(data.b[1,"crf"],dim(data.a)[1]) )
  data.m<-cbind(data.a,data.b)
  
  data.m$colors<-dplyr::recode(as.character(data.m$value),
                               'Entered' = "lightblue3",
                               'Missing' = "lightcoral",
                               'Not Done' = "lightgrey",
                               'Refused' = "mistyrose1",
                               'Unknown' = "palegoldenrod"
  )
  
  return(data.m)
  
})



plotInput<-reactive({
  
  data.m<-databycentre()
  
  MainTitle<-ggtitle(paste( paste("Entered vs Missing records barplot. ",paste("Variable", input$variables,sep = " "),sep=""),
                            paste("Total records",sum(data.m$count_value),sep=":"),sep=", ")
  )
  
  
  fillcat<-levels(factor(data.m$value))
  fillcolor<-levels(factor(data.m$colors))
  
  myColors <- as.character(c(fillcat=fillcolor))
  
  colScale <- scale_fill_manual(name="value",values=myColors)
  
  ggplot(data.m, aes(centreid, Percentage,fill=value,width=0.75)) +
    geom_bar(position = position_dodge(width = 0.9), stat="identity", color="black",size=0.2)+
    colScale +
    MainTitle +
    geom_text(data=data.m,aes(label=count_value,group=value),
              position = position_dodge(0.9), vjust = -0.5)+
    ylim(0,max(data.m$Percentage+8)) +
    theme_bw()+
    theme(
      plot.title = element_text(color="black", size=14, face="bold"),
      axis.title=element_text(size=14,face="bold"))+
    guides(fill=guide_legend(title=NULL))
  
})

output$missplot<-renderPlot({print(plotInput())})



  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$variables, '.png', sep="") },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png",scale=1.8,width=12,height = 6)
    }
  )