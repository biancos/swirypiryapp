dataoverall<-reactive({
  
  form<-stcs[[crf()]]
  var<-as.character(input$variables)
  pp<-form[,var]
  
  if(type()=="1")
  {
    sum<-NULL
  }
  
  else if(type()=="2")
  {
    sum<-NULL
  }
  
  
  
  else if(type()=="3"){ 
    valueall<- dplyr::recode(as.character(pp), 
                             .missing="Missing", 
                             'Unknown' = "Unknown",
                             'Global consent refused' = "Global consent refused",
                             'Refused' = "Refused",
                             'Answer Refused'  = "Refused",
                             'Not applicable' = "Not applicable",
                             'Not Done' = "Not Done"
    )
    
    back<-cbind(form[,c("patid","refcentre")],valueall)
    back2 <- data.frame(as.matrix( filter(back, valueall != "Global consent refused" & valueall != "Not applicable" )))
    back2$value<-back2$valueall
    
    sumcentre<- back2 %>%
      summarise(nobs=n())
    
    
    sumvalue<- back2 %>%
      group_by(value) %>%
      summarise(count_value=n())
    
    sum<- merge(sumvalue,sumcentre)
    print(sum)
    
  }
  
  else if(type()=="4"){
    valueall<-car::recode(pp,"c(-444,-555,-666,-777,-888,-999)= NA")
    back<-cbind(pp,valueall)
    sum<-cbind(back, form[,c("patid","refcentre")])
  }
  
  #still missed: set the colors to stay consistent with barplot missing values: try a recode + random color for other levels than missing specificated values
  else{sum<-NULL}
  print(head(sum,3))
  return(sum)
  
})


output$prova<-renderTable(if(type()=="3"){
  dataoverall()
}
else("other type of variable")
)


plotInPie<-reactive({
  
  if(type()=="3"){ 
    data.all<-dataoverall()
    data.all <- mutate(data.all, Percentage= round(count_value/nobs*100,2))
    data.all$valuefill<-paste(data.all$value,paste(data.all$Percentage,"%)",sep = ""),sep=" (")
    
    colcount<-length(unique(data.all$value))
    
    bp<- ggplot(data.all, aes(x="", y=count_value, fill=valuefill))+
      geom_bar(width = 1, stat = "identity", color='black')+
      coord_polar("y", start=0) + blank_theme +
      scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(colcount))+
      theme(axis.text.x=element_blank())+
      guides(fill=guide_legend(title="Values"))+
      ggtitle(paste(paste("variable",input$variables,sep = " "),"values pie chart",sep=" "))
  }
  
  else if(type()=="4"){
    data.all<-dataoverall()
    ggplot(data=subset(data.all,!is.na(valueall)), aes(x = refcentre, y = valueall)) + geom_boxplot() 
  }
  
  else{factor(0)}
  
  
  
})

output$pieplot<-renderPlot({print(plotInPie())})


  output$downloadPie <- downloadHandler(
    filename = function() { paste(input$variables, '.png', sep="") },
    content = function(file) {
      ggsave(file, plot = plotInPie(), device = "png",scale=1.8,width=12,height = 6)
    }
  )

