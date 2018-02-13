source("./DataDef.R", local = TRUE)

# try to run it in shocase mode
#debug for aesthetic in browser mode

shinyServer(function(input, output,session) {
#update the inputs
  observe({
    updateSelectInput(session, "organs")
    updateSelectInput(session, "forms")
    updateSelectInput(session, "versions")
  })
  

#create a combination of the inputs that lead to the crf name    
  crf<-reactive({
    paste0(paste0(input$organs,input$forms,""),input$versions,"")
    })
  
#update the list of variables in the crf
  outVar<-reactive({
    form<-stcs[[crf()]]
    var0<-levels(unique(factor(names(form))))
    var1<-grep("_[1-9]",var0)
    var<-as.list(var0[-var1])
    return(var)
  })
  
  observe({
    updateSelectInput(session, "variables",
                      choices = outVar()   
    )})
  

#select the type of variable from the eCB  
  type<-reactive({t<-types %>%
                     filter(crf==!!toupper(crf()),variable==!!input$variables) %>% 
                     select(type)
                  return(as.character(t))
                  })
  
  dataoverall<-reactive({
    
    
    form<-stcs[[crf()]]
    var<-as.character(input$variables)
    pp<-form[,var]
    
    
    if(type()=="3"){ 
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
    
    else if(type=="1")
    {
      sum<-NULL
    }

    else if(type=="2")
    {
      sum<-NULL
    }
    
    #still missed: set the colors to stay consistent with barplot missing values: try a recode + random color for other levels than missing specificated values
    else{sum<-NULL}
    print(head(sum,3))
    return(sum)
    
  })
  
  databycentre<-reactive({
    
     form<-stcs[[crf()]]
     var<-as.character(input$variables)
     pp<-form[,var]
     
    if(type()=="2") {
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

    if(type()=="3") {
    
      
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
     
     if(type()=="4") {
       
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
  
#print selected inputs for crosschecks, remember to delete them once you finalize the code  
   
  output$current<-renderText({crf()})
  output$selected_var<-renderText({input$variables})
  output$type<-renderText( { print(type())})
  output$prova<-renderTable(if(type()=="3"){
                                dataoverall()
  }
  else("other type of variable")
                            )
  
    output$titles<-renderText({
      t<-labels %>% 
          filter(crf==!!toupper(crf()),variable==!!input$variables) %>%
          select(figure_title)
    as.character(t[1])
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

#
#   output$downloadPlot <- downloadHandler(
#     filename = function() { paste(input$variables, '.png', sep="") },
#     content = function(file) {
#       ggsave(file, plot = plotInput(), device = "png",scale=1.8,width=12,height = 6)
#     }
#   )
#
#   output$downloadPie <- downloadHandler(
#     filename = function() { paste(input$variables, '.png', sep="") },
#     content = function(file) {
#       ggsave(file, plot = plotInPie(), device = "png",scale=1.8,width=12,height = 6)
#     }
#   )


})