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
  
 
# #print selected inputs for crosschecks, remember to delete them once you finalize the code  
#    
#   output$current<-renderText({crf()})
#   output$selected_var<-renderText({input$variables})
#   output$type<-renderText( { print(type())})
# 
    output$titles<-renderText({
      t<-labels %>%
          filter(crf==!!toupper(crf()),variable==!!input$variables) %>%
          select(figure_title)
    as.character(t[1])
    })


    
    
    source(file.path("server", "overall.R"),  local = TRUE)$value  
    source(file.path("server", "bycentre.R"),  local = TRUE)$value  



})