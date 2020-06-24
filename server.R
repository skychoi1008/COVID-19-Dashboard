server = function(input, output, session) {
  
  inputdata1 <- reactive({
    thedate <- input$inputdate
    thedate <- as.Date(thedate,origin = "1970-01-01")
    temp1 <- subset(mydata,mydata$`Country/Region` %in% input$inputcountry)
    temp1 <- subset(temp1,temp1$Date<=thedate)
    temp1
  })

  #line chart of comfirmed, recovered, deaths data 
  output$plot1 <- renderPlot({
    
    plotdata1 <- inputdata1()
    plotdata1 <- plotdata1%>%group_by(`Country/Region`,Date)%>%
      summarise(Confirmed=sum(Confirmed,na.rm = TRUE),
                Recovered=sum(Recovered,na.rm = TRUE),
                Deaths=sum(Deaths,na.rm = TRUE))
    
    plotdata2 <- inputdata4()
    plotdata2 <- plotdata2%>%group_by(`Country/Region`,Date)%>%
      summarise(Confirmed=sum(Confirmed,na.rm = TRUE),
                Recovered=sum(Recovered,na.rm = TRUE),
                Deaths=sum(Deaths,na.rm = TRUE))
    
    if (is.null(input$inputcountry)) {
      plotdata <- plotdata2
    }else{
      plotdata <- plotdata1
    }
    
    if (input$inputstatus=="Confirmed") {
      ggplot(plotdata,aes(Date,Confirmed,color=`Country/Region`))+
        geom_line()
    }else if(input$inputstatus=="Recovered") {
      ggplot(plotdata,aes(Date,Recovered,color=`Country/Region`))+
        geom_line()
    }else{
      ggplot(plotdata,aes(Date,Deaths,color=`Country/Region`))+
        geom_line()
    }
  })
  
  #mapping of the comfirmed, recovered, deaths data 
  output$plot3 <- renderLeaflet({
    thedata <- inputdata5()
    
    if (input$inputstatus=="Confirmed") {
      leaflet() %>% addTiles() %>%
        addCircleMarkers(lng=thedata$Long,lat =thedata$Lat,
                         radius=(thedata$Confirmed)/max(thedata$Confirmed)*20,
                         color = "red",
                         popup = paste0("Country:",thedata$`Country/Region`,"<br>",
                                        "Confirmed:",thedata$Confirmed,"<br>",
                                        "Date:",thedata$Date))
    }else if(input$inputstatus=="Recovered") {
      leaflet() %>% addTiles() %>%
        addCircleMarkers(lng=thedata$Long,lat =thedata$Lat,
                         radius=(thedata$Recovered)/max(thedata$Recovered)*20,
                         color = "red",
                         popup = paste0("Country:",thedata$`Country/Region`,"<br>",
                                        "Recovered:",thedata$Recovered,"<br>",
                                        "Date:",thedata$Date))
    }else{
      leaflet() %>% addTiles() %>%
        addCircleMarkers(lng=thedata$Long,lat =thedata$Lat,
                         radius=(thedata$Deaths)/max(thedata$Deaths)*20,
                         color = "red",
                         popup = paste0("Country:",thedata$`Country/Region`,"<br>",
                                        "Deaths:",thedata$Deaths,"<br>",
                                        "Date:",thedata$Date))
    }
  })
  
  
  inputdata2 <- reactive({
    thedate <- input$inputdate
    thedate <- as.Date(thedate,origin = "1970-01-01")
    temp1 <- subset(mydata,mydata$`Country/Region` %in% input$inputcountry)
    temp1 <- subset(temp1,temp1$Date==thedate)
    temp1
  })
  
  inputdata3 <- reactive({
    thedate <- input$inputdate
    thedate <- as.Date(thedate,origin = "1970-01-01")
    temp1 <- subset(mydata,mydata$Date==thedate)
    temp1$Date <- as.character(temp1$Date)
    temp1 <- temp1%>%group_by(`Country/Region`,Date)%>%
      summarise(Confirmed=sum(Confirmed,na.rm = TRUE),
                Recovered=sum(Recovered,na.rm = TRUE),
                Deaths=sum(Deaths,na.rm = TRUE))
    temp1 <- temp1[order(temp1$Confirmed,decreasing = TRUE),]
    temp1[c(1:12),]
  })
  
  inputdata4 <- reactive({
    thedata <- inputdata3()
    thedate <- input$inputdate
    thedate <- as.Date(thedate,origin = "1970-01-01")
    temp1 <- subset(mydata,mydata$`Country/Region` %in% thedata$`Country/Region`)
    temp1 <- subset(temp1,temp1$Date<=thedate)
    temp1
  })
  
  
  inputdata5 <- reactive({
    thedate <- input$inputdate
    thedate <- as.Date(thedate,origin = "1970-01-01")
    temp1 <- subset(mydata,mydata$Date==thedate)
    temp1$Date <- as.character(temp1$Date)
    temp1 <- temp1%>%group_by(`Country/Region`,Date)%>%
      summarise(Confirmed=sum(Confirmed,na.rm = TRUE),
                Recovered=sum(Recovered,na.rm = TRUE),
                Deaths=sum(Deaths,na.rm = TRUE),
                Lat=mean(Lat),
                Long=mean(Long))
    temp1
  })
  
  inputdata6 <- reactive({
    thedata <- inputdata3()
    thedate <- input$inputdate
    thedate <- as.Date(thedate,origin = "1970-01-01")
    temp1 <- subset(mydata,mydata$`Country/Region` %in% thedata$`Country/Region`)
    temp1 <- subset(temp1,temp1$Date==thedate)
    temp1
  })
  
  #bar chart of the comfirmed, recovered, deaths data
  output$plot2 <- renderPlot({
    thedata1 <- inputdata2()
    thedata1$Date <- as.character(thedata1$Date)
    thedata1 <- thedata1%>%group_by(`Country/Region`)%>%
      summarise(Confirmed=sum(Confirmed,na.rm = TRUE),
                Recovered=sum(Recovered,na.rm = TRUE),
                Deaths=sum(Deaths,na.rm = TRUE))
    
    thedata2 <- inputdata3()
    thedata2$Date <- as.character(thedata2$Date)
    thedata2 <- thedata2%>%group_by(`Country/Region`)%>%
      summarise(Confirmed=sum(Confirmed,na.rm = TRUE),
                Recovered=sum(Recovered,na.rm = TRUE),
                Deaths=sum(Deaths,na.rm = TRUE))    
    
    if (is.null(input$inputcountry)) {
      thedata <- thedata2
    }else{
      thedata <- thedata1
    }
    
    thedata <- thedata %>% gather(key,value,-`Country/Region`)
    ggplot(thedata,aes(`Country/Region`,value,fill=key))+geom_bar(stat = 'identity', position='dodge')
  })
  
  #bar chart of recovered rate ranking
  output$plot4 <- renderPlot({
    thedata <- inputdata6()
    thedata$Date <- as.character(thedata$Date)
    thedata <- thedata%>%group_by(`Country/Region`)%>%
      summarise(Recovered_Rate=sum(Recovered,na.rm = TRUE)/sum(Confirmed,na.rm = TRUE))

    thedata <- thedata %>% gather(key,value,-`Country/Region`)
    ggplot(thedata,aes(reorder(`Country/Region`, -value),value))+geom_bar(stat = 'identity', position='dodge')+
      ylab("Recovered Rate")+coord_flip()
  })
  
  #bar chart of deaths rate ranking
  output$plot5 <- renderPlot({
    thedata <- inputdata6()
    thedata$Date <- as.character(thedata$Date)
    thedata <- thedata%>%group_by(`Country/Region`)%>%
      summarise(Recovered_Rate=sum(Deaths,na.rm = TRUE)/sum(Confirmed,na.rm = TRUE))
    
    thedata <- thedata %>% gather(key,value,-`Country/Region`)
    ggplot(thedata,aes(reorder(`Country/Region`, -value),value))+geom_bar(stat = 'identity', position='dodge')+
      ylab("Deaths Rate")+coord_flip()
  })
  
  
  choose_day <- reactive({
    thedate <- input$inputdate
    thedate <- as.Date(thedate,origin = "1970-01-01")
    temp1 <- subset(mydata,mydata$Date==thedate)
    temp1 <- temp1%>%group_by(Date)%>%
      summarise(Confirmed=sum(Confirmed,na.rm = TRUE),
                Recovered=sum(Recovered,na.rm = TRUE),
                Deaths=sum(Deaths,na.rm = TRUE))
    temp1
  })

  choose_last <- reactive({
    thedate <- input$inputdate
    thedate <- as.Date(thedate,origin = "1970-01-01")
    temp1 <- subset(mydata,mydata$Date==(thedate-1))
    temp1 <- temp1%>%group_by(Date)%>%
      summarise(Confirmed=sum(Confirmed,na.rm = TRUE),
                Recovered=sum(Recovered,na.rm = TRUE),
                Deaths=sum(Deaths,na.rm = TRUE))
    temp1
  })
  
  
  # 9 valueboxes
  output$Total_Confirmed <- renderValueBox({

    valueBox(
      value = choose_day()$Confirmed,
      subtitle = "Total Confirmed",
      icon = icon("ambulance"),
      color = "red"
    )
  })

  output$Total_Recovered <- renderValueBox({

    valueBox(
      value = choose_day()$Recovered,
      subtitle = "Total Recovered",
      icon = icon("user-check"),
      color = "green"
    )
  })

  output$Total_Deaths <- renderValueBox({

    valueBox(
      choose_day()$Deaths,
      "Total Deaths",
      icon = icon("frown"),
      color = "light-blue"
    )
  })

  output$New_Confirmed <- renderValueBox({

    valueBox(
      value = choose_day()$Confirmed-choose_last()$Confirmed,
      subtitle = "New Confirmed",
      icon = icon("ambulance"),
      color = "red"
    )
  })

  output$New_Recovered <- renderValueBox({

    valueBox(
      value = choose_day()$Recovered-choose_last()$Recovered,
      subtitle = "New Recovered",
      icon = icon("user-check"),
      color = "green"
    )
  })

  output$New_Deaths <- renderValueBox({

    valueBox(
      choose_day()$Deaths-choose_last()$Deaths,
      "New Deaths",
      icon = icon("frown"),
      color = "light-blue"
    )
  })
  
  output$Existing_Confirmed <- renderValueBox({
    
    valueBox(
      value = choose_day()$Confirmed-choose_day()$Recovered-choose_day()$Deaths,
      subtitle = "Existing Confirmed",
      icon = icon("ambulance"),
      color = "red"
    )
  })
  
  output$Recovered_Rate <- renderValueBox({
    
    valueBox(
      value = round(choose_day()$Recovered/choose_day()$Confirmed,3)*100,
      subtitle = "Recovered Rate(%)",
      icon = icon("percent"),
      color = "teal"
    )
  })
  
  output$Deaths_Rate <- renderValueBox({
    
    valueBox(
      value = round(choose_day()$Deaths/choose_day()$Confirmed,3)*100,
      subtitle = "Deaths_Rate(%)",
      icon = icon("percent"),
      color = "teal"
    )
  })
  
  output$packageTable <- renderTable({
    thedata <- inputdata3()
    thedata
  }, digits = 1)
  
}
