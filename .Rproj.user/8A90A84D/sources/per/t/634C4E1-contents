options(shiny.maxRequestSize=30*1024^2)
shinyServer(function(input, output,session) {
  
  output$jianli_name<-renderText({input$name})
  output$jianli_sex<-renderText({input$sex})
  output$jianli_date<-renderText({format(input$date)})
  output$jianli_university<-renderText({input$university})
  output$jianli_school<-renderText({input$school})
  output$jianli_major<-renderText({input$major})
  output$jianli_paiming<-renderText({input$paiming})
  output$jianli_number<-renderText({input$number})
  output$jianli_paimingdanwei<-renderText({input$paimingdanwei})
  
  output$jianli_baifenbi<-renderText({
    p<-as.numeric(input$paiming)
    n<-as.numeric(input$number)
    t<-p/n*100
    t<-round(t,2)
    paste(t,"%")
  })
  
  observe({
    paimingdanwei<-input$paimingdanwei
    updateTextInput(session, "paiming", label = paste(paimingdanwei,"排名"))
    updateTextInput(session, "number", label = paste(paimingdanwei,"人数"))
  })
  
  jiang_t<-reactive({
    JX %>%
      filter(Jiangxiang==input$jiangli) %>%
      select(Jiangxiangjibie)
  })
    
  output$jxjb<-renderUI({
    selectInput("jiang","级别：",choices=jiang_t()$Jiangxiangjibie)
  })


})