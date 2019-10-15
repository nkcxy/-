library(shiny)
library(ggplot2)
shinyApp(
  
  ui = fixedPage(
    
    fixedPanel(
      
      top = 50, right=50, width=200, draggable = TRUE, style="padding: 20px; border: 1px solid red;",
      
      "可以移动的框框1"
      
    ),
    
    absolutePanel(
      
      top = 150, right=150, width=200, draggable = TRUE, style="padding: 20px; border: 1px solid red;",
      
      "可以移动的框框2"
      
    )
    
  ),
  
  server = function(session, input, output) {
    
  })



###############################
shinyApp(
  ui = fixedPage(
    tags$head(
      tags$title('南开大学优秀学生夏令营报名表'),
      tags$style(
        rel = 'stylesheet',
        '.title-panel {background: #ABCDEF} ',
        '.title-panel h2 {text-align:center; color:black}'
      )
    ),
    
    div(
      class='col-md-12 title-panel',
      h2('南开大学优秀学生夏令营报名表')
    ),
    
    
    tags$style(
      ".container div {border: 1px solid gray; min-height:30px;}",
      "h4 {color:red; margin-top: 20px;}"
    ),
    
    h4("两栏模板"),
    
    sidebarLayout(
      sidebarPanel("side bar panel"),
      mainPanel(splitLayout("aaaa", "bbbb", "cccc", "dddd"))
    ),
    
    h4("垂直分割模板"),
    splitLayout("aaaa", "bbbb", "cccc", "dddd"),
    
    h4("垂直排列模板"),
    verticalLayout("aaaa", "bbbb", "cccc", "dddd"),
    
    h4("流式（自动折行）模板"),
    flowLayout("aaaa", "bbbb", "cccc", "dddd")
  ),
  
  server = function(input, output, session) {}
)




#########################################
shinyApp(
  
  ui = fixedPage(
    textInput('itx1', '', value='1111'),
    textInput('itx2', '', value='2222'),
    textOutput('otx', container=pre)
  ),
  
  server = function(input, output, session) {
    output$otx <- renderPrint({
      a <- NULL
      isolate(a <- input$itx1)
      b <- input$itx2
      list(a=a, b=b)
    })
  })

##################################
shinyApp(
  
  ui = fixedPage(
    h1('测试'), hr(),
    
    radioButtons('opts', '', choices = c('图像', '文字'), inline = T, selected='图像'),
    
    conditionalPanel(
      condition = 'input.opts==="图像"',
      plotOutput('pl')
    ),
    
    conditionalPanel(
      condition = 'input.opts==="文字"',
      textOutput('tx', container=pre)
    )
  ),
  
  server = function(input, output, session) {
    air <- na.omit(airquality)
    pp <- ggplot(air, aes(x=Solar.R, y=Ozone)) + geom_point()
    observe({
      xtype <- input$opts
      if(xtype=='图像') output$pl <- renderPlot({ pp })
      else output$tx <- renderPrint({ str(pp) })
    })
  })


##########################
shinyApp(
  ui = fixedPage(
    fileInput('f', '上传文件', multi=T,accept=c(".jpg",".jpeg")),
    textOutput('tx', container=pre),
    imageOutput("pplot")
  ),
  
  server = function(input, output, session) {
    output$tx <- renderPrint({ str(input$f) })
    output$pplot<-renderImage({input$f})
  }
)


#############################
fig.w <- 400
fig.h <- 300
shinyApp(
  ui = fixedPage(
    plotOutput('pl', width=fig.w, height=fig.h),
    radioButtons('xtype', '图片格式', c('png', 'jpeg', 'bmp'), selected='png', inline=T),
    downloadLink('file', '保存图片')
  ),
  
  server = function(input, output, session) {
    air <- na.omit(airquality)
    pp <- ggplot(air, aes(x=Solar.R, y=Ozone)) + geom_point()
    output$pl <- renderPlot({ pp })
    observeEvent(
      input$xtype,
      output$file <- downloadHandler(
        filename = paste0('plot.', input$xtype),
        content = function(file) {
          image <- switch(input$xtype,
                          png=png, jpeg=jpeg, bmp=bmp)

          image(file, width=fig.w, height=fig.h)
          print(pp)
          dev.off()
        }
      )
    )
  })


########################################################
shinyApp(
  ui = fixedPage(
    h2('输入控件演示'),
    hr(),
    sidebarLayout(
      sidebarPanel(
        textInput('tx', '文字输入', value='abc'),
        checkboxGroupInput('cg', '选项组', choices=LETTERS[1:4], selected=c('A', 'D'), inline=TRUE),
        sliderInput('sl', '滑动选数', min=1, max=10, value=6),
        
        HTML('<label for="tt">文本框输入</label>',
              
              '<textarea id="tt" class="form-control" style="resize:none"></textarea>'
              
        ),
        
        HTML('<label for="clx">颜色选取</label>',
              '<input id="clx" type="color" class="form-control" value="#FF0000">',
              '<input id="cl" type="text" class="form-control" value="#FF0000" style="display:none">',
              '<script>',
              '$(function(){$("#clx").change(function(){$("#cl").val($(this).val()).trigger("change");});})',
              '</script>'
        )
      ),
      
      mainPanel(
        HTML('<textarea id="ta" class="form-control shiny-text-output"',
              'style="resize:none; height:200px;" readonly></textarea>'
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    
    output$ta <- renderText({
      paste(c(input$tx, input$tt, paste(input$cg, collapse=';'),
              input$sl, input$cl), collapse='\n')
    })
    
    observe({
      updateTextInput(session, inputId='tt', value=paste('文本输入：', input$tx))
    })
  })

###############################
shinyApp(
  ui = fixedPage(
    textOutput('tx', container=h1),
    plotOutput('pl', width='100%', height='400px')
  ),
  
  server = function(input, output, session) {
    output$tx <- renderText({
      "这是服务器输出的一些文字"
    })
    
    output$pl <- renderPlot({
      a <- rnorm(20)
      par(mar=c(3, 3, 0.5, 0.5), mgp=c(2, 0.5, 0))
      plot(a)
    })
  })


###############################
library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(plotly)

run.sql <- function(sql, debug=FALSE) {
  if(debug==FALSE){
    df <- XXXXX # 自行定义函数，根据数据存储位置，执行SQL语句
  }
  else{
    # 测试数据
    group_id <- rep(1, nrow(economics))
    dt <- paste(as.character(economics$date), "00:00:00")
    df <- cbind(group_id, dt, economics)
  }
  return(df)
}

ui <- fluidPage(
  useShinyjs(),
  titlePanel("时间序列数据可视化工具"),
  # 第一部分：SQL命令提交界面
  div(id="download",
      fluidRow(
        column(12,
               textOutput(outputId="download_info")
        )
      ),
      fluidRow(
        column(12, 
               HTML(
                 paste('<textarea id="sql_cmd" rows="10", cols="180">', 
                       "select * from xxxx limit 1000;",         
                       '</textarea>')
               )
        )
      ),
      fluidRow(
        column(12,
               actionButton(inputId="refresh_button", label="加载数据", icon=icon("submit")
               )
        )
      )
  ),
  
  
  shinyjs::hidden(
    div(id="table",
        # 第二部分：SQL命令执行结果显示
        hr(),
        dataTableOutput(outputId="sql_tab"),
        
        # 第三部分：可视化规则设置
        hr(),
        textOutput(outputId="tab_button_message"),
        sidebarLayout(
          div(id="table_tool",
              sidebarPanel(
                selectInput(inputId="group_fields", label="绘图分组字段", choices=NULL, selected=NULL, multiple=TRUE),
                selectInput(inputId="x_field", label="设置x轴字段，必须是日期时间", choices=NULL, selected=NULL, multiple=FALSE),
                selectInput(inputId="y_line_fields", label="设置y轴线图字段", choices=NULL, selected=NULL, multiple=TRUE),
                selectInput(inputId="y_point_fields", label="设置y轴点图字段", choices=NULL, selected=NULL, multiple=TRUE),
                selectInput(inputId="group_shape_field", label="设置点图形状字段", choices=NULL, selected=NULL, multiple=FALSE),
                actionButton(inputId="tab_button", label="显示分组表格", icon=icon("submit")),
                width=3
              )
          ),
          div(id="group_content",
              mainPanel(dataTableOutput(outputId="group_tab"),
                        width=9
              )
          )
        )
    )
  ),
  
  # 第四部分：可视化图形
  shinyjs::hidden(
    div(id = "plot",
        hr(),
        plotlyOutput(outputId="case_viewer", height="600px")
    )   
  )
)

server <- function(input, output, session) {
  observe({
    # 检查SQL输入框
    if(is.null(input$sql_cmd) | input$sql_cmd == "") {
      shinyjs::disable("refresh_button")
    }
    else{
      shinyjs::enable("refresh_button")
    }
    # 检查可视化规则设置
    if (input$x_field == "" | (is.null(input$y_line_fields) & is.null(input$y_point_fields)) | is.null(input$group_fields)) {
      shinyjs::disable("tab_button")
    } else {
      shinyjs::enable("tab_button")
    }
  })
  
  # 执行SQL命令获取数据
  sql_data <- eventReactive(input$refresh_button, {
    cat(file=stderr(), "#### event log ####: refresh button clicked\n")
    shinyjs::disable("refresh_button")
    shinyjs::hide(id = "table", anim = TRUE)
    shinyjs::hide(id = "plot", anim = TRUE)
    res <- run.sql(input$sql_cmd, debug=TRUE)
    updateSelectInput(session, inputId="group_fields", choices=colnames(res))
    updateSelectInput(session, inputId="x_field", choices=colnames(res))
    updateSelectInput(session, inputId="y_line_fields", choices=colnames(res))
    updateSelectInput(session, inputId="y_point_fields", choices=colnames(res))
    updateSelectInput(session, inputId="group_shape_field", choices=c("无",colnames(res)), selected="无")
    shinyjs::enable("refresh_button")
    shinyjs::show(id = "table", anim = TRUE)
    shinyjs::hide(id = "group_content", anim = FALSE)
    return(res)
  })  
  
  # SQL命令执行状态
  output$download_info <- renderText({
    if(input$refresh_button == 0){
      message <- "请敲入SQL select查询语句，点击按钮提交"
    }
    else{
      message <- isolate({paste0("表格下载成功！总行数",  nrow(sql_data()), "，总列数", ncol(sql_data()), "，更新时间是", as.character(lubridate::now(), format="%Y-%m-%d %H:%M:%S"))
      })
    }
    message
  })
  
  # 显示SQL执行结果
  output$sql_tab <- DT::renderDataTable({
    datatable(sql_data(), filter='top', selection='single')
  })
  
  # 获取绘图分组结果
  group_data <- eventReactive(input$tab_button, {
    cat(file=stderr(), "#### event log ####: tab button clicked\n")
    res <- sql_data() %>%
      select(one_of(input$group_fields)) %>%
      distinct()
    shinyjs::show(id="group_content", anim=TRUE)
    return(res)
  })
  
  output$tab_button_message <- renderText({
    if(input$tab_button == 0) {
      message <- "请在下方左侧设置数据可视化规则；
      点击按钮后，下方右侧将以表格显示数据分组结果；
      点击表格的一行，将在下方绘制该行所指分组数据的图形"
    }
    else {
      message <- isolate({paste0("绘图分组数",  nrow(group_data()), "，更新时间是", as.character(lubridate::now(), format="%Y-%m-%d %H:%M:%S"))
      })
    }
    message
    })
  
  # 显示绘图分组结果
  output$group_tab <- DT::renderDataTable({
    datatable(group_data(), filter='top', selection='single')
  })
  
  # 显示绘图
  observeEvent(input$group_tab_rows_selected, {
    cat(file=stderr(), paste0("#### event log ####: group table row ", input$group_tab_rows_selected, " clicked\n"))
    output$case_viewer <- renderPlotly({
      s <- input$group_tab_row_last_clicked
      cat(file=stderr(), "#### event log ####: table row", s, "clicked\n")
      p <- ggplot()
      filter_str <- isolate({str_c(group_data()[s, input$group_fields], collapse="_")}) # 使用_以配合unite方法
      target_plot_data <- sql_data() %>%
        unite_("new_var", input$group_fields, remove=FALSE) %>%
        filter(new_var==filter_str)
      
      if(length(input$y_line_fields) > 0) {
        target_plot_data$dt <- lubridate::ymd_hms(target_plot_data[,input$x_field], tz="UTC-8")
        line_df <- target_plot_data %>%
          tidyr::gather(col_name, thresh, one_of(input$y_line_fields)) %>%
          dplyr::mutate(thresh=as.numeric(thresh))
        p <- p + geom_line(data=line_df, aes(x=dt,y=thresh,color=col_name))
      }
      if(length(input$y_point_fields) > 0) {
        target_plot_data$dt <- lubridate::ymd_hms(target_plot_data[,input$x_field], tz="UTC-8")
        point_df <- target_plot_data %>%
          tidyr::gather(col_name, thresh, one_of(input$y_point_fields)) %>%
          dplyr::mutate(thresh=as.numeric(thresh))
        if(input$group_shape_field != "无") {
          point_df[, input$group_shape_field] <- as.factor(point_df[, input$group_shape_field])
          p <- p + geom_point(data=point_df, aes_string(x="dt",y="thresh",color="col_name", shape=input$group_shape_field))
        }
        else{
          p <- p + geom_point(data=point_df, aes(x=dt,y=thresh,color=col_name))
        }
      }
      p <- p
      ggplotly(p)
    })
    shinyjs::show("plot", anim = TRUE)
  })
}

shinyApp(ui=ui, server=server)


##################################
fluidPage(
  
  titlePanel("Changing the values of inputs from the server"),
  fluidRow(
    column(3, wellPanel(
      h4("These inputs control the other inputs on the page"),
      textInput("control_label",
                "This controls some of the labels:",
                "LABEL TEXT"),
      sliderInput("control_num",
                  "This controls values:",
                  min = 1, max = 20, value = 15)
    )),
    
    column(3, wellPanel(
      textInput("inText",  "Text input:", value = "start text"),
      numericInput("inNumber", "Number input:",
                   min = 1, max = 20, value = 5, step = 0.5),
      numericInput("inNumber2", "Number input 2:",
                   min = 1, max = 20, value = 5, step = 0.5),
      sliderInput("inSlider", "Slider input:",
                  min = 1, max = 20, value = 15),
      sliderInput("inSlider2", "Slider input 2:",
                  min = 1, max = 20, value = c(5, 15)),
      sliderInput("inSlider3", "Slider input 3:",
                  min = 1, max = 20, value = c(5, 15)),
      dateInput("inDate", "Date input:"),
      dateRangeInput("inDateRange", "Date range input:")
    )),
    
    
    column(3,
           wellPanel(
             checkboxInput("inCheckbox", "Checkbox input",value = FALSE),
             checkboxGroupInput("inCheckboxGroup",
                                "Checkbox group input:",
                                c("label 1" = "option1",
                                  "label 2" = "option2")),
             
             radioButtons("inRadio", "Radio buttons:",
                          c("label 1" = "option1",
                            "label 2" = "option2")),
             
             selectInput("inSelect", "Select input:",
                         c("label 1" = "option1",
                           "label 2" = "option2")),
             
             selectInput("inSelect2", "Select input 2:",
                         multiple = TRUE,
                         c("label 1" = "option1",
                           "label 2" = "option2"))
           ),
           
           
           
           tabsetPanel(id = "inTabset",
                       tabPanel("panel1", h2("This is the first panel.")),
                       tabPanel("panel2", h2("This is the second panel."))
           )
    )
  ))

###############################
function(input, output, clientData, session) {
  
  observe({
    # We‘ll use these multiple times, so use short var names for convenience.
    c_label <- input$control_label
    c_num <- input$control_num
    
    # Text =====================================================
    # Change both the label and the text
    updateTextInput(session, "inText",
                    label = paste("New", c_label),
                    value = paste("New text", c_num)
    )
    
    # Number ===================================================
    # Change the value
    updateNumericInput(session, "inNumber", value = c_num)
    
    # Change the label, value, min, and max
    updateNumericInput(session, "inNumber2",
                       label = paste("Number ", c_label),
                       value = c_num, min = c_num-10, max = c_num+10, step = 5)
    
    # Slider input =============================================
    # Only label and value can be set for slider
    updateSliderInput(session, "inSlider",
                      label = paste("Slider", c_label),
                      value = c_num)
    
    # Slider range input =======================================
    # For sliders that pick out a range, pass in a vector of 2
    # values.
    updateSliderInput(session, "inSlider2",
                      value = c(c_num-1, c_num+1))
    
    # An NA means to not change that value (the low or high one)
    updateSliderInput(session, "inSlider3",
                      value = c(NA, c_num+2))
    
    # Date input ===============================================
    # Only label and value can be set for date input
    updateDateInput(session, "inDate",
                    label = paste("Date", c_label),
                    value = paste("2013-04-", c_num, sep=""))
    
    # Date range input =========================================
    # Only label and value can be set for date range input
    updateDateRangeInput(session, "inDateRange",
                         label = paste("Date range", c_label),
                         start = paste("2013-01-", c_num, sep=""),
                         end = paste("2013-12-", c_num, sep=""),
                         min = paste("2001-01-", c_num, sep=""),
                         max = paste("2030-12-", c_num, sep="")
    )
    
    
    
    # # Checkbox ===============================================
    updateCheckboxInput(session, "inCheckbox",value = c_num %% 2)
    
    # Checkbox group ===========================================
    # Create a list of new options, where the name of the items is something like ‘option label x A‘, and the values are ‘option-x-A‘.
    cb_options <- list()
    cb_options[[paste("option label", c_num, "A")]]<-paste0("option-", c_num, "-A")
    cb_options[[paste("option label", c_num, "B")]] <-paste0("option-", c_num, "-B")
    
    # Set the label, choices, and selected item
    updateCheckboxGroupInput(session, "inCheckboxGroup",
                             label = paste("checkboxgroup", c_label),
                             choices = cb_options,
                             selected = paste0("option-", c_num, "-A")
    )
    
    # Radio group ==============================================
    # Create a list of new options, where the name of the items is something like ‘option label x A‘, and the values are 
    r_options <- list()
    r_options[[paste("option label", c_num, "A")]]<-paste0("option-", c_num, "-A")
    r_options[[paste("option label", c_num, "B")]]<-paste0("option-", c_num, "-B")
    
    # Set the label, choices, and selected item
    updateRadioButtons(session, "inRadio",label = paste("Radio", c_label),choices = r_options,selected = paste0("option-", c_num, "-A")
    )
    
    
    
    
    
    # Select input =============================================
    # Create a list of new options, where the name of the items is something like ‘option label x A‘, and the values are ‘option-x-A‘.
    
    s_options <- list()
    s_options[[paste("option label", c_num, "A")]]<-paste0("option-", c_num, "-A")
    s_options[[paste("option label", c_num, "B")]]<-paste0("option-", c_num, "-B")
    
    # Change values for input$inSelect
    updateSelectInput(session, "inSelect",
                      choices = s_options,
                      selected = paste0("option-", c_num, "-A")
    )
    
    
    # Can also set the label and select an item (or more than one if it‘s a multi-select)
    updateSelectInput(session, "inSelect2",
                      label = paste("Select label", c_label),
                      choices = s_options,
                      selected = paste0("option-", c_num, "-B")
    )
    
    # Tabset input =============================================
    # Change the selected tab.
    # The tabsetPanel must have been created with an ‘id‘ argument
    
    if (c_num %% 2) {
      updateTabsetPanel(session, "inTabset", selected = "panel2")
    } else {
      updateTabsetPanel(session, "inTabset", selected = "panel1")
    }
  })}



################################
######我要这种格式##############
library(shinydashboard)
header <- dashboardHeader(title="doodle look at me")
sidebar <- dashboardSidebar(
  selectInput(inputId = "dataset",
              label = "Choose a dataset:",
              choices = c("rock", "pressure", "cars")),
  sliderInput("obs","rows:",1,50,20)
)
body <- dashboardBody(tableOutput("view"))
ui <- dashboardPage(header, sidebar, body,skin = "green")
server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
}
shinyApp(ui = ui, server = server)



############################################
if (interactive()) {
  library(shiny)
  
  ui <- dashboardPage(
    dashboardHeader(title = "Dynamic boxes"),
    dashboardSidebar(),
    dashboardBody(
      fluidRow(
        box(width = 2, actionButton("count", "Count")),
        infoBoxOutput("ibox"),
        valueBoxOutput("vbox")
      )
    )
  )
  
  server <- function(input, output) {
    output$ibox <- renderInfoBox({
      infoBox(
        "Title",
        input$count,
        icon = icon("credit-card")
      )
    })
    output$vbox <- renderValueBox({
      valueBox(
        "Title",
        input$count,
        icon = icon("credit-card")
      )
    })
  }
  
  shinyApp(ui, server)
}

