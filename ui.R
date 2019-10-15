#rgb(126,12,110)南开紫
#奖项分类
jiangxiang<-c("北美建模","全国大学生数学建模","全国大学生数学竞赛","奖学金")
jiangxiangjibie<-c("M奖","H奖","全国一等","全国二等","全国三等","省级一等","省级二等","省级三等","国家级","省级","市级","校级")
Jiangxiang<-rep(jiangxiang,c(2,6,6,4))
Jiangxiangjibie<-paste(Jiangxiang,jiangxiangjibie,sep="")
JX<-data.frame(Jiangxiang,Jiangxiangjibie,result=LETTERS[1:18])


library(shinydashboard)
library(shiny)
baoming<-fluidPage(
  a(href="http://www.nankai.edu.cn", "南开大学官网"),
  tags$head(
    tags$title('南开大学优秀学生夏令营报名表'),
    tags$style(
      rel = 'stylesheet',
      '.title-panel {background: rgb(126,12,110)} ',
      '.title-panel h2 {text-align:center; color:black}'
    )
  ),
  
  div(
    class='col-md-12 title-panel',
    h2('南开大学优秀学生夏令营报名表')
  ),
  
  
  tags$style(
    ".container div {border: 1px solid gray; min-height:30px;}",
    "h4 {color:purple; margin-top: 20px;}"
  ),
  
  h4("1、基本信息"),
  textInput("name","姓名","Your name"),
  checkboxGroupInput("sex","性别",c("男"="男","女"="女")),
  textInput("shenfenzheng","身份证号码","Your ID"),
  dateInput("date", "出生日期：", value = Sys.Date()),
  textInput("minzu","民族","Your name"),
  
  
  h4("2、学业信息"),
  textInput("university","学校名称","Your University"),
  textInput("school","学院名称","Your School"),
  textInput("major","所学专业","Your Major"),
  checkboxGroupInput("paimingdanwei","排名单位",c("学院"="学院","专业"="专业")),
  textInput("paiming","排名"),
  textInput("number","人数"),
  fileInput("chengjidan", "成绩单", multi=T,accept=c(".jpg",".jpeg",".jpg")),
  fileInput("排名证明", "排名证明", multi=T,accept=c(".jpg",".jpeg",".jpg")),
  
  h4("3、奖惩情况"),
  selectInput("jiangli","奖惩情况：",
              list('建模'=list("北美建模","全国大学生数学建模"),
                   '数学竞赛'=list("全国大学生数学竞赛"),
                   '奖学金'=list("奖学金"))
              ),
  uiOutput("jxjb")
  

  #submitButton("Update View", icon("refresh"))
)


jianli<-fluidPage(
  
  splitLayout(splitLayout("姓名", verbatimTextOutput("jianli_name")),splitLayout("性别", verbatimTextOutput("jianli_sex")),br()),
  splitLayout(splitLayout("出生日期", verbatimTextOutput("jianli_date"))),
  splitLayout(splitLayout("学校名称", verbatimTextOutput("jianli_university")),splitLayout("学院名称",verbatimTextOutput("jianli_school"))),
  splitLayout(verbatimTextOutput("jianli_paimingdanwei"),splitLayout("排名", verbatimTextOutput("jianli_paiming")),splitLayout("人数",verbatimTextOutput("jianli_number")),
              splitLayout("排名百分比",verbatimTextOutput("jianli_baifenbi")))
)

header <- dashboardHeader(title="南开大学夏令营")
sidebar <- dashboardSidebar(
  sidebarUserPanel("User Name",
                   subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   # Image file should be in www/ subdir
                   image = "userimage.png"
  ),
  sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("报名表", tabName = "baomingbiao", icon = icon("dashboard")),
    menuItem("生成简历", icon = icon("th"), tabName = "jianli", badgeLabel = "to-do",
             badgeColor = "green"),
    menuItem("Charts", icon = icon("bar-chart-o"),
             menuSubItem("Sub-item 1", tabName = "subitem1"),
             menuSubItem("Sub-item 2", tabName = "subitem2")
             
    ),
    a(img(src="nankai.jpeg",height=100))
  )
)
body<-dashboardBody(
  tabItems(
    tabItem("baomingbiao",baoming),
    tabItem("jianli",jianli)
  )
)


dashboardPage(header, sidebar, body,skin = "purple")




