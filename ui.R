
# 필요한 라이브러리(server)
#library(RMySQL) # DB 연결
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(rsconnect) # 브라우저 연결
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(tidyverse)
library(plyr)
library(dplyr)
library(gridExtra) # table plot
library(DT)
library(fmsb) # radar chart
library(shinycssloaders)


options(shiny.sanitize.errors = FALSE)
#rsconnect::showLogs()
# UI 정의
# # ShinyUI(fluidPage()): shiny app의 ui를 설정해줌(GUI 아웃라인)
ui <- shinyUI(fluidPage(theme = shinytheme("simplex"),
                        
                        # title 제목
                        h1(strong("E-EPL")),
                        h4(strong('Easy - English Premier League')),
                        
                        # sidebar(네모상자) 레이아웃 지정
                        sidebarLayout(
                          sidebarPanel( # 사이드바 제목
                            
                            # h1,h2,h3,h4,h5,h6: 글의 대제목 1~6까지 지정 가능
                            h3("DB Term Project Final"),
                            
                            # p: 내용 입력
                            h4('"Now you can talk about football"'),
                            br(),
                            br(),
                            p('Team Member'),
                            p('21400754 최민호'),
                            p('21500700 조현범'),
                            p('21600033 권하은'),
                            p('21800284 박성현'),
                            br(),
                            
                            
                            br('-------------------------------------------------------------------------------'),
                            h3("Search for Player of EPL"),
                            p("Please select all search options and press the button."),
                            
                            # 검색 버튼 1
                            fluidRow(
                              
                              column(6,
                                     actionButton("search_btn1", "Search"))
                              
                            ),
                            
                            # 선수 이름 검색창 1
                            fluidRow( # fluidRow(): 행 추가
                              
                              column(6,
                                     textInput("player_names", h3("Search Player 1's name")))
                              
                            ),
                            
                            # 선수 이름 검색창 2
                            fluidRow( # fluidRow(): 행 추가
                              
                              column(6,
                                     textInput("player_names2", h3("Search Player 2's name")))
                              
                            ),
                            
                            # 시즌 셀렉트 박스(선수)
                            fluidRow(
                              
                              column(6,
                                     checkboxGroupInput("player_seasons", h3("Select Season"),
                                                        choices = list('2014_2015',
                                                                       '2015_2016',
                                                                       '2016_2017',
                                                                       '2017_2018',
                                                                       '2018_2019',
                                                                       '2019_2020'),
                                                        selected = '2019_2020'))
                              
                            ),
                            
                            
                            # 포지션 셀렉트 박스
                            # fluidRow(
                            #   
                            #   column(6,
                            #          selectInput("positions", h3("Select Position"),
                            #                      choices = c("NULL", position_name),
                            #                      selected = "NULL"))
                            #   
                            # ),
                            
                            
                            br('-------------------------------------------------------------------------------'),
                            h3("Search for Team of EPL"),
                            p("Please select all search options and press the button."),
                            
                            # 검색 버튼 2
                            fluidRow(
                              
                              column(6,
                                     actionButton("search_btn2", "Search"))
                              
                            ),
                            
                            # 팀 셀렉트 박스 1
                            fluidRow(
                              
                              column(6,
                                     selectInput("teams", h3("Select Team 1"),
                                                 choices = c("NULL", team_name),
                                                 selected = "NULL"))
                              
                            ),
                            
                            # 팀 셀렉트 박스 2
                            fluidRow(
                              
                              column(6,
                                     selectInput("teams2", h3("Select Team 2"),
                                                 choices = c("NULL", team_name),
                                                 selected = "NULL"))
                              
                            ),
                            
                            # 시즌 체크 박스(팀)
                            fluidRow(
                              
                              column(6,
                                     checkboxGroupInput("team_seasons", h3("Select Season"),
                                                        choices = list('2014_2015',
                                                                       '2015_2016',
                                                                       '2016_2017',
                                                                       '2017_2018',
                                                                       '2018_2019',
                                                                       '2019_2020'),
                                                        selected = '2019_2020'))
                              
                            ),
                            
                            br('-------------------------------------------------------------------------------'),
                            h3("Search for Everything in the EPL"),
                            p("Please select options and press the button."),
                            
                            # 검색 버튼 3
                            fluidRow(
                              
                              column(6,
                                     actionButton("search_btn3", "Show"))
                              
                            ),
                            
                            # 시각화 셀렉트 박스
                            fluidRow(
                              
                              column(7,
                                     selectInput("graphs", h4("Select What You Want to See"),
                                                 choices = list("NULL",
                                                                "League Trend",
                                                                "Power Ranking",
                                                                "Nationality of Player",
                                                                "Effective Goal Ratio",
                                                                "Ability to Create Opportunities"),
                                                 selected = "NULL"))
                              
                            ),
                            
                            br(),
                            br(),
                            br()
                            
                          ),
                          
                          # 메인 패널(앱 기준 오른쪽 패널)
                          mainPanel(
                            h4(strong("Player Name Database")),
                            p("해당 선수가 있는지 이름을 검색하실 수 있습니다."),
                            dataTableOutput("table"),
                            br(),
                            h4(strong("Player Radar Chart")),
                            textOutput("graph_txt1"),
                            textOutput("graph_txt1_2"),
                            plotOutput("graph1"),
                            br(),
                            h4(strong("Team Radar Chart")),
                            textOutput("graph_txt2"),
                            textOutput("graph_txt2_2"),
                            plotOutput("graph2"),
                            br(),
                            h4(strong("League Analysis")),
                            textOutput("graph_txt3"),
                            plotOutput("graph3"),
                            br()
                            
                          )
                        )
))