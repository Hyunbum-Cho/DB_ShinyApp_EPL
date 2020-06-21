
# Define server logic ----
server <- function(input, output) {
  
  # search_name 대신 if문 + query 들어가면 됨
  output$table <- renderDataTable(search_name,
                                  options = list(pageLength = 5)
  )
  
  # 선수 특성 문장
  output$graph_txt1 <- renderText({
    if(input$search_btn1){
      df <- preset_player %>% filter(full_name == input$player_names)
      paste0(as.character(df[1]), " : A '", as.character(df[2]), "' '", as.character(df[4]), "' '", as.character(df[3]), "'")
    }
  })
  
  # 선수 특성 문장
  output$graph_txt1_2 <- renderText({
    if(input$search_btn1){
      df <- preset_player %>% filter(full_name == input$player_names2)
      paste0(as.character(df[1]), " : A '", as.character(df[2]), "' '", as.character(df[4]), "' '", as.character(df[3]), "'")
    }
  })
  
  output$graph1 <- renderPlot({
    if(input$search_btn1){
      plot.new() # plot 창 초기화
      
      layout(matrix(1:2, ncol=2)) # radar chart 분할
      
      # filtering 1
      pos <- search_name[grep(input$player_names, search_name$full_name, ignore.case = TRUE), 2]
      if(pos == 'Goalkeeper'){
        df <- gk_player %>%
          filter(full_name == input$player_names) %>% 
          filter(season %in% input$player_seasons)
        df <- rbind(gk_player[c(1:2), ], df)
        
        # radar chart
        colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        radarchart(df[-(1:2)], axistype=2,
                   #custom polygon
                   pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.7, title = paste0("Player 1 : ", input$player_names)
        )
      } else if(pos == 'Defender'){
        df <- cb_player %>%
          filter(full_name == input$player_names) %>% 
          filter(season %in% input$player_seasons)
        df <- rbind(cb_player[c(1:2), ], df)
        
        # radar chart
        colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        radarchart(df[-(1:2)], axistype=2,
                   #custom polygon
                   pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.7, title = paste0("Player 1 : ", input$player_names)
        )
      } else if(pos == 'Midfielder'){
        df <- mf_player %>%
          filter(full_name == input$player_names) %>% 
          filter(season %in% input$player_seasons)
        df <- rbind(mf_player[c(1:2), ], df)
        
        # radar chart
        colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        radarchart(df[-(1:2)], axistype=2,
                   #custom polygon
                   pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.7, title = paste0("Player 1 : ", input$player_names)
        )
      } else if(pos == 'Forward'){
        df <- cf_player %>%
          filter(full_name == input$player_names) %>% 
          filter(season %in% input$player_seasons)
        df <- rbind(cf_player[c(1:2), ], df)
        
        # radar chart
        colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        radarchart(df[-(1:2)], axistype=2,
                   #custom polygon
                   pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.7, title = paste0("Player 1 : ", input$player_names)
        )
      }
      
      # filtering 2
      pos2 <- search_name[grep(input$player_names2, search_name$full_name, ignore.case = TRUE), 2]
      if(pos2 == 'Goalkeeper'){
        df2 <- gk_player %>%
          filter(full_name == input$player_names2) %>% 
          filter(season %in% input$player_seasons)
        df2 <- rbind(gk_player[c(1:2), ], df2)
        
        # radar chart
        colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        radarchart(df2[-(1:2)], axistype=2,
                   #custom polygon
                   pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.7, title = paste0("Player 2 : ", input$player_names2)
        )
      } else if(pos2 == 'Defender'){
        df2 <- cb_player %>%
          filter(full_name == input$player_names2) %>% 
          filter(season %in% input$player_seasons)
        df2 <- rbind(cb_player[c(1:2), ], df2)
        
        # radar chart
        colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        radarchart(df2[-(1:2)], axistype=2,
                   #custom polygon
                   pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.7, title = paste0("Player 2 : ", input$player_names2)
        )
      } else if(pos2 == 'Midfielder'){
        df2 <- mf_player %>%
          filter(full_name == input$player_names2) %>% 
          filter(season %in% input$player_seasons)
        df2 <- rbind(mf_player[c(1:2), ], df2)
        
        # radar chart
        colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        radarchart(df2[-(1:2)], axistype=2,
                   #custom polygon
                   pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.7, title = paste0("Player 2 : ", input$player_names2)
        )
      } else if(pos2 == 'Forward'){
        df2 <- cf_player %>%
          filter(full_name == input$player_names2) %>% 
          filter(season %in% input$player_seasons)
        df2 <- rbind(cf_player[c(1:2), ], df2)
        
        # radar chart
        colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        radarchart(df2[-(1:2)], axistype=2,
                   #custom polygon
                   pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.7, title = paste0("Player 2 : ", input$player_names2)
        )
      }
      
      layout(matrix(1:1, ncol=1))
    }
  })
  
  # 팀 특성 문장
  output$graph_txt2 <- renderText({
    if(input$search_btn2){
      df <- preset_team %>% filter(common_name == input$teams)
      paste0(as.character(df[1]), " : A '", as.character(df[2]), "' style '", as.character(df[3]), "' '", as.character(df[4]), "'")
    }
  })
  
  # 팀 특성 문장
  output$graph_txt2_2 <- renderText({
    if(input$search_btn2){
      df <- preset_team %>% filter(common_name == input$teams2)
      paste0(as.character(df[1]), " : A '", as.character(df[2]), "' style '", as.character(df[3]), "' '", as.character(df[4]), "'")
    }
  })
  
  output$graph2 <- renderPlot({
    if(input$search_btn2){
      plot.new() # plot 창 초기화
      
      layout(matrix(1:2, ncol=2))
      
      # filtering
      if(input$teams != "NULL" || input$teams2 != "NULL"){
        df <- team_inf %>%
          filter(common_name == input$teams) %>%
          filter(season %in% input$team_seasons)
        df <- rbind(team_inf[c(1:2), ], df)
        
        df2 <- team_inf %>%
          filter(common_name == input$teams2) %>%
          filter(season %in% input$team_seasons)
        df2 <- rbind(team_inf[c(1:2), ], df2)
        
        # radar chart
        colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        radarchart(df[-(1:2)], axistype=2,
                   #custom polygon
                   pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.7, title = paste0("Team 1 : ", input$teams)
        )
        
        radarchart(df2[-(1:2)], axistype=2,
                   #custom polygon
                   pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.7, title = paste0("Team 2 : ", input$teams2)
        )
        
      }
      
      layout(matrix(1:1, ncol=1))
      
    }
  })
  
  # 리그 특성 문장
  output$graph_txt3 <- renderText({
    if(input$search_btn3){
      if(input$graphs == 'Trend'){
        paste0("")
      }
    }
  })
  
  output$graph3 <- renderPlot({
    if(input$search_btn3){
      if(input$graphs == 'League Trend'){
        plot.new() # plot 창 초기화
        
        # ggplot에서 화면분할
        # http://blog.daum.net/buillee/375
        p1 <- ggplot(league_stat, aes(x = season, y = average_goals_per_match, group = 1)) +
          geom_line() + geom_point() + theme_bw() +
          ggtitle("시즌별 한 경기 평균 골 수") + 
          geom_hline(yintercept=2.6, linetype='dashed', color='red', size=1) +
          geom_hline(yintercept=2.9, linetype='dashed', color='red', size=1) + 
          annotate("text", x=1.5, y=2.62, label="Balanced", size=3 , color='red') +
          annotate("text", x=1.5, y=2.92, label="Aggresive", size=3 , color='red')
        
        p2 <- ggplot(league_stat, aes(x = season, y = average_cards_per_match, group = 1)) +
          geom_line() + geom_point() + theme_bw() +
          ggtitle("시즌별 한 경기 평균 카드 수") + 
          geom_hline(yintercept=4.5, linetype='dashed', color='red', size=1) +
          geom_hline(yintercept=3.5, linetype='dashed', color='red', size=1) + 
          geom_hline(yintercept=3.0, linetype='dashed', color='red', size=1) + 
          annotate("text", x=1.5, y=3.05, label="Average Rough", size=3 , color='red') +
          annotate("text", x=1.5, y=3.55, label="Rough", size=3 , color='red') + 
          annotate("text", x=1.5, y=4.55, label="Very Rough", size=3 , color='red')
        
        p3 <- ggplot(league_stat, aes(x = season, y = prediction_risk, group = 1)) +
          geom_line() + geom_point() + theme_bw() +
          ggtitle("시즌 예측 위험도") + 
          geom_hline(yintercept=64, linetype='dashed', color='red', size=1) +
          geom_hline(yintercept=49, linetype='dashed', color='red', size=1) + 
          annotate("text", x=1.5, y=50, label="Possible to predict", size=3 , color='red') +
          annotate("text", x=1.5, y=65, label="Hard to predict", size=3 , color='red')
        
        p4 <- ggplot(league_stat, aes(x = season, y = btts_percentage, group = 1)) +
          geom_line() + geom_point() + theme_bw() +
          ggtitle("시즌별 두 팀 모두 득점할 확률") + 
          geom_hline(yintercept=50, linetype='dashed', color='red', size=1) +
          geom_hline(yintercept=40, linetype='dashed', color='red', size=1) + 
          annotate("text", x=2.5, y=40.5, label="Games with one or two goals", size=3 , color='red') +
          annotate("text", x=2.5, y=50.5, label="Games with many goals", size=3 , color='red')
        
        grid.arrange(p1, p2, p3, p4, ncol=4)
      }
      
      if(input$graphs == 'Power Ranking'){
        plot.new() # plot 창 초기화
        
        ggplot(power_rank, aes(x=season, y=rank, colour=common_name)) +
          geom_line() + geom_point() + xlab("season") + ylab("ranking") +
          ggtitle("EPL Power Ranking for 6 seasons") +
          scale_y_reverse() +
          geom_hline(yintercept=4, linetype='dashed', color='blue', size=1) +
          geom_hline(yintercept=17.5, linetype='dashed', color='red', size=1) +
          geom_hline(yintercept=20.5, linetype='dashed', color='black', size=1) + 
          theme_light() + 
          annotate("text", x=2019.5, y=3, label="UCL qualified", size=4 , color='blue') +
          annotate("text", x=2019.5, y=16, label="Relegated", size=4 , color='red') + 
          annotate("text", x=2019.5, y=19, label="EFL Championship", size=4 , color='black')
      }
      
      else if(input$graphs == 'Nationality of Player'){
        plot.new() # plot 창 초기화
        
        ggplot(nation, aes(x=season, y=num, colour=nationality)) +
          geom_line() + geom_point() + xlab("season") + ylab("ratio") +
          ggtitle("Nationality of Players for 6 seasons") +
          theme_light()
      }
      
      else if(input$graphs == 'Effective Goal Ratio'){
        plot.new() # plot 창 초기화
        
        ggplot(eff_goal, aes(x=season, y=ratio, colour=home_team_name)) +
          geom_line() + geom_point() + xlab("season") + ylab("ratio") +
          ggtitle("Effective Goal Ratio for 6 seasons") +
          theme_light()
      }
      
      else if(input$graphs == 'Ability to Create Opportunities'){
        plot.new() # plot 창 초기화
        
        ggplot(goal_involved, aes(x=reorder(name, goals_involved), y=goals_involved)) +
          geom_col(aes(group = 1), fill="lightblue") +
          facet_wrap(.~season, ncol=3, scales="free_x") +
          geom_text_repel(data = goal_involved, aes(label = name)) +
          xlab(" ") + ylab(" ") +
          theme_light()
      }
    }
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
