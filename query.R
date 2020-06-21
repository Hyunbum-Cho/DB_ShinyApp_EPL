
# 필요한 라이브러리(server)
library(RMySQL) # DB 연결
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


# rm(list=ls())


# DB 연결 - dbConnect() 함수
eplDB <- dbConnect(
  MySQL(),
  user = 'dba',
  password = 'mysql',
  host = '192.168.56.101',
  dbname = 'soccer'
)



# DB의 캐릭터 셋을 UTF-8로 지정해주어야 합니다.
# 이 설정이 없으면 R로 데이터를 불러올 때 문자를 제대로 가져오지 못할 수도 있습니다.
dbSendQuery(eplDB, 'set character set "utf8"')

# 함수 사용법
# 데이터 조회 - SELECT - dbGetQuery()
# 데이터 삽입/변경/삭제 - INSERT/UPDATE/DELETE - dbSendQuery()


######################### 쿼리 기초 작성 #########################

# 1) 팀 이름 수집
team_2015 <- dbGetQuery(
  eplDB,
  "SELECT common_name FROM epl_teams_2014_2015;"
)

team_2016 <- dbGetQuery(
  eplDB,
  "SELECT common_name FROM epl_teams_2015_2016;"
)

team_2017 <- dbGetQuery(
  eplDB,
  "SELECT common_name FROM epl_teams_2016_2017;"
)

team_2018 <- dbGetQuery(
  eplDB,
  "SELECT common_name FROM epl_teams_2017_2018;"
)

team_2019 <- dbGetQuery(
  eplDB,
  "SELECT common_name FROM epl_teams_2018_2019;"
)

team_2020 <- dbGetQuery(
  eplDB,
  "SELECT common_name FROM epl_teams_2019_2020;"
)

team_name <- rbind(team_2015, team_2016, team_2017, team_2018, team_2019, team_2020)
team_name <- unique(team_name)

# 사용한 데이터프레임 삭제
rm(list = c("team_2015", "team_2016", "team_2017", "team_2018", "team_2019", "team_2020"))


# 2) 포지션 이름
position_name <- dbGetQuery(
  eplDB,
  "SELECT distinct position FROM epl_players_2019_2020;"
)


# 3) 이름 검색 방법
search_name <- dbGetQuery(
  eplDB,
  paste0("select distinct full_name, position from epl_players_2019_2020;")
)
search_name <- rbind(search_name, dbGetQuery(
  eplDB,
  paste0("select distinct full_name, position from epl_players_2018_2019;")
))
search_name <- rbind(search_name, dbGetQuery(
  eplDB,
  paste0("select distinct full_name, position from epl_players_2017_2018;")
))
search_name <- rbind(search_name, dbGetQuery(
  eplDB,
  paste0("select distinct full_name, position from epl_players_2016_2017;")
))
search_name <- rbind(search_name, dbGetQuery(
  eplDB,
  paste0("select distinct full_name, position from epl_players_2015_2016;")
))
search_name <- rbind(search_name, dbGetQuery(
  eplDB,
  paste0("select distinct full_name, position from epl_players_2014_2015;")
))
search_name <- unique(search_name)

# 4) 팀 육각형 그래프
# 2020
team_inf <- dbGetQuery(
  eplDB,
  "select common_name, points_per_game, goals_scored_per_match,
       goals_conceded_per_match, fouls from epl_teams_2019_2020;"
)
temp <- dbGetQuery(eplDB,
                   "select home_team_name, round(avg(attendance)) as avg_attendance
from epl_matches_2019_2020 group by home_team_name;")
colnames(temp)[1] <- "common_name"

team_inf <- merge(team_inf, temp, by="common_name", all.x = T)
team_inf <- cbind(team_inf, season = "2019_2020")
rownames(team_inf) <- paste0(team_inf$common_name, "_2019_2020")

# 2019
df <- dbGetQuery(
  eplDB,
  "select common_name, points_per_game, goals_scored_per_match,
       goals_conceded_per_match, fouls from epl_teams_2018_2019;"
)
temp <- dbGetQuery(eplDB,
                   "select home_team_name, round(avg(attendance)) as avg_attendance
from epl_matches_2018_2019 group by home_team_name;")
colnames(temp)[1] <- "common_name"
df <- merge(df, temp, by="common_name", all.x = T)
df <- cbind(df, season = "2018_2019")
rownames(df) <- paste0(df$common_name, "_2018_2019")

team_inf <- rbind(team_inf, df)

# 2018
df <- dbGetQuery(
  eplDB,
  "select common_name, points_per_game, goals_scored_per_match,
       goals_conceded_per_match, fouls from epl_teams_2017_2018;"
)
temp <- dbGetQuery(eplDB,
                   "select home_team_name, round(avg(attendance)) as avg_attendance
from epl_matches_2017_2018 group by home_team_name;")
colnames(temp)[1] <- "common_name"
df <- merge(df, temp, by="common_name", all.x = T)
df <- cbind(df, season = "2017_2018")
rownames(df) <- paste0(df$common_name, "_2017_2018")

team_inf <- rbind(team_inf, df)

# 2017
df <- dbGetQuery(
  eplDB,
  "select common_name, points_per_game, goals_scored_per_match,
       goals_conceded_per_match, fouls from epl_teams_2016_2017;"
)
temp <- dbGetQuery(eplDB,
                   "select home_team_name, round(avg(attendance)) as avg_attendance
from epl_matches_2016_2017 group by home_team_name;")
colnames(temp)[1] <- "common_name"
df <- merge(df, temp, by="common_name", all.x = T)
df <- cbind(df, season = "2016_2017")
rownames(df) <- paste0(df$common_name, "_2016_2017")

team_inf <- rbind(team_inf, df)

# 2016
df <- dbGetQuery(
  eplDB,
  "select common_name, points_per_game, goals_scored_per_match,
       goals_conceded_per_match, fouls from epl_teams_2015_2016;"
)
temp <- dbGetQuery(eplDB,
                   "select home_team_name, round(avg(attendance)) as avg_attendance
from epl_matches_2015_2016 group by home_team_name;")
colnames(temp)[1] <- "common_name"
df <- merge(df, temp, by="common_name", all.x = T)
df <- cbind(df, season = "2015_2016")
rownames(df) <- paste0(df$common_name, "_2015_2016")

team_inf <- rbind(team_inf, df)

# 2015
df <- dbGetQuery(
  eplDB,
  "select common_name, points_per_game, goals_scored_per_match,
       goals_conceded_per_match, fouls from epl_teams_2014_2015;"
)
temp <- dbGetQuery(eplDB,
                   "select home_team_name, round(avg(attendance)) as avg_attendance
from epl_matches_2014_2015 group by home_team_name;")
colnames(temp)[1] <- "common_name"
df <- merge(df, temp, by="common_name", all.x = T)
df <- cbind(df, season = "2014_2015")
rownames(df) <- paste0(df$common_name, "_2014_2015")

team_inf <- rbind(team_inf, df)
team_inf <- team_inf[c(1, 7, 2:6)]

# min-max
team_inf <- rbind(c("", "", 3, 3, 3, 600, 80000),
                  c("", "", 0, 0, 0, 0, 0), team_inf
)
team_inf$points_per_game <- as.numeric(team_inf$points_per_game)
team_inf$goals_scored_per_match <- as.numeric(team_inf$goals_scored_per_match)
team_inf$goals_conceded_per_match <- as.numeric(team_inf$goals_conceded_per_match)
team_inf$fouls <- as.numeric(team_inf$fouls)
team_inf$avg_attendance <- as.numeric(team_inf$avg_attendance)



# 5) 파워랭킹 추이
power_rank <- dbGetQuery(eplDB,
                         "SELECT A.common_name, IFNULL(B.performance_rank,21) AS 2014_2015, IFNULL(C.performance_rank,21) AS 2015_2016,
       IFNULL(D.performance_rank,21) AS 2016_2017, IFNULL(E.performance_rank,21) AS 2017_2018, IFNULL(F.performance_rank,21) AS 2018_2019,
       IFNULL(G.performance_rank,21) AS 2019_2020
    FROM (SELECT common_name FROM epl_teams_2014_2015
    UNION SELECT common_name FROM epl_teams_2015_2016
    UNION SELECT common_name FROM epl_teams_2016_2017
    UNION SELECT common_name FROM epl_teams_2017_2018
    UNION SELECT common_name FROM epl_teams_2018_2019
    UNION SELECT common_name FROM epl_teams_2019_2020) AS A
LEFT OUTER JOIN epl_teams_2014_2015 B ON A.common_name=B.common_name
LEFT OUTER JOIN epl_teams_2015_2016 C ON A.common_name=C.common_name
LEFT OUTER JOIN epl_teams_2016_2017 D ON A.common_name=D.common_name
LEFT OUTER JOIN epl_teams_2017_2018 E ON A.common_name=E.common_name
LEFT OUTER JOIN epl_teams_2018_2019 F ON A.common_name=F.common_name
LEFT OUTER JOIN epl_teams_2019_2020 G ON A.common_name=G.common_name;")

colnames(power_rank) <- c("common_name", 2015, 2016, 2017, 2018, 2019, 2020)
power_rank <- power_rank %>% gather(season, rank, -common_name)
power_rank$season <- as.numeric(power_rank$season)


# 6) 개인 육각형 그래프
# 골키퍼
gk_player <- dbGetQuery(eplDB,
                        "SELECT full_name, '2014_2015' as season, conceded_overall, conceded_per_90_overall, appearances_overall, clean_sheets_overall
FROM epl_players_2014_2015 WHERE position = 'Goalkeeper'
UNION SELECT full_name, '2015_2016' AS season,conceded_overall, conceded_per_90_overall, appearances_overall, clean_sheets_overall
       FROM epl_players_2015_2016 WHERE position = 'Goalkeeper'
UNION SELECT full_name, '2016_2017' AS season, conceded_overall, conceded_per_90_overall, appearances_overall, clean_sheets_overall
       FROM epl_players_2016_2017 WHERE position = 'Goalkeeper'
UNION SELECT full_name, '2017_2018' AS season, conceded_overall, conceded_per_90_overall, appearances_overall, clean_sheets_overall
       FROM epl_players_2017_2018 WHERE position = 'Goalkeeper'
UNION SELECT full_name, '2018_2019' AS season, conceded_overall, conceded_per_90_overall, appearances_overall, clean_sheets_overall
       FROM epl_players_2018_2019 WHERE position = 'Goalkeeper'
UNION SELECT full_name, '2019_2020' AS season, conceded_overall, conceded_per_90_overall, appearances_overall, clean_sheets_overall
       FROM epl_players_2019_2020 WHERE position = 'Goalkeeper';")

# 수비수
cb_player <- dbGetQuery(eplDB,
                        "SELECT full_name, '2014_2015' as season, goals_overall, assists_overall, appearances_overall,
       conceded_per_90_overall, clean_sheets_overall,(yellow_cards_overall+red_cards_overall) AS card
FROM epl_players_2014_2015 WHERE position = 'Defender'
UNION SELECT full_name, '2015_2016' AS season, goals_overall,assists_overall, appearances_overall,
       conceded_per_90_overall, clean_sheets_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2015_2016 WHERE position = 'Defender'
UNION SELECT full_name, '2016_2017' AS season, goals_overall, assists_overall, appearances_overall,
       conceded_per_90_overall, clean_sheets_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2016_2017 WHERE position = 'Defender'
UNION SELECT full_name, '2017_2018' AS season, goals_overall, assists_overall, appearances_overall,
       conceded_per_90_overall, clean_sheets_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2017_2018 WHERE position = 'Defender'
UNION SELECT full_name, '2018_2019' AS season, goals_overall, assists_overall, appearances_overall,
       conceded_per_90_overall, clean_sheets_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2018_2019 WHERE position = 'Defender'
UNION SELECT full_name, '2019_2020' AS season,goals_overall, assists_overall, appearances_overall,
       conceded_per_90_overall, clean_sheets_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2019_2020 WHERE position = 'Defender';")

# 미드필더
mf_player <- dbGetQuery(eplDB,
                        "SELECT full_name, '2014_2015' as season, goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
FROM epl_players_2014_2015 WHERE position = 'Midfielder'
UNION SELECT full_name, '2015_2016' AS season, goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2015_2016 WHERE position = 'Midfielder'
UNION SELECT full_name, '2016_2017' AS season, goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2016_2017 WHERE position = 'Midfielder'
UNION SELECT full_name, '2017_2018' AS season, goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2017_2018 WHERE position = 'Midfielder'
UNION SELECT full_name, '2018_2019' AS season, goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2018_2019 WHERE position = 'Midfielder'
UNION SELECT full_name, '2019_2020' AS season,goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2019_2020 WHERE position = 'Midfielder';")

# 공격수
cf_player <- dbGetQuery(eplDB,
                        "SELECT full_name, '2014_2015' as season, goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
FROM epl_players_2014_2015 WHERE position = 'Forward'
UNION SELECT full_name, '2015_2016' AS season, goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2015_2016 WHERE position = 'Forward'
UNION SELECT full_name, '2016_2017' AS season, goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2016_2017 WHERE position = 'Forward'
UNION SELECT full_name, '2017_2018' AS season, goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2017_2018 WHERE position = 'Forward'
UNION SELECT full_name, '2018_2019' AS season, goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2018_2019 WHERE position = 'Forward'
UNION SELECT full_name, '2019_2020' AS season,goals_overall, goals_per_90_overall, assists_overall, assists_per_90_overall,
       appearances_overall, (yellow_cards_overall+red_cards_overall) AS card
       FROM epl_players_2019_2020 WHERE position = 'Forward';")

gk_player <- rbind(c("", "", 60, 4, 38, 38),
                   c("", "", 0, 0, 0, 0), gk_player
)
rownames(gk_player)[-c(1:2)] <- paste0(gk_player$full_name[-c(1:2)], '_', gk_player$season[-c(1:2)])

cb_player <- rbind(c("", "", 5, 10, 38, 19, 38, 12),
                   c("", "", 0, 0, 0, 0, 0, 0), cb_player
)
#How To Deal With Duplicate Row Names Error In R - BioStar
# https://www.biostars.org/p/62988/
rownames(cb_player)[-c(1:2)] <- make.names(paste0(cb_player$full_name[-c(1:2)], '_', cb_player$season[-c(1:2)]),
                                           unique = T)

mf_player <- rbind(c("", "", 15, 1, 15, 3, 38, 15),
                   c("", "", 0, 0, 0, 0, 0, 0), mf_player
)
rownames(mf_player)[-c(1:2)] <- paste0(mf_player$full_name[-c(1:2)], '_', mf_player$season[-c(1:2)])

cf_player <- rbind(c("", "", 30, 3, 10, 3, 38, 10),
                   c("", "", 0, 0, 0, 0, 0, 0), cf_player
)
rownames(cf_player)[-c(1:2)] <- paste0(cf_player$full_name[-c(1:2)], '_', cf_player$season[-c(1:2)])

gk_player$conceded_overall <- as.numeric(gk_player$conceded_overall)
gk_player$conceded_per_90_overall <- as.numeric(gk_player$conceded_per_90_overall)
gk_player$appearances_overall <- as.numeric(gk_player$appearances_overall)
gk_player$clean_sheets_overall <- as.numeric(gk_player$clean_sheets_overall)

cb_player$goals_overall <- as.numeric(cb_player$goals_overall)
cb_player$assists_overall <- as.numeric(cb_player$assists_overall)
cb_player$appearances_overall <- as.numeric(cb_player$appearances_overall)
cb_player$conceded_per_90_overall <- as.numeric(cb_player$conceded_per_90_overall)
cb_player$clean_sheets_overall <- as.numeric(cb_player$clean_sheets_overall)
cb_player$card <- as.numeric(cb_player$card)

mf_player$goals_overall <- as.numeric(mf_player$goals_overall)
mf_player$goals_per_90_overall <- as.numeric(mf_player$goals_per_90_overall)
mf_player$assists_overall <- as.numeric(mf_player$assists_overall)
mf_player$assists_per_90_overall <- as.numeric(mf_player$assists_per_90_overall)
mf_player$appearances_overall <- as.numeric(mf_player$appearances_overall)
mf_player$card <- as.numeric(mf_player$card)

cf_player$goals_overall <- as.numeric(cf_player$goals_overall)
cf_player$goals_per_90_overall <- as.numeric(cf_player$goals_per_90_overall)
cf_player$assists_overall <- as.numeric(cf_player$assists_overall)
cf_player$assists_per_90_overall <- as.numeric(cf_player$assists_per_90_overall)
cf_player$appearances_overall <- as.numeric(cf_player$appearances_overall)
cf_player$card <- as.numeric(cf_player$card)

# 7) 선수 특징 프리셋
preset_player <- dbGetQuery(eplDB,
                            "SELECT
   full_name,

    CASE
      WHEN (u.position = 'Forward') THEN

            CASE
                WHEN (u.goals_overall > 9 AND u.assists_overall > 4) THEN 'Perfect'
                WHEN (u.goals_overall > 9) THEN 'Goal Hunting'
                WHEN (u.goals_per_90_overall > 0.45) THEN 'One Shot One Kill'
                WHEN (u.assists_overall > 3) THEN 'Supporting'
                WHEN (u.appearances_overall > 19) THEN 'Diligent'
                WHEN (u.appearances_overall > 9) THEN 'Ordinary'
                WHEN (u.appearances_overall > 4) THEN 'Backup'
                ELSE 'Reserve Team'
            END

      WHEN (u.position = 'Midfielder') THEN

          CASE
                WHEN(u.assists_overall >= 10 AND u.assists_per_90_overall > 0.7) THEN 'Perfect play controlling'
                WHEN(u.assists_overall >= 10) THEN 'Fantastic supporting'
                WHEN(u.assists_overall < 10 AND u.assists_overall > 0) THEN 'Helpful'
              WHEN (u.goals_overall >= 6) THEN 'Goal sentient'
                WHEN (u.appearances_overall >= 25) THEN 'Diligent'
                WHEN (u.appearances_overall < 25) and u.appearances_overall >= 10 THEN 'Ordinary'
                WHEN (u.appearances_overall < 10) THEN 'Backup'
          END

       WHEN (u.position = 'Defender') THEN

           CASE
                WHEN (u. appearances_overall > 20 AND u.conceded_per_90_overall<1.5) THEN 'The human vacuum cleaner '
                WHEN (u.conceded_per_90_overall<1.5) THEN 'Great defender'
                WHEN (u.appearances_overall > 10 AND u.clean_sheets_overall > 5) THEN 'Good defender'
                    WHEN (u.yellow_cards_overall + u.red_cards_overall < 2) THEN 'Fair player'
                ELSE 'Normal player'
        END


       WHEN (u.position = 'Goalkeeper') THEN

            CASE
                WHEN (u.appearances_overall > 19 AND u.conceded_per_90_overall < 1.0) THEN 'Fantastic Starting'
                WHEN (u.appearances_overall > 19 AND u.conceded_per_90_overall < 1.5) THEN 'Good Starting'
                WHEN (u.appearances_overall > 19) THEN 'Ordinary Starting'
                WHEN (u.appearances_overall > 2 AND u.conceded_per_90_overall < 0.9) THEN 'Fantastic Backup'
                WHEN (u.appearances_overall > 2 AND u.conceded_per_90_overall < 1.6) THEN 'Good Backup'
                WHEN (u.appearances_overall > 2) THEN 'Ordinary Back Up'
                ELSE 'Reserve Team'
            END

        ELSE 'NO DATA'

   END AS F1,

    CASE
      WHEN (u.position = 'Forward') THEN

            CASE
                WHEN (u.age > 34) THEN 'Past his prime'
                WHEN (u.age > 29) THEN 'Veteran'
                WHEN (u.age > 22) THEN 'in his Prime'
                ELSE 'Young'
            END

      WHEN (u.position = 'Midfielder') THEN

            CASE
                WHEN (u.age > 35) THEN 'Past his prime'
                WHEN (u.age > 30) THEN 'Veteran'
                WHEN (u.age > 23) THEN 'in his Prime'
                ELSE 'Young'
            END

       WHEN (u.position = 'Defender') THEN

            CASE
                WHEN (u.age > 35) THEN 'Past his prime'
                WHEN (u.age > 31) THEN 'Veteran'
                WHEN (u.age > 25) THEN 'in his Prime'
                ELSE 'Young'
            END

       WHEN (u.position = 'Goalkeeper') THEN

            CASE
                WHEN (u.age > 39) THEN 'Past his prime'
                WHEN (u.age > 34) THEN 'Veteran'
                WHEN (u.age > 26) THEN 'in his Prime'
                ELSE 'Young'
            END

        ELSE 'NO DATA'
   END AS F2,



   CASE
      WHEN (u.position = 'Forward') THEN 'Forward'

      WHEN (u.position = 'Midfielder') THEN 'Midfielder'

       WHEN (u.position = 'Defender') THEN 'Defender'

       WHEN (u.position = 'Goalkeeper') THEN 'Goalkeeper'

        ELSE 'NO DATA'

   END AS F3

FROM `epl_players_2018_2019` u;")


# 7-2) 팀 특징 프리셋
preset_team <- dbGetQuery(eplDB,
                          "SELECT
	common_name,

    CASE
        WHEN (u.shots > 350 ) THEN 'Aggressive'
		WHEN (u.shots < 300 ) THEN 'Deliberate'
        WHEN (u.fouls > 425) THEN 'Rough'
        ELSE 'Gentle'
	END AS F1,

    CASE

        WHEN (u.average_possession > 55 ) THEN 'Possession Football'
        WHEN (u.average_possession > 45 AND u.goals_scored > 50) THEN 'Attacking Football'
        WHEN (u.average_possession > 45 AND u.goals_scored > 40) THEN 'Balanced Football'
        ELSE 'Counterattack Football'

	END AS F2,

	CASE
		WHEN (u.league_position < 4 ) THEN 'UCL qualified'
        WHEN (u.league_position < 6 ) THEN 'UEL qualified'
        WHEN (u.league_position > 17) THEN 'Relegated'
        ELSE 'Remaining'

	END AS F3

FROM `epl_teams_2018_2019` u;")


# 8) 리그 트렌드
league <- dbGetQuery(eplDB,
                     "SELECT
	name,

    CASE
        WHEN (u.average_goals_per_match > 2.9 ) THEN 'Aggressive'
		WHEN (u.average_goals_per_match > 2.6 ) THEN 'Balanced'
        ELSE 'Defensive'
	END AS Aggressiveness,

    CASE

        WHEN (u.average_cards_per_match > 4.5 ) THEN 'Very Rough'
        WHEN (u.average_cards_per_match > 3.5 ) THEN 'Rough'
        WHEN (u.average_cards_per_match > 3.0 ) THEN 'Average Rough'
        ELSE 'Gentle'

	END AS Roughness,

	CASE
		WHEN (u.prediction_risk > 64 ) THEN 'Hard to predict'
        WHEN (u.prediction_risk > 49 ) THEN 'Possible to predict'
	    ELSE 'Easy to predict'
	END AS Predict,

    CASE
		WHEN (u.btts_percentage > 50 ) THEN 'Games with many goals'
        WHEN (u.btts_percentage > 40 ) THEN 'Games with one or two goals'
	    ELSE 'Games with one or no goal'
	END AS Goals

FROM `epl_leagues_2014_2015` u;")
league <- cbind(league, season = "2014_2015")

league <- rbind(league, cbind(dbGetQuery(eplDB,
                                         "SELECT
	name,

    CASE
        WHEN (u.average_goals_per_match > 2.9 ) THEN 'Aggressive'
		WHEN (u.average_goals_per_match > 2.6 ) THEN 'Balanced'
        ELSE 'Defensive'
	END AS Aggressiveness,

    CASE

        WHEN (u.average_cards_per_match > 4.5 ) THEN 'Very Rough'
        WHEN (u.average_cards_per_match > 3.5 ) THEN 'Rough'
        WHEN (u.average_cards_per_match > 3.0 ) THEN 'Average Rough'
        ELSE 'Gentle'

	END AS Roughness,

	CASE
		WHEN (u.prediction_risk > 64 ) THEN 'Hard to predict'
        WHEN (u.prediction_risk > 49 ) THEN 'Possible to predict'
	    ELSE 'Easy to predict'
	END AS Predict,

    CASE
		WHEN (u.btts_percentage > 50 ) THEN 'Games with many goals'
        WHEN (u.btts_percentage > 40 ) THEN 'Games with one or two goals'
	    ELSE 'Games with one or no goal'
	END AS Goals

FROM `epl_leagues_2015_2016` u;"), season = "2015_2016"))

league <- rbind(league, cbind(dbGetQuery(eplDB,
                                         "SELECT
	name,

    CASE
        WHEN (u.average_goals_per_match > 2.9 ) THEN 'Aggressive'
		WHEN (u.average_goals_per_match > 2.6 ) THEN 'Balanced'
        ELSE 'Defensive'
	END AS Aggressiveness,

    CASE

        WHEN (u.average_cards_per_match > 4.5 ) THEN 'Very Rough'
        WHEN (u.average_cards_per_match > 3.5 ) THEN 'Rough'
        WHEN (u.average_cards_per_match > 3.0 ) THEN 'Average Rough'
        ELSE 'Gentle'

	END AS Roughness,

	CASE
		WHEN (u.prediction_risk > 64 ) THEN 'Hard to predict'
        WHEN (u.prediction_risk > 49 ) THEN 'Possible to predict'
	    ELSE 'Easy to predict'
	END AS Predict,

    CASE
		WHEN (u.btts_percentage > 50 ) THEN 'Games with many goals'
        WHEN (u.btts_percentage > 40 ) THEN 'Games with one or two goals'
	    ELSE 'Games with one or no goal'
	END AS Goals

FROM `epl_leagues_2016_2017` u;"), season = "2016_2017"))

league <- rbind(league, cbind(dbGetQuery(eplDB,
                                         "SELECT
	name,

    CASE
        WHEN (u.average_goals_per_match > 2.9 ) THEN 'Aggressive'
		WHEN (u.average_goals_per_match > 2.6 ) THEN 'Balanced'
        ELSE 'Defensive'
	END AS Aggressiveness,

    CASE

        WHEN (u.average_cards_per_match > 4.5 ) THEN 'Very Rough'
        WHEN (u.average_cards_per_match > 3.5 ) THEN 'Rough'
        WHEN (u.average_cards_per_match > 3.0 ) THEN 'Average Rough'
        ELSE 'Gentle'

	END AS Roughness,

	CASE
		WHEN (u.prediction_risk > 64 ) THEN 'Hard to predict'
        WHEN (u.prediction_risk > 49 ) THEN 'Possible to predict'
	    ELSE 'Easy to predict'
	END AS Predict,

    CASE
		WHEN (u.btts_percentage > 50 ) THEN 'Games with many goals'
        WHEN (u.btts_percentage > 40 ) THEN 'Games with one or two goals'
	    ELSE 'Games with one or no goal'
	END AS Goals

FROM `epl_leagues_2017_2018` u;"), season = "2017_2018"))

league <- rbind(league, cbind(dbGetQuery(eplDB,
                                         "SELECT
	name,

    CASE
        WHEN (u.average_goals_per_match > 2.9 ) THEN 'Aggressive'
		WHEN (u.average_goals_per_match > 2.6 ) THEN 'Balanced'
        ELSE 'Defensive'
	END AS Aggressiveness,

    CASE

        WHEN (u.average_cards_per_match > 4.5 ) THEN 'Very Rough'
        WHEN (u.average_cards_per_match > 3.5 ) THEN 'Rough'
        WHEN (u.average_cards_per_match > 3.0 ) THEN 'Average Rough'
        ELSE 'Gentle'

	END AS Roughness,

	CASE
		WHEN (u.prediction_risk > 64 ) THEN 'Hard to predict'
        WHEN (u.prediction_risk > 49 ) THEN 'Possible to predict'
	    ELSE 'Easy to predict'
	END AS Predict,

    CASE
		WHEN (u.btts_percentage > 50 ) THEN 'Games with many goals'
        WHEN (u.btts_percentage > 40 ) THEN 'Games with one or two goals'
	    ELSE 'Games with one or no goal'
	END AS Goals

FROM `epl_leagues_2018_2019` u;"), season = "2018_2019"))

league <- rbind(league, cbind(dbGetQuery(eplDB,
                                         "SELECT
	name,

    CASE
        WHEN (u.average_goals_per_match > 2.9 ) THEN 'Aggressive'
		WHEN (u.average_goals_per_match > 2.6 ) THEN 'Balanced'
        ELSE 'Defensive'
	END AS Aggressiveness,

    CASE

        WHEN (u.average_cards_per_match > 4.5 ) THEN 'Very Rough'
        WHEN (u.average_cards_per_match > 3.5 ) THEN 'Rough'
        WHEN (u.average_cards_per_match > 3.0 ) THEN 'Average Rough'
        ELSE 'Gentle'

	END AS Roughness,

	CASE
		WHEN (u.prediction_risk > 64 ) THEN 'Hard to predict'
        WHEN (u.prediction_risk > 49 ) THEN 'Possible to predict'
	    ELSE 'Easy to predict'
	END AS Predict,

    CASE
		WHEN (u.btts_percentage > 50 ) THEN 'Games with many goals'
        WHEN (u.btts_percentage > 40 ) THEN 'Games with one or two goals'
	    ELSE 'Games with one or no goal'
	END AS Goals

FROM `epl_leagues_2019_2020` u;"), season = "2019_2020"))
league <- league[-c(2, 7), ]

# 9) 리그 트렌드 숫자
league_stat <- cbind(dbGetQuery(eplDB,
                                "select name, average_goals_per_match, average_cards_per_match,
      prediction_risk, btts_percentage from epl_leagues_2014_2015 limit 1;"),
                     season = "2015")

league_stat <- rbind(league_stat, cbind(dbGetQuery(eplDB,
                                                   "select name, average_goals_per_match, average_cards_per_match,
      prediction_risk, btts_percentage from epl_leagues_2015_2016 limit 1;"),
                                        season = "2016"))

league_stat <- rbind(league_stat, cbind(dbGetQuery(eplDB,
                                                   "select name, average_goals_per_match, average_cards_per_match,
      prediction_risk, btts_percentage from epl_leagues_2016_2017 limit 1;"),
                                        season = "2017"))

league_stat <- rbind(league_stat, cbind(dbGetQuery(eplDB,
                                                   "select name, average_goals_per_match, average_cards_per_match,
      prediction_risk, btts_percentage from epl_leagues_2017_2018 limit 1;"),
                                        season = "2018"))

league_stat <- rbind(league_stat, cbind(dbGetQuery(eplDB,
                                                   "select name, average_goals_per_match, average_cards_per_match,
      prediction_risk, btts_percentage from epl_leagues_2018_2019 limit 1;"),
                                        season = "2019"))

league_stat <- rbind(league_stat, cbind(dbGetQuery(eplDB,
                                                   "select name, average_goals_per_match, average_cards_per_match,
      prediction_risk, btts_percentage from epl_leagues_2019_2020 limit 1;"),
                                        season = "2020"))
# league_stat <- league_stat[-c(2, 7), ]

# 10) 선수 국적
nation <- dbGetQuery(
  eplDB,
  "select nationality, count(nationality) as cnt_nation from epl_players_2014_2015
group by nationality order by cnt_nation desc;")
colnames(nation)[2] <- "2015"
nation <- nation[-c(11, 54), ]

temp <- dbGetQuery(
  eplDB,
  "select nationality, count(nationality) as cnt_nation from epl_players_2015_2016
group by nationality order by cnt_nation desc;")
colnames(temp)[2] <- "2016"
nation <- merge(nation, temp, by = "nationality")

temp <- dbGetQuery(
  eplDB,
  "select nationality, count(nationality) as cnt_nation from epl_players_2016_2017
group by nationality order by cnt_nation desc;")
colnames(temp)[2] <- "2017"
nation <- merge(nation, temp, by = "nationality")

temp <- dbGetQuery(
  eplDB,
  "select nationality, count(nationality) as cnt_nation from epl_players_2017_2018
group by nationality order by cnt_nation desc;")
colnames(temp)[2] <- "2018"
nation <- merge(nation, temp, by = "nationality")

temp <- dbGetQuery(
  eplDB,
  "select nationality, count(nationality) as cnt_nation from epl_players_2018_2019
group by nationality order by cnt_nation desc;")
colnames(temp)[2] <- "2019"
nation <- merge(nation, temp, by = "nationality")

temp <- dbGetQuery(
  eplDB,
  "select nationality, count(nationality) as cnt_nation from epl_players_2019_2020
group by nationality order by cnt_nation desc;")
colnames(temp)[2] <- "2020"
nation <- merge(nation, temp, by = "nationality")

nation$`2015` <- round(nation$`2015` / sum(nation$`2015`) * 100, 2)
nation$`2016` <- round(nation$`2016` / sum(nation$`2016`) * 100, 2)
nation$`2017` <- round(nation$`2017` / sum(nation$`2017`) * 100, 2)
nation$`2018` <- round(nation$`2018` / sum(nation$`2018`) * 100, 2)
nation$`2019` <- round(nation$`2019` / sum(nation$`2019`) * 100, 2)
nation$`2020` <- round(nation$`2020` / sum(nation$`2020`) * 100, 2)

nation <- nation %>% filter(nation$`2015` > 0.5 & nation$`2016` > 0.5)

nation <- nation %>% gather(season, num, -nationality)
nation$season <- as.numeric(nation$season)

# 11) 골결정력이 좋은 팀
eff_goal <- dbGetQuery(
  eplDB,
  "select h.home_team_name, (h.on_target+a.on_target)/(h.shots+a.shots)*100 as on_target
from (select home_team_name, sum(home_team_shots_on_target) as on_target,
                       sum(home_team_shots_on_target)+sum(home_team_shots_off_target) as shots
from epl_matches_2014_2015 group by home_team_name) as h
join
(select away_team_name, sum(away_team_shots_on_target) as on_target,
                       sum(away_team_shots_on_target)+sum(away_team_shots_off_target) as shots
from epl_matches_2014_2015 group by away_team_name) as a
on h.home_team_name = a.away_team_name order by on_target desc;")
colnames(eff_goal)[2] <- "2015"

temp <- dbGetQuery(
  eplDB,
  "select h.home_team_name, (h.on_target+a.on_target)/(h.shots+a.shots)*100 as on_target
from (select home_team_name, sum(home_team_shots_on_target) as on_target,
                       sum(home_team_shots_on_target)+sum(home_team_shots_off_target) as shots
from epl_matches_2015_2016 group by home_team_name) as h
join
(select away_team_name, sum(away_team_shots_on_target) as on_target,
                       sum(away_team_shots_on_target)+sum(away_team_shots_off_target) as shots
from epl_matches_2015_2016 group by away_team_name) as a
on h.home_team_name = a.away_team_name order by on_target desc;")
colnames(temp)[2] <- "2016"
eff_goal <- merge(eff_goal, temp, by="home_team_name", all = T)

temp <- dbGetQuery(
  eplDB,
  "select h.home_team_name, (h.on_target+a.on_target)/(h.shots+a.shots)*100 as on_target
from (select home_team_name, sum(home_team_shots_on_target) as on_target,
                       sum(home_team_shots_on_target)+sum(home_team_shots_off_target) as shots
from epl_matches_2016_2017 group by home_team_name) as h
join
(select away_team_name, sum(away_team_shots_on_target) as on_target,
                       sum(away_team_shots_on_target)+sum(away_team_shots_off_target) as shots
from epl_matches_2016_2017 group by away_team_name) as a
on h.home_team_name = a.away_team_name order by on_target desc;")
colnames(temp)[2] <- "2017"
eff_goal <- merge(eff_goal, temp, by="home_team_name", all = T)

temp <- dbGetQuery(
  eplDB,
  "select h.home_team_name, (h.on_target+a.on_target)/(h.shots+a.shots)*100 as on_target
from (select home_team_name, sum(home_team_shots_on_target) as on_target,
                       sum(home_team_shots_on_target)+sum(home_team_shots_off_target) as shots
from epl_matches_2017_2018 group by home_team_name) as h
join
(select away_team_name, sum(away_team_shots_on_target) as on_target,
                       sum(away_team_shots_on_target)+sum(away_team_shots_off_target) as shots
from epl_matches_2017_2018 group by away_team_name) as a
on h.home_team_name = a.away_team_name order by on_target desc;")
colnames(temp)[2] <- "2018"
eff_goal <- merge(eff_goal, temp, by="home_team_name", all = T)

temp <- dbGetQuery(
  eplDB,
  "select h.home_team_name, (h.on_target+a.on_target)/(h.shots+a.shots)*100 as on_target
from (select home_team_name, sum(home_team_shots_on_target) as on_target,
                       sum(home_team_shots_on_target)+sum(home_team_shots_off_target) as shots
from epl_matches_2018_2019 group by home_team_name) as h
join
(select away_team_name, sum(away_team_shots_on_target) as on_target,
                       sum(away_team_shots_on_target)+sum(away_team_shots_off_target) as shots
from epl_matches_2018_2019 group by away_team_name) as a
on h.home_team_name = a.away_team_name order by on_target desc;")
colnames(temp)[2] <- "2019"
eff_goal <- merge(eff_goal, temp, by="home_team_name", all = T)

temp <- dbGetQuery(
  eplDB,
  "select h.home_team_name, (h.on_target+a.on_target)/(h.shots+a.shots)*100 as on_target
from (select home_team_name, sum(home_team_shots_on_target) as on_target,
                       sum(home_team_shots_on_target)+sum(home_team_shots_off_target) as shots
from epl_matches_2019_2020 group by home_team_name) as h
join
(select away_team_name, sum(away_team_shots_on_target) as on_target,
                       sum(away_team_shots_on_target)+sum(away_team_shots_off_target) as shots
from epl_matches_2019_2020 group by away_team_name) as a
on h.home_team_name = a.away_team_name order by on_target desc;")
colnames(temp)[2] <- "2020"
eff_goal <- merge(eff_goal, temp, by="home_team_name", all = T)

eff_goal <- eff_goal %>% gather(season, ratio, -home_team_name)
eff_goal$season <- as.numeric(eff_goal$season)


# 12) 기회창출
goal_involved <- cbind(dbGetQuery(
  eplDB,
  "select full_name, `Current.Club` as club, goals_involved_per_90_overall from epl_players_2019_2020
where appearances_overall >= 10
order by goals_involved_per_90_overall desc limit 10;"), season = 2020)

temp <- cbind(dbGetQuery(
  eplDB,
  "select full_name, `Current.Club` as club, goals_involved_per_90_overall from epl_players_2018_2019
where appearances_overall >= 10
order by goals_involved_per_90_overall desc limit 10;"), season = 2019)
goal_involved <- rbind(temp, goal_involved)

temp <- cbind(dbGetQuery(
  eplDB,
  "select full_name, `Current.Club` as club, goals_involved_per_90_overall from epl_players_2017_2018
where appearances_overall >= 10
order by goals_involved_per_90_overall desc limit 10;"), season = 2018)
goal_involved <- rbind(temp, goal_involved)

temp <- cbind(dbGetQuery(
  eplDB,
  "select full_name, `Current.Club` as club, goals_involved_per_90_overall from epl_players_2016_2017
where appearances_overall >= 10
order by goals_involved_per_90_overall desc limit 10;"), season = 2017)
goal_involved <- rbind(temp, goal_involved)

temp <- cbind(dbGetQuery(
  eplDB,
  "select full_name, `Current.Club` as club, goals_involved_per_90_overall from epl_players_2015_2016
where appearances_overall >= 10
order by goals_involved_per_90_overall desc limit 10;"), season = 2016)
goal_involved <- rbind(temp, goal_involved)

temp <- cbind(dbGetQuery(
  eplDB,
  "select full_name, `Current.Club` as club, goals_involved_per_90_overall from epl_players_2014_2015
where appearances_overall >= 10
order by goals_involved_per_90_overall desc limit 10;"), season = 2015)
goal_involved <- rbind(temp, goal_involved)
colnames(goal_involved) <- c("name", "team", "goals_involved", "season")


