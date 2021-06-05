library(tidyverse)
library(nbastatR)
library(lubridate)
library(httr)
library(hablar)
library(jsonlite)
library(gt)

headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://www.nba.com/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

get_dataPLAYER <- function(Season) {
  url <-
    paste0(
      "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=",
      Season,
      "-",
      sprintf('%02d', (Season + 1) %% 100),
      "&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
    )
  
  res <- GET(url = url, add_headers(.headers = headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  df <- df %>% 
    mutate(Season = Season + 1)
  
  return(df)
  
}

df_data <- 
  map_df(2019:2020, get_dataPLAYER) %>%
  retype()

get_dataPLAYERpbp <- function(Season) {
  
  url <- paste0("https://api.pbpstats.com/get-totals/nba?Season=", 
                Season,
                "-",
                sprintf('%02d', (Season + 1) %% 100),
                "&SeasonType=Regular%2BSeason&StartType=All&Type=Player"
  )
  
  res = httr::GET(url)
  json_resp <- jsonlite::fromJSON(content(res, "text", encoding = "UTF-8"))
  frame <- data.frame(json_resp$multi_row_table_data)
  df <- frame %>% 
    mutate(Season = Season + 1)
  
}

df_pbp <-
  map_df(2019:2020, get_dataPLAYERpbp) %>% 
  retype()

df <- inner_join(df_data, df_pbp, by = c("PLAYER_ID" = "EntityId", "Season" = "Season"))

df_t <- df %>% 
  group_by(Season) %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0))) %>% 
  filter(Minutes > 500) %>% 
  mutate(scoringusg = 100*(FG2A + FG3A + FTA*0.44)/OffPoss,
         turnoverusg = (100*Turnovers)/OffPoss,
         playmakingusg = 100*(POTENTIAL_AST + FT_AST)/OffPoss,
         totalusg = scoringusg + turnoverusg + playmakingusg
  ) %>% 
  select(Season, PLAYER_NAME, TeamAbbreviation, totalusg, scoringusg, playmakingusg, turnoverusg, TsPct) %>% 
  pivot_wider(names_from = Season, values_from = c(TeamAbbreviation:TsPct)) %>% 
  mutate(difftotalusg = totalusg_2021 - totalusg_2020,
         diffscoringusg = scoringusg_2021 - scoringusg_2020,
         diffplaymakingusg = playmakingusg_2021 - playmakingusg_2020,
         diffturnoverusg = turnoverusg_2021 - turnoverusg_2020,
         difftrueshooting = TsPct_2021 - TsPct_2020
  ) %>% 
  filter_if(is.numeric, all_vars((.) != 0)) %>% 
  arrange(desc(difftotalusg))

nbastatR::assign_nba_players()
playerlist = df_dict_nba_players %>% 
  select(namePlayer, urlPlayerHeadshot)

df_t %>%
  select(1:14) %>% 
  left_join(playerlist, by = c("PLAYER_NAME" = "namePlayer")) %>% 
  select(urlPlayerHeadshot, everything()) %>% 
  slice(which(
    difftotalusg %in% head(difftotalusg, n = 10) |
      difftotalusg %in% tail(difftotalusg, n = 10)
  )) %>%
  distinct() %>%
  gt() %>%
  text_transform(
    locations = cells_body(
      vars(urlPlayerHeadshot)
    ),
    fn = function(x) {
      web_image(
        url = x,
        height = 25
      )
    }
  ) %>% 
  tab_header(
    title = md("**Total Usage Comparisons**"),
    subtitle = md(
      "Among players that played at least 500 minutes in last two seasons"
    )
  ) %>%
  cols_label(
    PLAYER_NAME = "Player",
    TeamAbbreviation_2020 = "2019-20 Team",
    TeamAbbreviation_2021 = "2020-21 Team",
    totalusg_2021 = "TOTAL",
    totalusg_2020 = "TOTAL",
    difftotalusg = "Change in USG",
    scoringusg_2021 = "Scoring",
    scoringusg_2020 = "Scoring",
    playmakingusg_2021 = "Playmaking",
    playmakingusg_2020 = "Playmaking",
    turnoverusg_2021 = "Turnover",
    turnoverusg_2020 = "Turnover",
    TsPct_2021 = "TS%",
    TsPct_2020 = "TS%",
    urlPlayerHeadshot = ""
  ) %>%
  tab_spanner(
    label =  gt::html("<span style='font-weight:bold;font-size:12px'>Usage in 2020-21</span>"),
    columns = vars(scoringusg_2021, playmakingusg_2021, turnoverusg_2021, totalusg_2021, TsPct_2021)
  ) %>% 
  tab_spanner(
    label =  gt::html("<span style='font-weight:bold;font-size:12px'>Usage in 2019-20</span>"),
    columns = vars(scoringusg_2020, playmakingusg_2020, turnoverusg_2020, totalusg_2020, TsPct_2020)
  ) %>% 
  fmt_percent(columns = vars(TsPct_2020, TsPct_2021),
              decimals = 1) %>%
  fmt_number(columns = contains(c("usg")), 
             decimals = 2) %>% 
  cols_width(1 ~ px(20)) %>% 
  cols_width(3:4 ~ px(45)) %>%
  cols_width(15 ~ px(60)) %>%
  #cols_width(5:14 ~ px(60)) %>%
  #opt_row_striping() %>%
  tab_options(
    table.background.color = "white",
    column_labels.font.size = 12,
    column_labels.font.weight = 'bold',
    row_group.font.weight = 'bold',
    row_group.background.color = "#E5E1D8",
    table.font.size = 11,
    heading.title.font.size = 22,
    heading.subtitle.font.size = 10,
    table.font.names = "Franklin Gothic Medium",
    data_row.padding = px(2),
    footnotes.padding = px(.5)
  )  %>%
  tab_source_note(source_note = md("Data Source: NBA.com/stats | pbpstats.com<br>Methodology: Seth Partnow<br>Table: @burakcankoc")) %>%
  data_color(
    columns = vars(difftotalusg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(palette = "Redmonder::dPBIRdGn",
                                       direction = 1) %>% as.character(),
      domain = NULL,
      na.color = "#005C55FF"
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(TsPct_2020)
      )
    )
  ) %>%
  gtsave("usage.png")