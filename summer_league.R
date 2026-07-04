library(dplyr)
library(hoopR)
library(janitor)
library(hablar)
library(gt)
library(gtExtras)
library(gtUtils)
library(httr)
library(jsonlite)


twitter <- "<span style='color:#b14f04'>&#x1D54F;</span>"
tweetelcheff <- "<span style='font-weight:bold;color: grey;'>*@elcheff*</span>"
insta <- "<span style='color:#E1306C;font-family: \"Font Awesome 6 Brands\"'>&#xE055;</span>"
instaelcheff <- "<span style='font-weight:bold;color: grey;'>*@sport_iv0*</span>"
github <- "<span style='color:#c8102e;font-family: \"Font Awesome 6 Brands\"'>&#xF092;</span>"
githubelcheff <- "<span style='font-weight:bold;color: grey;'>*IvoVillanueva*</span>"
caption <- glue::glue("**Datos**: *@NBA* | **Gráfico**: *Ivo Villanueva* • {twitter} {tweetelcheff} • The Clean Shot")


headers <- c(
  "accept" = "*/*",
  "accept-encoding" = "gzip, deflate, br, zstd",
  "accept-language" = "es-ES,es;q=0.9,en;q=0.8",
  "cache-control" = "no-cache",
  "connection" = "keep-alive",
  "host" = "stats.nba.com",
  "origin" = "https://www.nba.com",
  "pragma" = "no-cache",
  "referer" = "https://www.nba.com/",
  "sec-ch-ua" = "\"Google Chrome\";v=\"149\", \"Chromium\";v=\"149\", \"Not)A;Brand\";v=\"24\"",
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = "\"macOS\"",
  "sec-fetch-dest" = "empty",
  "sec-fetch-mode" = "cors",
  "sec-fetch-site" = "same-site",
  "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36"
)

url <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=13&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2026&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))


df <- as_tibble(json_resp$resultSets$rowSet[[1]],
                .name_repair = ~json_resp$resultSets$headers[[1]]) %>%
retype() %>%
  clean_names() %>%
   transmute(logo = ifelse(team_abbreviation == "GWG", "GSW",team_abbreviation ),
          logo = paste0("https://raw.githubusercontent.com/IvoVillanueva/NBA/refs/heads/main/logos_cuadrados/",logo, ".png"),
          player_name,
          min,
          pts,
          reb,
          ast,
          tov,
          stl,
          blk,
          pf,
          fgm,
          fga,
          fg3m,
          fg3a,
          ftm,
          fta,
          plus_minus,
          dre = (.79*pts-.72*(fga-fg3a)-.55*fg3a-.16*fta+.13*oreb+.40*dreb+.54*ast+1.68*stl+.76*blk-1.36*tov-.11*pf)
   ) %>%
   mutate(fgm = paste0(fgm, "/", fga),
          fg3m = paste0(fg3m, "/", fg3a),
          ftm = paste0(ftm, "/", fta)) %>%
   arrange(desc(dre)) %>%
   # take top 20
   filter(row_number() <= 20) %>%
  write.csv(paste0("data/mvp_odds_", gsub("-", "_", today), ".csv"), row.names = F)

 p %>%
   gt() %>%
   tab_header(title = md("**Best of Summer League Day One**"),
              subtitle = "Los 20 mejores en RAPM Diario Estimado (DRE) en el primer día de la Summer League de Las Vegas"
   ) %>%
   tab_source_note(
     source_note = md(caption)
   ) %>%
   cols_label(logo = "",
              player_name = "Player",
              min = "MIN",
              pts = "PTS",
              reb = "REB",
              ast = "AST",
              tov = "TOV",
              stl = "STL",
              blk = "BLK",
              fgm = "FGM/A",
              fg3m = "3PM/A",
              ftm = "FTM/A",
              plus_minus = "+/-",
              dre = "DRE") %>%
   fmt_number(plus_minus, force_sign = T, decimals = 0) %>%
   fmt_number(dre, decimals = 1) %>%
   gt_img_rows(columns = logo) %>%
   data_color(columns = "dre",
              alpha = .75,
              reverse = F,
              palette = "ggsci::blue_grey_material") %>%
   tab_options(data_row.padding = '0px',
               table.font.names = "Bebas Neue",
               table_body.hlines.color = "transparent",
               column_labels.border.top.color = 'black',
               column_labels.border.top.width = px(1),
               column_labels.border.bottom.style = 'none',
               column_labels.font.weight = "strong",
               row_group.border.top.style = "none",
               row_group.border.top.color = "black",
               row_group.border.bottom.width = px(1),
               row_group.border.bottom.color = "black",
               row_group.border.bottom.style = "solid",
               row_group.padding = px(1.5),
               heading.align = 'left',
               heading.border.bottom.style = "none",
               table_body.border.top.style = "none",
               table_body.border.bottom.color = "white",
               table.border.bottom.style = 'none',
               table.border.top.style = 'none',
               source_notes.border.lr.style = "none")%>%
   # save table
   gt_save_crop("summer_league_r_tbl.png")
