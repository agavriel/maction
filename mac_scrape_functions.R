# Trying to build this with as much base R so it's easy to share?

# Example game link for testing
# game_link <- "https://getsomemaction.com/boxscore.aspx?id=7vMPRmSIuvynng5chC8YNX4BEXgL8bHxFn%2bcr2MWdWFEPr87rccH7XxDnAWwNIRGFd6A6C%2bON36SNLKNU37CG2kO8JUv%2fXaPUGW0pi4K%2fbAZuxRKyTQM9k5DAiXQ3ze7&path=mbball"

# Box Score Tab -----------------------------------------------------------

# Testing link (comment out)
#game_link <- "https://getsomemaction.com/boxscore.aspx?id=7Gx48kkB20iPlOppZhxXHy%2f5Zy4VtNXkGzuHdKHXRgkOw0pduZ0zZ9dLA8PyalFluCBCpHtNipVxMoZWikX6f1YoKMDDbHxGojPWtEt5AUwstjJSsrsi8OtZwbVHZlleL6fEKk%2b5vp2obH5V98CWPw%3d%3d&path=mbball"

process_box <- function(game_link){
  
  # Reads it in as a long html source rather than large list
  full_raw <- try(unlist(strsplit(gsub("\\t", "", gsub("\\r", "", httr::GET(game_link))), "\n")), TRUE)
  
  # I think Cancun is messing with the loop for some reason I can't figure out
  # remove from the equation since it's a neutral site game anyway?
  full_raw <- gsub("Hard Rock Cancun, Cancún, MEX", "Neutral Site", full_raw)
  
  # Index the rows in the html to ID where the box score begins and ends
  box_row_start <- grep('<section id="box-score">', full_raw) + 0 # +0 to make numeric
  box_row_end <- grep('<section id="play-by-play"', full_raw) - 1 # -1 to get row before play by play
  
  # Filter full_raw for just these rows
  box_raw <- full_raw[box_row_start:box_row_end]
  
  # Find the rows in box_raw with the team headers
  box_team_start <- grep('<h3 class="sub-heading">', box_raw) + 0 # +0 to make numeric
  
  ## Team 1 (Away team?) --------------------------------------------------
  
  # Filter box_raw for these rows which will be team 1 (usually away team?)
  box_raw_team1 <- box_raw[box_team_start[1]:box_team_start[2]-1]
  
  # Start picking out the pieces we need (might need to trim this later)
  # Everything ID'd as unintelligble single letters for the subsetting into
  # box_working below
  box_raw_1 <- gsub('.*<td class="hide-on-medium-down"> *([0-9]{1,2})</td>.*', "A;\\1", box_raw_team1) # Player jersey number
  box_raw_2 <- gsub(".*</span> ([A-Za-zÀ-ÖØ-öø-ÿ' \\.\\(\\),-]{1,})</th>", "B;\\1", box_raw_1) # player name
  box_raw_3 <- gsub('.*<td class="text-center hide-on-medium-down">(\\*)</td>.*', "O;\\1", box_raw_2) # Starter flag
  box_raw_3 <- gsub('.*<td class="text-center hide-on-medium-down"></td>.*', "O;0", box_raw_3) # Non-Starter flag
  box_raw_4 <- gsub('.* data-label="MIN">([+0-9]{1,2})</td>.*', "C;\\1", box_raw_3) # Player minutes played
  box_raw_4 <- gsub("\\+", "", box_raw_4) # Remove instances where player minutes is 0+ for numeric effectiveness
  box_raw_5 <- gsub('.* data-label="FG">([0-9]{1,2}[-][0-9]{1,2})</td>.*', "D;\\1", box_raw_4) # shots attempted and made
  box_raw_6 <- gsub('.* data-label="3PT">([0-9]{1,2}[-][0-9]{1,2})</td>.*', "E;\\1", box_raw_5) # 3pt attempted and made
  box_raw_7 <- gsub('.* data-label="FT">([0-9]{1,2}[-][0-9]{1,2})</td>.*', "F;\\1", box_raw_6) # FT attempted and made
  box_raw_8 <- gsub('.* data-label="ORB-DRB">([0-9]{1,2}[-][0-9]{1,2})</td>.*', "G;\\1", box_raw_7) # Rebound splits
  box_raw_9 <- gsub('.* data-label="REB">([0-9]{1,2})</td>.*', "H;\\1", box_raw_8) # Total rebounds
  box_raw_10 <- gsub('.* data-label="PF">([0-9]{1,2})</td>.*', "I;\\1", box_raw_9) # Fouls
  box_raw_11 <- gsub('.* data-label="A">([0-9]{1,2})</td>.*', "J;\\1", box_raw_10) # Assists
  box_raw_12 <- gsub('.* data-label="TO">([0-9]{1,2})</td>.*', "K;\\1", box_raw_11) # Turnovers
  box_raw_13 <- gsub('.* data-label="BLK">([0-9]{1,2})</td>.*', "L;\\1", box_raw_12) # Blocks
  box_raw_14 <- gsub('.* data-label="STL">([0-9]{1,2})</td>.*', "M;\\1", box_raw_13) # Steals
  box_raw_15 <- gsub('.* data-label="PTS">([0-9]{1,2})</td>.*', "N;\\1", box_raw_14) # Points
  
  # Subset box_raw_15 to remove any lines that start with ' ' and blanks
  box_working <- box_raw_15[substr(box_raw_15, 1, 1) != " "]
  box_working <- box_working[substr(box_working, 1, 1) != ""]
  
  # Build what will be the df
  metric_id <- substr(box_working, 1, 1) #Grabs the letter ID from above
  metric <- substr(box_working,3,nchar(box_working)) #Grabs the metric from the letter
  lm <- length(metric_id) 
  metric_id_1 <- rbind(metric_id[1:(lm-14)], 
                       metric_id[2:(lm-13)], 
                       metric_id[3:(lm-12)],
                       metric_id[4:(lm-11)],
                       metric_id[5:(lm-10)],
                       metric_id[6:(lm-9)],
                       metric_id[7:(lm-8)],
                       metric_id[8:(lm-7)],
                       metric_id[9:(lm-6)],
                       metric_id[10:(lm-5)],
                       metric_id[11:(lm-4)],
                       metric_id[12:(lm-3)],
                       metric_id[13:(lm-2)],
                       metric_id[14:(lm-1)],
                       metric_id[15:lm]
                       )
  sts <- which(apply(metric_id_1 == c("A","B", "O", "C", "D", "E",
                                      "F", "G", "H", "I", "J",
                                      "K", "L", "M", "N"), 2, prod)>0)
  
  # Grab team name from box_raw
  team_name <- box_raw_team1[2]
  team_name <- gsub('<h3 class="sub-heading">([A-Za-z \\.\\(\\),-]{1,})[ \t].*', "P;\\1", team_name) # team name
  team_name <- trimws(team_name, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") # trim leading space
  team_name <- substr(team_name,3,nchar(team_name)) #Grabs the team name
  
  # Build the data frame from the sts key above that counts every 14 digis
  box_score_team1 <- data.frame(number=metric[sts],
                          player = metric[sts+1],
                          starter = metric[sts+2],
                          minutes = metric[sts+3],
                          FGM_FGA = metric[sts+4],
                          threeFGM_threeFGA = metric[sts+5],
                          FTM_FTA = metric[sts+6],
                          OREB_DREB = metric[sts+7],
                          reb = metric[sts+8],
                          fouls = metric[sts+9],
                          assists = metric[sts+10],
                          turnovers = metric[sts+11],
                          blocks = metric[sts+12],
                          steals = metric[sts+13],
                          points = metric[sts+14],
                          team = team_name
                          )
  
  ## Team 2 (Home team?) --------------------------------------------------
  
  # Filter box_raw for these rows which will be team 1 (usually away team?)
  box_raw_team2 <- box_raw[box_team_start[2]:length(box_raw)]
  
  # Start picking out the pieces we need (might need to trim this later)
  # Everything ID'd as unintelligble single letters for the subsetting into
  # box_working below
  box_raw_1 <- gsub('.*<td class="hide-on-medium-down"> *([0-9]{1,2})</td>.*', "A;\\1", box_raw_team2) # Player jersey number
  box_raw_2 <- gsub(".*</span> ([A-Za-zÀ-ÖØ-öø-ÿ' \\.\\(\\),-]{1,})</th>", "B;\\1", box_raw_1) # player name
  box_raw_3 <- gsub('.*<td class="text-center hide-on-medium-down">(\\*)</td>.*', "O;\\1", box_raw_2) # Starter flag
  box_raw_3 <- gsub('.*<td class="text-center hide-on-medium-down"></td>.*', "O;0", box_raw_3) # Non-Starter flag
  box_raw_4 <- gsub('.* data-label="MIN">([0-9]{1,2})</td>.*', "C;\\1", box_raw_3) # Player minutes played
  box_raw_5 <- gsub('.* data-label="FG">([0-9]{1,2}[-][0-9]{1,2})</td>.*', "D;\\1", box_raw_4) # shots attempted and made
  box_raw_6 <- gsub('.* data-label="3PT">([0-9]{1,2}[-][0-9]{1,2})</td>.*', "E;\\1", box_raw_5) # 3pt attempted and made
  box_raw_7 <- gsub('.* data-label="FT">([0-9]{1,2}[-][0-9]{1,2})</td>.*', "F;\\1", box_raw_6) # FT attempted and made
  box_raw_8 <- gsub('.* data-label="ORB-DRB">([0-9]{1,2}[-][0-9]{1,2})</td>.*', "G;\\1", box_raw_7) # Rebound splits
  box_raw_9 <- gsub('.* data-label="REB">([0-9]{1,2})</td>.*', "H;\\1", box_raw_8) # Total rebounds
  box_raw_10 <- gsub('.* data-label="PF">([0-9]{1,2})</td>.*', "I;\\1", box_raw_9) # Fouls
  box_raw_11 <- gsub('.* data-label="A">([0-9]{1,2})</td>.*', "J;\\1", box_raw_10) # Assists
  box_raw_12 <- gsub('.* data-label="TO">([0-9]{1,2})</td>.*', "K;\\1", box_raw_11) # Turnovers
  box_raw_13 <- gsub('.* data-label="BLK">([0-9]{1,2})</td>.*', "L;\\1", box_raw_12) # Blocks
  box_raw_14 <- gsub('.* data-label="STL">([0-9]{1,2})</td>.*', "M;\\1", box_raw_13) # Steals
  box_raw_15 <- gsub('.* data-label="PTS">([0-9]{1,2})</td>.*', "N;\\1", box_raw_14) # Points
  
  # Subset box_raw_15 to remove any lines that start with ' ' and blanks
  box_working <- box_raw_15[substr(box_raw_15, 1, 1) != " "]
  box_working <- box_working[substr(box_working, 1, 1) != ""]
  
  # Build what will be the df
  metric_id <- substr(box_working, 1, 1) #Grabs the letter ID from above
  metric <- substr(box_working,3,nchar(box_working)) #Grabs the metric from the letter
  lm <- length(metric_id) 
  metric_id_1 <- rbind(metric_id[1:(lm-14)], 
                       metric_id[2:(lm-13)], 
                       metric_id[3:(lm-12)],
                       metric_id[4:(lm-11)],
                       metric_id[5:(lm-10)],
                       metric_id[6:(lm-9)],
                       metric_id[7:(lm-8)],
                       metric_id[8:(lm-7)],
                       metric_id[9:(lm-6)],
                       metric_id[10:(lm-5)],
                       metric_id[11:(lm-4)],
                       metric_id[12:(lm-3)],
                       metric_id[13:(lm-2)],
                       metric_id[14:(lm-1)],
                       metric_id[15:lm]
  )
  sts <- which(apply(metric_id_1 == c("A","B", "O", "C", "D", "E",
                                      "F", "G", "H", "I", "J",
                                      "K", "L", "M", "N"), 2, prod)>0)
  
  # Grab team name from box_raw
  team_name1 <- box_raw_team2[1]
  team_name1 <- gsub('<h3 class="sub-heading">([A-Za-z \\.\\(\\),-]{1,})[ \t].*', "P;\\1", team_name1) # team name
  team_name1 <- trimws(team_name1, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") # trim leading space
  team_name1 <- substr(team_name1,3,nchar(team_name1)) #Grabs the team name
  
  # Build the data frame from the sts key above that counts every 14 digis
  box_score_team2 <- data.frame(number=metric[sts],
                                player = metric[sts+1],
                                starter = metric[sts+2],
                                minutes = metric[sts+3],
                                FGM_FGA = metric[sts+4],
                                threeFGM_threeFGA = metric[sts+5],
                                FTM_FTA = metric[sts+6],
                                OREB_DREB = metric[sts+7],
                                reb = metric[sts+8],
                                fouls = metric[sts+9],
                                assists = metric[sts+10],
                                turnovers = metric[sts+11],
                                blocks = metric[sts+12],
                                steals = metric[sts+13],
                                points = metric[sts+14],
                                team = team_name1
  )
  
  ## Final data frame cleansing -------------------------------------------
  
  # Bind the box scores together
  box_score <- rbind(box_score_team1, box_score_team2)
  
  # Grab the date of the game from the source
  # Index the rows in the html to ID where the box score begins and ends
  date_row <- grep('<dt>Date</dt>', full_raw)+1 # + 1 to get following line
  date_data <- full_raw[date_row]
  date <- gsub(".*<dd>(\\d+/\\d+/\\d+)</dd>", 'Q;\\1', date_data)
  
  # Extract
  date <- substr(date,3,nchar(date))
  
  # Apply to df
  box_score$game_date <- date
  
  # Grab the venue from the source to help ID home/away
  venue_row <- grep("<dt>Site</dt>", full_raw) +1 #+1 to get following line
  venue <- full_raw[venue_row]
  site <- gsub(".*<dd>([A-Za-z \\.\\(\\),/&]{1,})</dd>", 'R;\\1', venue)
  
  # Extract
  site <- substr(site,3,nchar(site))
  
  # Apply to df
  box_score$venue <- site
  
  # Replace starter * with 1 so column is binary
  box_score$starter <- gsub("\\*", "1", box_score$starter)
  
  # Create game_id and input into box
  game_id <- paste0(date,"_",team_name,"_",team_name1)
  box_score$game_id <- game_id
 
  return(box_score)
  
}






# Play by Play section ----------------------------------------------------

process_pbp <- function(game_link){
  
  # Game link for testing (comment out when done)
  #game_link <- "https://getsomemaction.com/boxscore.aspx?id=7Gx48kkB20iPlOppZhxXHy%2f5Zy4VtNXkGzuHdKHXRgkOw0pduZ0zZ9dLA8PyalFluCBCpHtNipVxMoZWikX6fwnaZY5kqBPIQzjVDjspFTWNw5hYJlizlf%2bZOhcyXuW%2b8hiTQFYHq8B75QRYLyUqvA%3d%3d&path=mbball"
  
  # Reads it in as a long html source rather than large list
  full_raw <- try(unlist(strsplit(gsub("\\t", "", gsub("\\r", "", httr::GET(game_link))), "\n")), TRUE)
  
  # Index the rows in the html to ID where the box score begins and ends
  pbp_row_start <- grep('<section id="play-by-play"', full_raw) + 0 # +0 to make numeric
  pbp_row_end <- grep('<section id="play-analysis"', full_raw) - 1 # -1 to get row before play play analysis
  
  # Filter full_raw for just these rows
  pbp_raw <- full_raw[pbp_row_start:pbp_row_end]
  
  # How can we figure out how many periods there were in the game when OTs
  # can be endless?
  
  # Maybe we don't have to?
  
  # Identify the rows where the periods begin
  period_start <- grep("<div id=\"period-", pbp_raw)
  periods <- length(period_start)
  
  
  # Build the pieces
  # pbp_raw_1 <- gsub('.*<div id="([a-z]{1,}[-][0-9])">', "A;\\1", pbp_raw) # Period
  pbp_raw_1 <- gsub('.*<th scope="row" class="text-center">([0-9]{1,2}[\\:][0-9]{1,2})</th>', "A;\\1", pbp_raw) # Time remaining
  pbp_raw_2 <- gsub('.*<th scope="row" class="text-center">([-]{1,2})</th>', "A;\\1", pbp_raw_1) # Time remaining --
  # pbp_raw_3 <- gsub('.*<td class="hide-on-large">([0-9A-Za-zÀ-ÖØ-öø-ÿ \\.\\(\\),-]{1,})</td>.*', "B;\\1", pbp_raw_2) # Play detail
  
  pbp_raw_3 <- gsub('.*<td class="hide-on-large">([A-Za-z0-9À-ÖØ-öø-ÿ\' \\.\\(\\),-]{1,})</td>.*', "B;\\1", pbp_raw_2) # Play detail
  
  
  # Remove everything that isn't a piece above
  pbp_working <- pbp_raw_3[substr(pbp_raw_3, 1, 1) != " "]
  pbp_working <- pbp_working[substr(pbp_working, 1, 1) != ""]
  
  # Build what will be the df
  metric_id <- substr(pbp_working, 1, 1) #Grabs the letter ID from above
  metric <- substr(pbp_working,3,nchar(pbp_working)) #Grabs the metric from the letter
  lm <- length(metric_id) 
  metric_id_1 <- rbind(metric_id[1:(lm-1)], 
                       metric_id[2:lm]
  )
  
  sts <- which(apply(metric_id_1 == c("A","B"), 2, prod)>0)
  
  # Build the data frame from the sts key above that counts every 2 digis
  pbp <- data.frame(time_remaining=metric[sts],
                                description = metric[sts+1]
  )
  
  # Replace -- with blanks in time remaining column
  pbp$time_remaining <- gsub("--", "", pbp$time_remaining)
  
  # Grab minutes & seconds remaining into its own column
  pbp$min_remaining <- as.numeric(as.character(gsub("([0-9]{1,2}):.*", "\\1", pbp$time_remaining)))
  pbp$sec_remaining <- as.numeric(as.character(gsub(".*:([0-9]{1,2})", "\\1", pbp$time_remaining)))
  
  # Calculate numeric remaining
  pbp$time_remaining <- as.numeric(pbp$min_remaining * 60 + pbp$sec_remaining)
  pbp <- pbp[, 1:2]
  
  # Fill the time remaining down for events that happen at the same time
  # (subs, rebounds, etc...)
  pbp <- tidyr::fill(pbp, time_remaining, .direction = "down")
  
  
  # Grab the time remaining from the previous row to ID when halves change
  pbp$prev_time <- dplyr::lag(pbp$time_remaining, n = 1L)
  pbp$half_change <- ifelse(pbp$time_remaining > pbp$prev_time, 1, 0)
  pbp$half_change <- ifelse(is.na(pbp$half_change), 1, pbp$half_change)
  
  # Counts the occurrences of values in the half_change column
  # So every time there's a 1, we get what period that is
  pbp <- data.table::as.data.table(pbp)[, count := seq(.N), by = "half_change"][]
  
  pbp$period <- ifelse(pbp$half_change == 1, pbp$count, "")
  pbp$period <- as.numeric(pbp$period)
  pbp <- tidyr::fill(pbp, period, .direction = "down")
  
  # Remove helper columns
  pbp <- pbp[,c(1:2, 6)]
  
  # Process box to apply metadata to game
  box <- process_box(game_link)
  
  # Steal data from the box score rip in this function
  pbp$game_date <- unique(box$game_date)
  pbp$venue <- unique(box$venue)
  pbp$game_id <- unique(box$game_id)
  
  return(pbp)
  
}
  
  
  
  
  
  
  
  