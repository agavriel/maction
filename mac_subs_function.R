require(tidyverse)

# Substitutions -----------------------------------------------------------

# we don't want this to use the process_box or process_pbp functions
# since those require a web hit, so can we just pick up after those
# functions run?

process_subs <- function(box_df, pbp_df){
  
  box_df <- mac_box
  pbp_df <- mac_pbp
  
  # Store the input dfs as their own within the function
  box <- box_df
  pbp <- pbp_df
  
  m_players <- box %>% 
    select(player, team, game_id) %>% 
    mutate(player = toupper(player))
  
  # Filter the box df to just the starters
  starters <- box %>% 
    filter(starter == "1") %>% 
    select(player, team, game_id) %>% 
    mutate(player = toupper(player),
           event = "player_in",
           time_remaining = 1200,
           period = 1)
  
  # Filter the pbp for substitution events only
  subs <- pbp %>%
    filter(grepl("SUB ", description) == TRUE) %>% 
    
    # Id event type
    mutate(event = 
             case_when(grepl("SUB IN", description) == TRUE ~ "player_in",
                       grepl("SUB OUT", description) == TRUE ~ "player_out",
                       TRUE ~ "remove")
           
           ) %>% 
    
    # Remove the player from the event description
    mutate(player = 
             case_when(
               event == "player_in" ~ gsub(".*SUB IN by ([A-Za-zÀ-ÖØ-öø-ÿ' \\.\\(\\),-]{1,})", "\\1", description),
               TRUE ~ gsub(".*SUB OUT by ([A-Za-zÀ-ÖØ-öø-ÿ' \\.\\(\\),-]{1,})", "\\1", description)
             )
           ) %>% 
    
    # Merge in the player team
    left_join(m_players,
              by = c("player" = "player",
                     "game_id" = "game_id"))
  
  ## All the sub fixes broken in the source data we've investigated -------
  
  # List of MAC teams
  d_mac <- c(
    "Akron",
    "Ball St.",
    "Bowling Green",
    "Buffalo",
    "Central Mich.",
    "Eastern Mich.",
    "Kent St.",
    "Miami (OH)",
    "NIU",
    "Ohio",
    "Toledo",
    "Western Mich."
  )
  
  
  # Start with the egregious killing off of all non-conf data
  w_subs <- subs %>% 
    filter(team  %in% d_mac)
  
  # Now we fix everything we've id'd
  w_subs <- w_subs %>% 
    
    # 11/19 South Bend v Ball St.
    mutate(check =
             case_when(
               game_id == "11/19/2022_Ind.-South Bend_Ball St." &
                 period == 1 &
                 time_remaining == 386 &
                 grepl("CLEARY", description) == TRUE ~ 1,
               TRUE ~ 0)
           ) %>%
    filter(check != 1) %>%
    
    #11/23/22 CSUN CMU 1
    mutate(check = 
             case_when(
               game_id == "11/23/2022_CSUN_Central Mich." &
                 period == 2 &
                 time_remaining == 364 ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    #11/23/22 CSUN CMU 2
    mutate(check = 
             case_when(
               game_id == "11/23/2022_CSUN_Central Mich." &
                 period == 2 &
                 time_remaining == 833 &
                 grepl("MILLER,KEVIN", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 11/26/22 Ball st. vs Missouri St
    mutate(check =
             case_when(
               game_id == "11/26/2022_Missouri St._Ball St." &
                 period == 2 &
                 time_remaining == 116 &
                 grepl("JACOBS,DEMARIUS", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 12/21/22 Ball St vs GA southern
    mutate(check =
            case_when(
              game_id == "12/21/2022_Ga. Southern_Ball St." &
                period == 2 &
                time_remaining == 19 ~ 1,
              TRUE ~ 0
            )
           ) %>% 
    filter(check != 1) %>% 
    
    # 1/3/23 NIU v Akron 1
    mutate(check =
             case_when(
               game_id == "1/3/2023_NIU_Akron" &
                 period == 1 &
                 time_remaining == 946 &
                 grepl("MITCHELL,KOBE", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 1/3/23 NIU v Akron 2
    mutate(check = 
             case_when(
               game_id == "1/3/2023_NIU_Akron" &
                 period == 2 &
                 time_remaining == 556 ~ 1,
               TRUE ~ 0
             ),
           check2 =
             case_when(
               check == 1 ~ check + row_number(),
               TRUE ~ 0
             )
           ) %>% 
    slice_min(., order_by = check2, n=nrow(.)-2) %>% 
    select(-check, -check2) %>% 
    
    # Rearrange after this fix because the slice moves items
    arrange(game_id, period, desc(time_remaining)) %>% 
    
    # 1/3/23 NIU v Akron 3
    mutate(check = 
             case_when(
               game_id == "1/3/2023_NIU_Akron" &
                 period == 2 &
                 time_remaining == 313 &
                 grepl("SUB OUT by NUTTER,ZARIQUE", description) == TRUE ~ 1,
               TRUE ~ 0),
           check2 =
             case_when(
               game_id == "1/3/2023_NIU_Akron" &
                 period == 2 &
                 time_remaining == 313 &
                 grepl("SUB IN by THORNTON,KALEB", description) == TRUE ~ 1,
               TRUE ~ 0
             )
    ) %>% 
    filter(check != 1) %>% 
    filter(check2 != 1) %>% 
    select(-check2) %>% 
    
    # 1/3/23 Miami OH v CMU
    mutate(check =
             case_when(
               game_id == "1/3/2023_Miami (OH)_Central Mich." &
                 team == "Miami (OH)" &
                 period == 1 &
                 time_remaining == 847 ~ 1,
               TRUE ~ 0
             ),
           check2 =
             case_when(
               check == 1 ~ check + row_number(),
               TRUE ~ 0
             )
           ) %>% 
    slice_min(., order_by = check2, n=nrow(.)-4) %>% 
    select(-check, -check2) %>% 
    
    # Rearrange after this fix because the slice moves items
    arrange(game_id, period, desc(time_remaining)) %>% 
    
    # 1/6/23 Akron Ball St.
    mutate(check = 
             case_when(
               game_id == "1/6/2023_Akron_Ball St." &
                 team == "Ball St." &
                 period == 2 &
                 time_remaining == 278 ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 11/15/22 EMU v Bradly
    mutate(check = 
             case_when(
               game_id == "11/15/2022_Eastern Mich._Bradley" &
                 period == 2 &
                 time_remaining == 630 &
                 grepl("FARRAKHAN", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 11/20/22 WMU Georgai Southern 1
    mutate(check = 
             case_when(
               game_id == "11/20/2022_Western Mich._Ga. Southern" &
                 period == 1 &
                 time_remaining == 765 &
                 grepl("HUBBARD", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 11/20/22 WMU Georgia Southern 2
    mutate(check =
             case_when(
               game_id == "11/20/2022_Western Mich._Ga. Southern" &
                 period == 2 &
                 time_remaining == 693 &
                 grepl("HUBBARD", description) == TRUE ~ 1,
               TRUE ~ 0
             )
    ) %>% 
    filter(check != 1) %>% 
    
    # 11/30/22 EMU v FIU
    mutate(check = 
             case_when(
               game_id == "11/30/2022_Eastern Mich._FIU" &
                 period == 2 &
                 time_remaining == 690 ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 11/7/22 WMU v Minnesota
    mutate(check = 
             case_when(
               game_id == "11/7/2022_Western Mich._Minnesota" &
                 period == 2 &
                 time_remaining == 287 &
                 grepl("MONEGRO,JEFFERSON", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 12/10 CMU Tulsa
    mutate(check = 
             case_when(
               game_id == "12/10/2022_Central Mich._Tulsa" &
                 period == 2 &
                 time_remaining == 36 ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 12/30 Ohio v Chicago St.
    mutate(check = 
             case_when(
               game_id == "12/30/2022_Chicago St._Ohio" &
                 period == 1 &
                 time_remaining == 310 &
                 grepl("BAKER,DEVON", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 11/13/22 CMU v Eastern Ill
    mutate(check =
             case_when(
               game_id == "11/13/2022_Eastern Ill._Central Mich." &
                 period == 1 &
                 time_remaining == 590 &
                 grepl("MILLER,KEVIN", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 1/10/2023 EMU v WMU
    mutate(check = 
             case_when(
               game_id == "1/10/2023_Eastern Mich._Western Mich." &
                 period == 2 &
                 time_remaining == 39 &
                 grepl("FARRAKHAN,NOAH", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 1/13/23 Kent St v Ohio
    mutate(check = 
             case_when(
               game_id == "1/13/2023_Kent St._Ohio" &
                 period == 2 &
                 time_remaining == 551 &
                 team == "Kent St." &
                 grepl("SUB OUT by SULLINGER,JALEN", description) == FALSE ~ 1,
               TRUE ~ 0
             ),
           check2 = 
             case_when(
               game_id == "1/13/2023_Kent St._Ohio" &
                 period == 2 &
                 time_remaining == 551 &
                 team == "Kent St." &
                 grepl("SUB IN by SANTIAGO,GIOVANNI", description) == FALSE ~ 1,
               TRUE ~ 0
             ),
           check3 = 
             case_when(
               check == 1 & check2 == 1 ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check3 != 1) %>% 
    select(-check2, -check3) %>% 
    
    # 1/13/23 EMU v Akron 1
    mutate(check = 
             case_when(
               game_id == "1/13/2023_Eastern Mich._Akron" &
                 period == 1 &
                 time_remaining == 486 &
                 team == "Akron" &
                 grepl("CASTANEDA|JOHNSON", description) == FALSE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>%  
    
    # 1/13/23 EMU v Akron 2
    mutate(check = 
             case_when(
               game_id == "1/13/2023_Eastern Mich._Akron" &
                 period == 1 &
                 time_remaining == 410 &
                 team == "Akron" &
                 grepl("CASTANEDA", description) == TRUE ~ 1,
               TRUE ~ 0
             )
    ) %>% 
    filter(check != 1) %>%  
    
    # 1/14/23 UB v CMU
    mutate(check =
             case_when(
               game_id == "1/14/2023_Buffalo_Central Mich." &
                 period == 3 &
                 time_remaining == 75 ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 1/14/23 Toledo v NIU
    mutate(check = 
             case_when(
               game_id == "1/14/2023_Toledo_NIU" &
                 period == 1 &
                 time_remaining == 346 &
                 grepl("IBARGUEN,HARVIN", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    #1/3/23 Kent St v WMU
    mutate(check = 
             case_when(
               game_id == "1/3/2023_Western Mich._Kent St." &
                 period == 2 &
                 time_remaining == 1200 &
                 grepl("SUB IN by PAYTON,CHRIS|SUB IN by THOMAS,MIRYNE|SUB OUT by SANTIAGO,GIOVANNI|SUB OUT by HORNBEAK,CLI'RON",
                       description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    ungroup() %>% 
    group_by(game_id, team, time_remaining, period) %>% 
    unique() %>% 
    ungroup() %>% 
    select(-check) %>% 
    
    # 1/17/23 CMU v Akron (1/4)
    mutate(check = 
             case_when(
               game_id == "1/17/2023_Akron_Central Mich." &
                 period == 2 &
                 time_remaining == 704 &
                 grepl("SUB OUT by HUNTER,|SUB IN by FREEMAN", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 1/17/23 CMU v Akron (2/4)
    mutate(check = 
             case_when(
               game_id == "1/17/2023_Akron_Central Mich." &
                 period == 2 &
                 time_remaining == 85 &
                 grepl("CLARKE,GARVIN", description) == TRUE ~ 1,
               TRUE ~ 0
             )
    ) %>% 
    filter(check != 1) %>% 
    
    # 1/17/23 CMU v Akron (3/4)
    mutate(check = 
             case_when(
               game_id == "1/17/2023_Akron_Central Mich." &
                 period == 1 &
                 time_remaining == 424 &
                 grepl("PAVRETTE,NICOLAS", description) == TRUE ~ 1,
               TRUE ~ 0
             )
    ) %>% 
    filter(check != 1) %>% 
    
    # 1/17/23 CMU v Akron (4/4)
    mutate(check = 
             case_when(
               game_id == "1/17/2023_Akron_Central Mich." &
                 period == 1 &
                 time_remaining == 251 &
                 grepl("MAJERLE,MAX", description) == TRUE ~ 1,
               TRUE ~ 0
             )
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    # 1/21/23 Ohio vs CMU
    mutate(check = 
             case_when(
               game_id == "1/21/2023_Central Mich._Ohio" &
                 period == 1 &
                 time_remaining == 472 &
                 grepl("HARDING,MARKUS", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
  
    # 1/24/23 Ball St vs Buffalo
    mutate(check =
            case_when(
              game_id == "1/24/2023_Buffalo_Ball St." &
                period == 2 &
                time_remaining == 631 &
                grepl("BLOCKER,KIDTRELL", description) == TRUE ~ 1,
              TRUE ~ 0
            )
          ) %>% 
    filter(check != 1) %>% 
    
    # 1/24/23 NIU vs Kent
    mutate(check =
             case_when(
               game_id == "1/24/2023_Kent St._NIU" &
                 period == 2 &
                 time_remaining == 20 &
                 team == "NIU" &
                 grepl("SUB ", description) == TRUE ~ 1,
               TRUE ~ 0
             )
           ) %>% 
    filter(check != 1) %>% 
    
    # 1/24/23 Akron vs Miami (OH)
    mutate(check =
             case_when(
               game_id == "1/24/2023_Miami (OH)_Akron" &
                 period == 2 &
                 time_remaining == 26 &
                 team == "Akron" &
                 grepl("SUB ", description) == TRUE ~ 1,
               TRUE ~ 0
             )
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    # 1/28/23 WMU @ Central Mich
    mutate(check =
             case_when(
               game_id == "1/28/2023_Western Mich._Central Mich." &
                 period == 1 &
                 time_remaining == 160 &
                 grepl("MONEGRO,JEFFERSON|HUBBARD,SETH", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    # 1/31/23 Akron at UB
    mutate(check = 
             case_when(
               game_id == "1/31/2023_Akron_Buffalo" &
                 period == 2 &
                 time_remaining == 623 &
                 grepl("SUB IN by ADAMS,ISAIAH|SUB OUT by JONES,KANYE", description) == TRUE ~ 1,
               TRUE ~ 0)
             ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    # 2/5/23 NIU @ BGSU
    mutate(check = 
             case_when(
               game_id == "2/4/2023_NIU_Bowling Green" &
                 period == 1 &
                 time_remaining == 146 &
                 grepl("HUNTER,DARWESHI", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    # 2/7/23 Ball @ CMU
    mutate(check = 
             case_when(
               game_id == "2/7/2023_Ball St._Central Mich." &
                 period == 2 &
                 time_remaining == 145 &
                 grepl("JIHAD,BASHEER", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    
    # 2/7/23 EMU @ UB
    mutate(check = 
             case_when(
               game_id == "2/7/2023_Eastern Mich._Buffalo" &
                 period == 2 &
                 time_remaining == 94 &
                 team == "Eastern Mich." ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    # 2/7/23 Ohio @ NIU
    mutate(check = 
             case_when(
               game_id == "2/7/2023_Ohio_NIU" &
                 period == 2 &
                 time_remaining == 420 &
                 grepl("THORNTON,KALEB", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    # 2/10/23 Kent @ UB
    mutate(check = 
             case_when(
               game_id == "2/10/2023_Kent St._Buffalo" &
                 period == 2 &
                 time_remaining == 839 &
                 grepl("POWELL,YAZID", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    #2/10/23 Akron @ Ohio
    mutate(check = 
             case_when(
               game_id == "2/10/2023_Akron_Ohio" &
                 period == 1 &
                 time_remaining == 380 &
                 grepl("BROWN,MILES|SUB IN by RODERICK,BEN", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
  
    #2/11/23 CMU @ Miami OH
    mutate(check = 
            case_when(
              game_id == "2/11/2023_Central Mich._Miami (OH)" &
                period == 2 &
                time_remaining == 7 ~ 1,
              TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    #2/14/23 CMU @ BGSU
    mutate(check = 
             case_when(
               game_id == "2/14/2023_Central Mich._Bowling Green" &
                 team == "Central Mich." &
                 period == 2 &
                 time_remaining == 1200 ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    #2/18/23 Ohio @ CMU
    mutate(check = 
             case_when(
               game_id == "2/18/2023_Ohio_Central Mich." &
                 period == 1 &
                 time_remaining == 833 &
                 grepl("HARDING,MARKUS", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    #2/18/23 WMU @ EMU
    mutate(check = 
             case_when(
               game_id == "2/21/2023_Western Mich._Eastern Mich." &
                 period == 2 &
                 time_remaining == 744 &
                 grepl("BILLINGLSEY,JALIN", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    
    # Part 2 same game
    mutate(check = 
             case_when(
               game_id == "2/21/2023_Western Mich._Eastern Mich." &
                 period == 2 &
                 time_remaining == 34 &
                 grepl("GOLSON,COLIN", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    # 2/25/23 NIU @ Central Mich
    mutate(check = 
             case_when(
               game_id == "2/25/2023_NIU_Central Mich." &
                 period == 2 &
                 time_remaining == 174 &
                 grepl("DRUMMOND,AMANI", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    
    # 2/25/23 Buffalo @ Toledo
    mutate(check = 
             case_when(
               game_id == "2/25/2023_Buffalo_Toledo" &
                 period == 2 &
                 time_remaining == 195 &
                 grepl("WALLACE,J", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    
    # 2/25/23 Ball @ EMU
    mutate(check = 
             case_when(
               game_id == "2/25/2023_Ball St._Eastern Mich." &
                 period == 2 &
                 time_remaining == 451 &
                 grepl("SELLERS,JAYLIN", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    
    # WMU @ Akron
    mutate(check = 
             case_when(
               game_id == "2/25/2023_Western Mich._Akron" &
                 period == 2 &
                 time_remaining == 514 &
                 grepl("HUNTER,SAMMY", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>% 
    select(-check) %>% 
    
    # 2/28 EMU @ BGSU
    mutate(check = 
             case_when(
               game_id == "2/28/2023_Eastern Mich._Bowling Green" &
                 period == 2 &
                 time_remaining == 1200 &
                 grepl("JIHAD,YUSUF|BATES,EMONI", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>%
    
    # 2/28 Ball St @ Akron
    mutate(check = 
             case_when(
               game_id == "2/28/2023_Ball St._Akron" &
                 period == 2 &
                 time_remaining == 20 &
                 grepl("JOHNSON,NATE", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>%
    
    # 2/28 Ball St @ Akron 2
    mutate(check = 
             case_when(
               game_id == "2/28/2023_Ball St._Akron" &
                 period == 2 &
                 time_remaining == 31 &
                 grepl("HENDRIKS,BEN", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>%
    
    # 2/28 Toledo @ CMU
    mutate(check = 
             case_when(
               game_id == "2/28/2023_Toledo_Central Mich." &
                 period == 1 &
                 time_remaining == 567 &
                 team == "Central Mich." ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>%
    
    # 3/3 BGSU @ Ohio
    mutate(check = 
             case_when(
               game_id == "3/3/2023_Bowling Green_Ohio" &
                 period == 2 &
                 time_remaining == 228 &
                 grepl("JAMES,ELMORE", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>%
    
    # 3/3 Eastern Mich @ NIU
    mutate(check = 
             case_when(
               game_id == "3/3/2023_Eastern Mich._NIU" &
                 period == 1 &
                 time_remaining == 377 &
                 grepl("BATES,EMONI", description) == TRUE ~ 1,
               TRUE ~ 0)
    ) %>% 
    filter(check != 1) %>%
    select(-check)
    
    
    
    
    
    
    
    
  
  
  ## Actually begin working with the data finally -------------------------
    
  w_subs2 <- w_subs %>% 
    
    # Make the date column dates
    mutate_at(vars(game_date), as.Date, format = "%m/%d/%Y") %>% 
    plyr::rename(c("venue" = "arena")) %>% 
    
    # Remove non-conference play because it helps the loop and I'm lazy
    filter(game_date > as.Date("2023-01-01")) %>% 
    
    # Now we can merge venue and use that to ID teams when we
    # merge to the PBP
    left_join(
      select(mac_schedule, Date, team, venue),
      by = c("game_date" = "Date",
             "team" = "team")
    )
    
    # I think we're okay on neutral tags since we've removed non-mac
    # Teams but this will come up again in MAC tourney, I guess...
    
    # I'm not sure if there's a better way to do this than a loop
    # in the loop for game_id by team
  
  pbp_list <- list()
    
    # One game test
    # game <- "1/10/2023_Akron_Bowling Green"
    
    for(game in sort(unique(w_subs2$game_id))) {
      
      on_court <- w_subs2 %>% 
        filter(game == game_id)
      
      # Split by team
      oc_split <- split(on_court, on_court$team)
      
      team1 <- oc_split[[1]]
      team2 <- oc_split[[2]]
      
      ### First team ------------------------------------------------------
      
      # Build the starters
      starters1 <- starters %>% 
        filter(game_id == game) %>% 
        filter(team == unique(team1$team)) %>% 
        
        # Fill in missing columns
        mutate(description = "game start",
               game_date = unique(team1$game_date),
               arena = unique(team1$arena),
               venue = unique(team1$venue)
        )
      
      oc_team1 <- starters1 %>% 
        bind_rows(team1)
        
      
      # Arrange by player so the five man concat is always the same order
      oc_team1 <- oc_team1 %>% 
        arrange(player)
      
      oc_team1 <- oc_team1 %>% 
        pivot_wider(
          names_from = "player",
          values_from = "event"
        ) %>% 
        
        # Re-arrange by period and time and fill down
        arrange(period, desc(time_remaining)) %>% 
        fill(names(.))
      
      # More loops!
      #Loop to replace any instance of "Player_In" with the player col header
      for (i in 1:length(oc_team1)) {
        oc_team1[[i]] <- stringr::str_replace(oc_team1[[i]], "player_in", colnames(oc_team1)[i])
      }
      
      # Replace "player_out" with NA so we can easily concat the five oc
      oc_team1 <- oc_team1 %>% 
        na_if("player_out") %>% 
        
        # Add a row index number and keep the highest row number so 
        # we get the last event for each sub combination so five ballers on
        rowid_to_column("index") %>% 
        ungroup() %>% 
        group_by(period, time_remaining) %>% 
        filter(index == max(index)) %>% 
        ungroup() %>% 
        
        # On court concatenation
        unite(.,
              "ballers", 10:ncol(.),
              na.rm = TRUE, remove = FALSE) %>% 
        
        # Rename ballers to team and on court and then clean
        select(game_id, game_date, arena, period, time_remaining, description, ballers) %>% 
        plyr::rename(c("ballers" = "team1_on_court")) %>% 
        
        # Subtract a decimal second from subs so if there is bucket then 
        # sub in the PBP, the sub always happens after the bucket
        
        mutate(time_remaining = as.numeric(time_remaining),
               time_remaining =
                 case_when(time_remaining > 0 ~ time_remaining - 0.01,
                           TRUE ~ time_remaining),
               period = as.numeric(period)
        )
      
      ## Second team ------------------------------------------------------
      
      # Build the starters
      starters2 <- starters %>% 
        filter(game_id == game) %>% 
        filter(team == unique(team2$team)) %>% 
        
        # Fill in missing columns
        mutate(description = "game start",
               game_date = unique(team2$game_date),
               arena = unique(team2$arena),
               venue = unique(team2$venue)
        )
      
      oc_team2 <- starters2 %>% 
        bind_rows(team2)
      
      
      # Arrange by player so the five man concat is always the same order
      oc_team2 <- oc_team2 %>% 
        arrange(player)
      
      oc_team2 <- oc_team2 %>% 
        pivot_wider(
          names_from = "player",
          values_from = "event"
        ) %>% 
        
        # Re-arrange by period and time and fill down
        arrange(period, desc(time_remaining)) %>% 
        fill(names(.))
      
      # More loops!
      #Loop to replace any instance of "Player_In" with the player col header
      for (i in 1:length(oc_team2)) {
        oc_team2[[i]] <- stringr::str_replace(oc_team2[[i]], "player_in", colnames(oc_team2)[i])
      }
      
      # Replace "player_out" with NA so we can easily concat the five oc
      oc_team2 <- oc_team2 %>% 
        na_if("player_out") %>% 
        
        # Add a row index number and keep the highest row number so 
        # we get the last event for each sub combination so five ballers on
        rowid_to_column("index") %>% 
        ungroup() %>% 
        group_by(period, time_remaining) %>% 
        filter(index == max(index)) %>% 
        ungroup() %>% 
        
        # On court concatenation
        unite(.,
              "ballers", 10:ncol(.),
              na.rm = TRUE, remove = FALSE) %>% 
        
        # Rename ballers to team and on court and then clean
        select(game_id, game_date, arena, period, time_remaining, description, ballers) %>% 
        plyr::rename(c("ballers" = "team2_on_court")) %>% 
        
        # Subtract a decimal second from subs so if there is bucket then 
        # sub in the PBP, the sub always happens after the bucket
        
        mutate(time_remaining = as.numeric(time_remaining),
               time_remaining =
          case_when(time_remaining > 0 ~ time_remaining - 0.01,
                    TRUE ~ time_remaining),
          period = as.numeric(period)
        )
      
      ## PBP Cleaning and merges ------------------------------------------
      
      
      # Get the pbp involved for merges, this will also be what
      # we return from the function
      
      # Create the game start skeleton and build into the pbp
      
      game_pbp <- pbp %>% 
        filter(game_id == game) %>% 
        mutate_at(vars(game_date), as.Date, format = "%m/%d/%Y")
      
      final_pbp <- tribble(
        ~time_remaining, ~description, ~period, ~game_date, ~venue, ~game_id,
        1199.99, "game start", 1, unique(game_pbp$game_date), 
        unique(oc_team1$arena), unique(oc_team1$game_id)
        ) %>% 
        
        bind_rows(game_pbp) %>%
         
        # ID Sub events and subtract a decimal
        mutate(time_remaining = 
                 case_when(
                   grepl("SUB ", description) == TRUE ~ time_remaining - 0.01,
                   TRUE ~ time_remaining
                 )) %>% 
        
        # Merge in team1
        left_join(
          select(oc_team1, -game_id, -game_date, -arena),
                  by = c("period" = "period",
                         "time_remaining" = "time_remaining",
                         "description" = "description"
                         )
          ) %>% 
        
        # Merge in team2
        left_join(
          select(oc_team2, -game_id, -game_date, -arena),
          by = c("period" = "period",
                 "time_remaining" = "time_remaining",
                 "description" = "description"
          )
        )
      
      # Add this game to the list
      pbp_list[[game]] <- final_pbp
      
      
      print(game)
        
        
        
      
    }
    
    # Bind everything that came out of that loop into a dataframe
    f_pbp_oc <- do.call(rbind, pbp_list)
  
    
    return(f_pbp_oc)
    
    print(game)
    
    
    
}

