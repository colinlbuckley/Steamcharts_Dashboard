library(tidyverse)
library(rvest)
library(polite)

# Base session to make functional/iterative polite scraping easier.
bow_steamcharts <- bow("https://steamcharts.com/", force = TRUE)

# Function to scrape one full page (25 games) of "Top Games by Current Players"
# including URL for each game's standalone page containing more data.
scrape_top_games <- function(link, starting_bow = bow_steamcharts) {
    
    # Modify session with Top Games URL
    session <- starting_bow %>% nod(path = link)
    
    # Locate and extract URL extensions
    link_extensions <- scrape(session) %>% 
        html_node("tbody") %>%
        html_nodes("tr") %>%
        html_nodes("td.game-name.left") %>%
        html_nodes("a") %>%
        html_attr("href")
    
    # Extract full table
    table <- scrape(session) %>% 
        html_node("table") %>%
        html_table() %>% 
        select(-c(1, 4)) %>% # Delete unnecessary columns
        as_tibble() %>%
        mutate(Link = paste("https://steamcharts.com",
                            link_extensions, 
                            sep = "")) # Attach URLs for each game
    
    table
    
}

# Function to scrape time series (monthly) player count data for a specific game
scrape_game_table <- function(game_link, n = 1, starting_bow = bow_steamcharts) {
    
    # Modify starting session with game-specific URL
    session <- starting_bow %>% nod(path = game_link)
    
    # Extract game name
    game_name <- scrape(session) %>%
        html_node("h1#app-title") %>%
        html_text()
    
    # Extract monthly player counts and attach game name
    game_table <- scrape(session) %>%
        html_node("table.common-table") %>%
        html_table() %>%
        mutate(Name = game_name, .before = Month) %>%
        mutate(Rank = n) %>% # Keep track of overall rank
        rename(Avg_Players = 'Avg. Players',
               Pct_Gain = '% Gain',
               Peak_Players = 'Peak Players') # Rename columns for ease of use
    
    # Progress ticker for completion of each game
    cat(n, ". ", "\"", as.character(game_name), "\"", " done!", "\n", sep = "")
    
    return(game_table)
    
}

# Following code commented to allow for easy sourcing from other files.

# # Iterate to scrape top 100 games by current player count (4 pages @ 25 games per page)
# top_100 <- map_df(.x = paste("https://steamcharts.com/top/p.", 1:4, sep = ""),
#                   ~ scrape_top_games(link = .x))
# 
# # Extract monthly time series player counts for each game
# top_games_ts <- map2_dfr(.x = top_100$Link, .y = 1:100,
#              ~ scrape_game_table(game_link = .x, n = .y))
# 
# # Write both to .csv file with current date
# write_csv(top_100, file = paste("Data/top_100_games_", Sys.Date(), ".csv", sep = ""))
# write_csv(top_games_ts, file = paste("Data/top_100_timeseries_monthly_", Sys.Date(), ".csv", sep = ""))
