#devtools::install_github("schochastics/Rokemon")
library(httr)
library(png)
library(rtweet)

## Choose 2 pokemon at random ====

# get data from the bulbapedia API
api_responses <- lapply(
  1:2,
  function(gen, link) httr::GET(paste0(link, gen)),
  link = "http://pokeapi.co/api/v2/generation/"
)

poke_content <- lapply(
  api_responses,
  function(resp) content(resp, as = "parsed")$pokemon_species
)

pokemon_info_dataframe <- unstack(stack(unlist(poke_content)))

## choose pokemon
pokemon_chosen <- sample(pokemon_info_dataframe$url, 2, replace = TRUE)
pokemon_names <- sapply(pokemon_chosen, function(url) pokemon_info_dataframe$name[pokemon_info_dataframe$url == url])
chosen_pokemon_links <- sprite_links  <- gsub("pokemon-species", "pokemon", pokemon_chosen)

## get pokemon stats ====
# small function to level up stats for pokemon
increases_stats <- function(base, name, lvl) {
  if(name == "hp") {
    addition <- lvl + 10
  } else {
    addition <- 5
  }
  
  # will assume pokemon are wild/ trainers for now so no addition to stat exp
  dv <- sample(5:9, 1)
  stat_val <- round(((base + dv)*2) * (lvl / 100)) + addition
}

# randomly select first pokemons level
first_level <- sample(10:99, 1)

first_pokemon_stats <- httr::GET(chosen_pokemon_links[1])
first_pokemon_content <- httr::content(first_pokemon_stats)
first_pokemon_stat_values <- sapply(first_pokemon_content$stats, function(stats) stats[["base_stat"]])
first_pokemon_stat_types <- sapply(first_pokemon_content$stats, function(stats) stats[["stat"]][["name"]])

first_pokemon_adjusted_stats <- mapply(
  increases_stats, 
  first_pokemon_stat_values, 
  first_pokemon_stat_types, 
  MoreArgs = list(lvl = first_level)
)
names(first_pokemon_adjusted_stats) <- first_pokemon_stat_types

# base the second pokemons level
second_pokemon_stats <- httr::GET(chosen_pokemon_links[2])
second_pokemon_content <- httr::content(second_pokemon_stats)
second_pokemon_stat_values <- sapply(second_pokemon_content$stats, function(stats) stats[["base_stat"]])
second_pokemon_stat_types <- sapply(second_pokemon_content$stats, function(stats) stats[["stat"]][["name"]])

second_pokemon_stat_total <- sapply(
  1:99,
  function(lvl, base, name) {
    levelled_stats <- mapply(
      increases_stats, 
      first_pokemon_stat_values, 
      first_pokemon_stat_types, 
      MoreArgs = list(lvl = lvl)
    )
    stats_total <- sum(levelled_stats)
  },
  base = second_pokemon_stat_values,
  name = second_pokemon_stat_names
)

equivalent_level <- which(second_pokemon_stat_total >= sum(first_pokemon_adjusted_stats))[1]
chosen_second_level <- equivalent_level + sample(-5:5, 1)
if(chosen_second_level > 99) chosen_second_level <- 99

second_pokemon_adjusted_stats <- mapply(
  increases_stats, 
  second_pokemon_stat_values, 
  second_pokemon_stat_types, 
  MoreArgs = list(lvl = chosen_second_level)
)
names(second_pokemon_adjusted_stats) <- second_pokemon_stat_types

## draw pokemon movesets ====
levels = c(first_level, chosen_second_level)

pokemon_moves <- lapply(chosen_pokemon_links, function(p) httr::content(httr::GET(p))$moves)

get_df <- function(moves, gen) {
  
  df <- do.call(
    rbind,
    lapply(
      moves,
      function(m) {
        gen = sapply(m$version_group_details, function(x) x$version_group$name)
        lvl = sapply(m$version_group_details, function(x) x$level_learned_at)
        
        move <- m$move$name
        url <- m$move$url
        df <- data.frame(gen, lvl, move, url)
      }
    )
  )
  
  gen_df <- df[df$gen == gen,]
}

gen_moves <- lapply(pokemon_moves, get_df, gen = "gold-silver")

pick_moves <- function(gen_moves, level, n_moves, machine_chance) {
  learned_moves <- data.frame(
    gen = NULL,
    lvl = NULL,
    move = NULL,
    url = NULL
  )
  
  for(m in seq(n_moves)) {
    
    # by levelling or by moveset
    if(machine_chance > runif(1)) {
      moveset <- gen_moves[gen_moves$lvl == 0,]
    } else {
      moveset <- gen_moves[gen_moves$lvl > 0 & gen_moves$lvl <= level,]
    }
    
    moveset <- moveset[!moveset$move %in% learned_moves$move,]
    
    if(nrow(moveset) > 0) {
      chosen_move <- moveset[sample(seq(nrow(moveset)), 1),]
      learned_moves <- rbind(learned_moves, chosen_move)
    }
  }
  
  return(learned_moves)
}

pokemon_movesets <- mapply(
  pick_moves,
  gen_moves,
  levels,
  MoreArgs = list(n_moves = 4, machine_chance = 0.1)
)
pokemon_movesets <- data.frame(t(pokemon_movesets))

## get pokemon sprites ====

temp_directory <- tempdir()

# download sprite pngs to temp folder
sprite_files <- mapply(
  function(position, link, dir) {
    pokemon_info <- httr::content(httr::GET(link))
    sprites <- pokemon_info$sprites$versions$`generation-iv`$`heartgold-soulsilver`
    selected_sprite <- sprites[[paste0(position, "_default")]]
    
    download.file(selected_sprite, file.path(temp_directory, paste0("sprite_", position, ".png")))
  },
  c("front", "back"),
  chosen_pokemon_links,
  MoreArgs = list(dir = temp_directory)
)

# open sprite imgs to be plotted
sprite1 <- png::readPNG(file.path(temp_directory, "sprite_front.png"))
sprite2 <- png::readPNG(file.path(temp_directory, "sprite_back.png"))
sprite2_cropped <- sprite2[-which(rowSums(sprite2[,,4]) == 0),,]

## make plot ====

background <- png::readPNG("./scaled_background.png")

# arrange onto a grid
x <- (1:225)/0.75
y <- 1:225

plot_location <- file.path(temp_directory, "battle_plot.png")
#plot_location <- file.path("~/Desktop", "battle_plot2.png")
grDevices::png(plot_location)
plot.new()
par(mar=c(0,0,0,0))
plot(y~x, axes = FALSE, col = NA, xlab = NA, ylab = NA, xaxs="i", yaxs="i")


rasterImage(background, 1, 1, 300, 225, interpolate = FALSE)
rasterImage(sprite2_cropped, 35, 54, 115, 54 + dim(sprite2_cropped)[1], interpolate = FALSE)
rasterImage(sprite1, 170, 130, 250, 210, interpolate = FALSE)

text(10, 187, paste0(toupper(pokemon_names[1]), " Lv", first_level), pos = 4, cex = 2)
text(180, 97.5, paste0(toupper(pokemon_names[2]), " Lv", chosen_second_level), pos = 4, cex = 2)

grDevices::dev.off()

## set up twitter auth ====
twitter_token <- rtweet::create_token(
  app = "pokebattlebowl_app",
  consumer_key =    Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

tweettext <- paste0(
  pokemon_names[2], " vs. ", pokemon_names[1], "!!!",
  "\n",
  pokemon_names[2], " knows ", gsub("-", " ", paste(pokemon_movesets$move[[2]], collapse = ", ")),
  "\n",
  pokemon_names[1], " knows ", gsub("-", " ", paste(pokemon_movesets$move[[1]], collapse = ", "))
)

rtweet::post_tweet(
  status = tweettext,
  media = plot_location,
  token = twitter_token
)
