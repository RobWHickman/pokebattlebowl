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

# get the names of the pokemon

## get pokemon sprites ====
sprite_links  <- gsub("pokemon-species", "pokemon", pokemon_chosen)

temp_directory <- tempdir()

sprite_files <- mapply(
  function(position, link, dir) {
    pokemon_info <- httr::content(httr::GET(link))
    sprites <- pokemon_info$sprites$versions$`generation-iv`$`heartgold-soulsilver`
    selected_sprite <- sprites[[paste0(position, "_default")]]
    
    download.file(selected_sprite, file.path(dir, paste0("sprite_", position, ".png")))
  },
  c("front", "back"),
  sprite_links,
  MoreArgs = list(dir = temp_directory)
)

sprite1 <- png::readPNG(file.path(temp_directory, "sprite_front.png"))
sprite2 <- png::readPNG(file.path(temp_directory, "sprite_back.png"))

# arrange onto a grid
x <- 1:100
y <- 1:100
plot_location <- file.path(temp_directory, "battle_plot.png")
grDevices::png(plot_location)
plot.new()
plot(x~y, axes = FALSE, col = NA, xlab = NA, ylab = NA)
rasterImage(sprite2, 10, 10, 50, 50)
rasterImage(sprite1, 50, 50, 90, 90)
grDevices::dev.off()

## set up twitter auth ====
twitter_token <- rtweet::create_token(
  app = "pokebattlebowl_app",
  consumer_key =    Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

tweettext <- paste0(pokemon_names[2], " vs. ", pokemon_names[1], "!!!")

rtweet::post_tweet(
  status = tweettext,
  media = plot_location,
  token = twitter_token
)
