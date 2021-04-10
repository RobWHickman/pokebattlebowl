twitter_token <- rtweet::create_token(
  app = "pokebattlebowl_app",
  consumer_key =    Sys.getenv("TWITTER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_SECRET_KEY"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

tweettext <- paste0("I am a twitter bot test. The time is currently: ", Sys.time())

rtweet::post_tweet(
  status = tweettext,
  token = twitter_token
)
