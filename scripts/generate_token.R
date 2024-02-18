spotify_token <- POST(
  "https://accounts.spotify.com/api/token",
  authenticate("983661a33f7e477eb72effaf65d556b5",
               "a9cca562c7eb451ca9173d45bd5564f9"),
  body = list(grant_type = "client_credentials"),
  encode = "form"
) %>%
  content() %>%
  pluck(1)