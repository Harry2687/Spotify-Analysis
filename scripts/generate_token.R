spotify_token <- POST(
  "https://accounts.spotify.com/api/token",
  authenticate("d17c5b9b944049b9bd68052d1c5eaeea",
               "bea6aa30966042ba970bff0491a57f31"),
  body = list(grant_type = "client_credentials"),
  encode = "form"
) %>%
  content() %>%
  pluck(1)