# =============================================================================
# 05_post_to_x.R
# Post approved smoke plume detections to X (Twitter) via API v2
# REQUIRES HUMAN APPROVAL — never posts automatically
# =============================================================================

library(httr2)
library(tidyverse)
library(jsonlite)
library(glue)

# ── Configuration ──────────────────────────────────────────────────────────────

# X API v2 credentials (OAuth 1.0a User Context for posting)
# Apply at https://developer.x.com/
# Set these in your .Renviron file
X_API_KEY        <- Sys.getenv("X_API_KEY")
X_API_SECRET     <- Sys.getenv("X_API_SECRET")
X_ACCESS_TOKEN   <- Sys.getenv("X_ACCESS_TOKEN")
X_ACCESS_SECRET  <- Sys.getenv("X_ACCESS_SECRET")
X_BEARER_TOKEN   <- Sys.getenv("X_BEARER_TOKEN")


# ── 1. OAuth 1.0a Signature ───────────────────────────────────────────────────

# X API v2 requires OAuth 1.0a for posting on behalf of a user
# We use httr2's req_oauth_auth_code or manual signing

build_oauth_header <- function(method, url, params = list()) {
  # Using the rtweet-style approach
  # For production, consider using the `rtweet` package directly
  
  nonce <- paste0(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE),
                  collapse = "")
  timestamp <- as.character(as.integer(Sys.time()))
  
  oauth_params <- list(
    oauth_consumer_key     = X_API_KEY,
    oauth_nonce            = nonce,
    oauth_signature_method = "HMAC-SHA1",
    oauth_timestamp        = timestamp,
    oauth_token            = X_ACCESS_TOKEN,
    oauth_version          = "1.0"
  )
  
  all_params <- c(oauth_params, params)
  all_params <- all_params[order(names(all_params))]
  
  param_string <- paste(
    sapply(names(all_params), function(k) {
      paste0(URLencode(k, reserved = TRUE), "=",
             URLencode(as.character(all_params[[k]]), reserved = TRUE))
    }),
    collapse = "&"
  )
  
  base_string <- paste(
    method,
    URLencode(url, reserved = TRUE),
    URLencode(param_string, reserved = TRUE),
    sep = "&"
  )
  
  signing_key <- paste(
    URLencode(X_API_SECRET, reserved = TRUE),
    URLencode(X_ACCESS_SECRET, reserved = TRUE),
    sep = "&"
  )
  
  signature <- openssl::sha1(charToRaw(base_string), key = signing_key) %>%
    base64enc::base64encode()
  
  oauth_params$oauth_signature <- signature
  
  header_string <- paste(
    sapply(names(oauth_params), function(k) {
      paste0(URLencode(k, reserved = TRUE), '="',
             URLencode(oauth_params[[k]], reserved = TRUE), '"')
    }),
    collapse = ", "
  )
  
  paste("OAuth", header_string)
}


# ── 2. Upload Media (Image) ───────────────────────────────────────────────────

upload_media <- function(image_path) {
  # Media upload uses v1.1 endpoint (still required for v2 tweets with media)
  
  url <- "https://upload.twitter.com/1.1/media/upload.json"
  
  # For images under 5MB, use simple upload
  file_size <- file.info(image_path)$size
  
  if (file_size > 5 * 1024 * 1024) {
    message("Image too large — compressing...")
    img <- magick::image_read(image_path)
    img <- magick::image_resize(img, "1920x1080>")
    temp_path <- tempfile(fileext = ".png")
    magick::image_write(img, temp_path, format = "png", quality = 85)
    image_path <- temp_path
  }
  
  auth_header <- build_oauth_header("POST", url)
  
  resp <- request(url) |>
    req_headers(Authorization = auth_header) |>
    req_body_multipart(
      media_data = base64enc::base64encode(image_path)
    ) |>
    req_perform()
  
  result <- resp_body_json(resp)
  media_id <- result$media_id_string
  
  message("Uploaded media: ", media_id)
  return(media_id)
}


# ── 3. Post Tweet ─────────────────────────────────────────────────────────────

post_tweet <- function(text, media_ids = NULL) {
  url <- "https://api.x.com/2/tweets"
  
  body <- list(text = text)
  
  if (!is.null(media_ids)) {
    body$media <- list(media_ids = as.list(media_ids))
  }
  
  auth_header <- build_oauth_header("POST", url)
  
  resp <- request(url) |>
    req_headers(
      Authorization  = auth_header,
      `Content-Type` = "application/json"
    ) |>
    req_body_json(body) |>
    req_perform()
  
  result <- resp_body_json(resp)
  
  tweet_id <- result$data$id
  message("Posted tweet: https://x.com/i/status/", tweet_id)
  
  return(result)
}


# ── 4. Publish Approved Posts ──────────────────────────────────────────────────

publish_approved <- function(post_queue_path = "data/post_queue.rds") {
  
  if (!file.exists(post_queue_path)) {
    stop("No post queue found at ", post_queue_path)
  }
  
  posts <- readRDS(post_queue_path)
  
  approved <- posts %>% filter(status == "approved")
  
  if (nrow(approved) == 0) {
    message("No approved posts to publish.")
    message("Use the Shiny review dashboard (app.R) to approve detections.")
    return(invisible(NULL))
  }
  
  message(glue("Publishing {nrow(approved)} approved posts..."))
  
  for (i in seq_len(nrow(approved))) {
    post <- approved[i, ]
    
    tryCatch({
      # Upload image
      media_id <- NULL
      if (!is.na(post$post_image_path) && file.exists(post$post_image_path)) {
        media_id <- upload_media(post$post_image_path)
      }
      
      # Post tweet
      result <- post_tweet(post$post_text, media_ids = media_id)
      
      # Update status
      posts$status[posts$cluster_id == post$cluster_id] <- "published"
      posts$tweet_id[posts$cluster_id == post$cluster_id] <- result$data$id
      posts$published_at[posts$cluster_id == post$cluster_id] <- Sys.time()
      
      message(glue("  ✓ Published: {post$location_name}"))
      
      # Rate limit: wait between posts
      Sys.sleep(5)
      
    }, error = function(e) {
      message(glue("  ✗ Failed: {post$location_name} — {e$message}"))
      posts$status[posts$cluster_id == post$cluster_id] <<- "failed"
      posts$error[posts$cluster_id == post$cluster_id] <<- e$message
    })
  }
  
  # Save updated queue
  saveRDS(posts, post_queue_path)
  
  n_published <- sum(posts$status == "published")
  message(glue("\nDone: {n_published} published, ",
               "{sum(posts$status == 'failed')} failed, ",
               "{sum(posts$status == 'pending_review')} still pending review"))
}


# ── 5. Alternative: Use rtweet (Simpler) ──────────────────────────────────────

# If you prefer, `rtweet` handles all the OAuth complexity:
#
# library(rtweet)
#
# auth <- rtweet_app(bearer_token = X_BEARER_TOKEN)
# # or for user-context posting:
# auth <- rtweet_bot(
#   api_key       = X_API_KEY,
#   api_secret    = X_API_SECRET,
#   access_token  = X_ACCESS_TOKEN,
#   access_secret = X_ACCESS_SECRET
# )
#
# post_tweet(
#   status    = post_text,
#   media     = post_image_path,
#   token     = auth
# )


# ── Run ───────────────────────────────────────────────────────────────────────

if (sys.nframe() == 0) {
  publish_approved()
}
