# =============================================================================
# app.R
# Human-in-the-loop review dashboard
# Displays detected smoke plume events with satellite imagery and maps
# Allows approve / reject / edit before publishing to X
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(leaflet)
library(glue)
library(DT)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = "Smoke Plume Monitor — Review Dashboard",
  theme = bs_theme(
    bootswatch = "slate",
    primary    = "#f97316",
    base_font  = font_google("Source Sans Pro"),
    code_font  = font_google("JetBrains Mono")
  ),
  
  # ── Tab 1: Event Queue ──
  nav_panel(
    "Detection Queue",
    icon = icon("satellite"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 280,
        selectInput("confidence_filter", "Min Confidence",
                    choices = c("All" = "none", "Low" = "low",
                                "Medium" = "medium", "High" = "high"),
                    selected = "medium"),
        selectInput("status_filter", "Status",
                    choices = c("All", "pending_review", "approved",
                                "rejected", "published"),
                    selected = "pending_review"),
        actionButton("refresh_btn", "Refresh Queue",
                     class = "btn-primary w-100 mb-3"),
        hr(),
        actionButton("run_pipeline_btn", "Run Detection Pipeline",
                     class = "btn-warning w-100"),
        textOutput("pipeline_status")
      ),
      
      # Main content
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Detected Events"),
          DTOutput("events_table")
        )
      )
    )
  ),
  
  # ── Tab 2: Event Detail / Review ──
  nav_panel(
    "Review Event",
    icon = icon("eye"),
    
    layout_columns(
      col_widths = c(7, 5),
      
      # Left: Satellite imagery
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          textOutput("event_title"),
          div(
            actionButton("approve_btn", "Approve",
                         class = "btn-success btn-sm me-1"),
            actionButton("reject_btn", "Reject",
                         class = "btn-danger btn-sm me-1"),
            actionButton("skip_btn", "Skip",
                         class = "btn-secondary btn-sm")
          )
        ),
        navset_card_tab(
          nav_panel("True Color", imageOutput("true_color_img", height = "500px")),
          nav_panel("SWIR Composite", imageOutput("swir_img", height = "500px")),
          nav_panel("Smoke Enhancement", imageOutput("aerosol_img", height = "500px")),
          nav_panel("Post Preview", imageOutput("post_preview_img", height = "500px"))
        )
      ),
      
      # Right: Map + metadata + post editor
      layout_columns(
        col_widths = c(12),
        
        card(
          card_header("Location"),
          leafletOutput("event_map", height = "250px")
        ),
        
        card(
          card_header("Detection Metadata"),
          tableOutput("event_metadata")
        ),
        
        card(
          card_header("Post Text (editable)"),
          textAreaInput("post_text_edit", NULL, rows = 8, width = "100%"),
          actionButton("save_text_btn", "Save Edits",
                       class = "btn-outline-primary btn-sm")
        )
      )
    )
  ),
  
  # ── Tab 3: Published ──
  nav_panel(
    "Published",
    icon = icon("check-circle"),
    card(
      card_header("Published Posts"),
      DTOutput("published_table")
    )
  ),
  
  # ── Tab 4: Map Overview ──
  nav_panel(
    "Map Overview",
    icon = icon("globe"),
    card(
      card_header("All Detected Events"),
      leafletOutput("overview_map", height = "700px")
    )
  )
)


# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # Reactive: load post queue
  post_queue <- reactiveVal(tibble())
  selected_event <- reactiveVal(NULL)
  
  load_queue <- function() {
    if (file.exists("data/post_queue.rds")) {
      post_queue(readRDS("data/post_queue.rds"))
    }
  }
  
  observe({ load_queue() })
  
  observeEvent(input$refresh_btn, { load_queue() })
  
  # ── Events Table ──
  
  filtered_events <- reactive({
    df <- post_queue()
    if (nrow(df) == 0) return(df)
    
    conf_levels <- c("none" = 0, "low" = 1, "medium" = 2, "high" = 3)
    
    if (input$confidence_filter != "none") {
      min_conf <- conf_levels[input$confidence_filter]
      df <- df %>% filter(conf_levels[smoke_confidence] >= min_conf)
    }
    
    if (input$status_filter != "All") {
      df <- df %>% filter(status == input$status_filter)
    }
    
    df
  })
  
  output$events_table <- renderDT({
    df <- filtered_events()
    if (nrow(df) == 0) return(datatable(tibble(Message = "No events found")))
    
    df %>%
      select(cluster_id, location_name, first_detected,
             smoke_confidence, n_detections, total_frp, score, status) %>%
      datatable(
        selection = "single",
        options = list(pageLength = 15, order = list(list(6, "desc"))),
        rownames = FALSE
      ) %>%
      formatRound(c("total_frp", "score"), 1)
  })
  
  # ── Select Event for Review ──
  
  observeEvent(input$events_table_rows_selected, {
    row <- input$events_table_rows_selected
    if (!is.null(row)) {
      event <- filtered_events()[row, ]
      selected_event(event)
      
      # Populate post text editor
      if (!is.null(event$post_text)) {
        updateTextAreaInput(session, "post_text_edit", value = event$post_text)
      }
    }
  })
  
  output$event_title <- renderText({
    event <- selected_event()
    if (is.null(event)) return("Select an event from the queue")
    glue("{event$location_name} — {format(event$first_detected, '%d %b %Y %H:%M UTC')}")
  })
  
  # ── Satellite Imagery ──
  
  render_event_image <- function(path_col) {
    renderImage({
      event <- selected_event()
      if (is.null(event)) {
        return(list(src = "", alt = "Select an event"))
      }
      path <- event[[path_col]]
      if (is.na(path) || !file.exists(path)) {
        return(list(src = "", alt = "Image not available"))
      }
      list(src = path, alt = path_col, width = "100%")
    }, deleteFile = FALSE)
  }
  
  output$true_color_img <- render_event_image("true_color_path")
  output$swir_img       <- render_event_image("swir_path")
  output$aerosol_img    <- render_event_image("aerosol_path")
  output$post_preview_img <- render_event_image("post_image_path")
  
  # ── Event Map ──
  
  output$event_map <- renderLeaflet({
    event <- selected_event()
    if (is.null(event)) {
      return(leaflet() %>% addTiles() %>% setView(42, 33, zoom = 4))
    }
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(event$center_lon, event$center_lat, zoom = 10) %>%
      addCircleMarkers(
        lng = event$center_lon, lat = event$center_lat,
        radius = 12, color = "#f97316", fillOpacity = 0.6,
        popup = glue("<b>{event$location_name}</b><br>",
                     "FRP: {event$total_frp} MW<br>",
                     "Confidence: {event$smoke_confidence}")
      )
  })
  
  # ── Event Metadata ──
  
  output$event_metadata <- renderTable({
    event <- selected_event()
    if (is.null(event)) return(NULL)
    
    tibble(
      Field = c("Cluster ID", "Detections", "Max FRP (MW)",
                "Total FRP (MW)", "First Detected", "Last Detected",
                "Nighttime", "Smoke Confidence", "Smoke Fraction", "Score"),
      Value = c(
        as.character(event$cluster_id),
        as.character(event$n_detections),
        round(event$max_frp, 1),
        round(event$total_frp, 1),
        format(event$first_detected, "%Y-%m-%d %H:%M UTC"),
        format(event$last_detected, "%Y-%m-%d %H:%M UTC"),
        ifelse(event$any_nighttime, "Yes", "No"),
        event$smoke_confidence,
        ifelse(is.na(event$smoke_fraction), "N/A",
               paste0(round(event$smoke_fraction * 100, 1), "%")),
        round(event$score, 1)
      )
    )
  })
  
  # ── Approve / Reject ──
  
  update_status <- function(new_status) {
    event <- selected_event()
    if (is.null(event)) return()
    
    df <- post_queue()
    df$status[df$cluster_id == event$cluster_id] <- new_status
    
    # Save edited post text
    df$post_text[df$cluster_id == event$cluster_id] <- input$post_text_edit
    
    saveRDS(df, "data/post_queue.rds")
    post_queue(df)
    
    showNotification(
      glue("Event {event$cluster_id} — {new_status}"),
      type = switch(new_status,
                    approved = "message",
                    rejected = "warning",
                    "default")
    )
    
    # Auto-advance to next pending event
    pending <- df %>% filter(status == "pending_review")
    if (nrow(pending) > 0) {
      selected_event(pending[1, ])
      updateTextAreaInput(session, "post_text_edit",
                          value = pending$post_text[1])
    }
  }
  
  observeEvent(input$approve_btn, { update_status("approved") })
  observeEvent(input$reject_btn,  { update_status("rejected") })
  observeEvent(input$skip_btn, {
    df <- filtered_events()
    event <- selected_event()
    if (is.null(event) || nrow(df) < 2) return()
    current_row <- which(df$cluster_id == event$cluster_id)
    next_row <- ifelse(current_row < nrow(df), current_row + 1, 1)
    selected_event(df[next_row, ])
    updateTextAreaInput(session, "post_text_edit",
                        value = df$post_text[next_row])
  })
  
  observeEvent(input$save_text_btn, {
    event <- selected_event()
    if (is.null(event)) return()
    df <- post_queue()
    df$post_text[df$cluster_id == event$cluster_id] <- input$post_text_edit
    saveRDS(df, "data/post_queue.rds")
    post_queue(df)
    showNotification("Post text saved", type = "message")
  })
  
  # ── Published Table ──
  
  output$published_table <- renderDT({
    df <- post_queue()
    if (nrow(df) == 0) return(datatable(tibble()))
    
    df %>%
      filter(status == "published") %>%
      select(cluster_id, location_name, first_detected,
             smoke_confidence, published_at, tweet_id) %>%
      datatable(rownames = FALSE)
  })
  
  # ── Overview Map ──
  
  output$overview_map <- renderLeaflet({
    df <- post_queue()
    if (nrow(df) == 0) {
      return(leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
               setView(42, 33, zoom = 5))
    }
    
    pal <- colorFactor(
      palette = c("gray", "orange", "green", "red", "blue"),
      domain = c("pending_review", "approved", "rejected", "published", "failed")
    )
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(
        lng = ~center_lon, lat = ~center_lat,
        radius = ~pmin(total_frp / 5, 20),
        color = ~pal(status),
        fillOpacity = 0.7,
        popup = ~glue("<b>{location_name}</b><br>",
                      "Confidence: {smoke_confidence}<br>",
                      "FRP: {total_frp} MW<br>",
                      "Status: {status}")
      ) %>%
      addLegend(pal = pal,
                values = c("pending_review", "approved",
                           "rejected", "published"),
                title = "Status")
  })
  
  # ── Run Pipeline ──
  
  observeEvent(input$run_pipeline_btn, {
    output$pipeline_status <- renderText("Running detection pipeline...")
    
    tryCatch({
      source("01_detect_hotspots.R", local = TRUE)
      events <- run_detection(days_back = 1)
      
      if (nrow(events) > 0) {
        source("02_pull_imagery.R", local = TRUE)
        events <- pull_imagery_for_events(events)
        
        source("03_classify_smoke.R", local = TRUE)
        events <- classify_all_events(events)
        
        source("04_generate_post.R", local = TRUE)
        posts <- generate_all_posts(events)
        
        load_queue()
        output$pipeline_status <- renderText(
          glue("Done: {nrow(posts)} events detected")
        )
      } else {
        output$pipeline_status <- renderText("No events detected")
      }
    }, error = function(e) {
      output$pipeline_status <- renderText(glue("Error: {e$message}"))
    })
  })
}


# ── Launch ────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
