library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(visNetwork)
library(igraph)

# Function to calculate influence metrics
calculate_influence <- function(data, window = 30) {
  upstream_list <- list()
  downstream_list <- list()

  for (i in 1:nrow(data)) {
    focal <- data[i, ]
    f_start <- focal$start_day
    f_end <- focal$end_day

    upstream <- c()
    downstream <- c()

    for (j in 1:nrow(data)) {
      if (i == j) next
      other <- data[j, ]
      o_start <- other$start_day
      o_end <- other$end_day

      if (o_start >= f_start && o_start <= f_end + window) {
        downstream <- c(downstream, other$species)
      }
      if (o_end <= f_end && o_end >= f_start - window) {
        upstream <- c(upstream, other$species)
      }
    }

    upstream_list[[focal$species]] <- unique(upstream)
    downstream_list[[focal$species]] <- unique(downstream)
  }

  list(
    data = data.frame(
      species = names(upstream_list),
      upstream_count = sapply(upstream_list, length),
      downstream_count = sapply(downstream_list, length),
      stringsAsFactors = FALSE
    ),
    downstream_edges = downstream_list
  )
}

# Function to compute burstiness and memory for each species
compute_temporal_variability <- function(data) {
  result <- data %>%
    group_by(species) %>%
    summarise(
      starts = list(sort(start_day)),
      ends = list(sort(end_day))
    ) %>%
    rowwise() %>%
    mutate(
      inter_event_times = list(diff(unlist(starts))),
      burstiness = ifelse(length(inter_event_times[[1]]) >= 2,
                          (sd(inter_event_times[[1]]) - mean(inter_event_times[[1]])) /
                            (sd(inter_event_times[[1]]) + mean(inter_event_times[[1]])),
                          NA),
      memory = ifelse(length(inter_event_times[[1]]) >= 3,
                      cor(inter_event_times[[1]][-length(inter_event_times[[1]])],
                          inter_event_times[[1]][-1]),
                      NA)
    ) %>%
    select(species, burstiness, memory)

  result
}
# ---- UI ----
ui <- fluidPage(
  tags$head(tags$style(HTML(".container-fluid { width: 100% !important; padding: 0 2em; }"))),
  titlePanel("🌸 Phenological Influence Explorer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Phenology CSV", accept = ".csv"),
      sliderInput("shift", "Shift days (advance flowering)", min = 0, max = 60, value = 30),
      uiOutput("species_selector"),
      actionButton("apply_shift", "Apply Shift", icon = icon("sync"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart", plotOutput("influencePlot")),
        tabPanel("Change Summary", tableOutput("summaryTable")),
        tabPanel("Timeline", plotOutput("timelinePlot")),
        tabPanel("Influence Network", visNetworkOutput("networkPlot", height = "700px")),
        tabPanel("Network Metrics",
                 downloadButton("downloadMetrics", "Download CSV"),
                 br(), br(),
                 tableOutput("networkStats")),
        tabPanel("📈 Network-Level Summary",tableOutput("networkSummary")),
        tabPanel("Metric Descriptions", includeMarkdown("Metrics.md"))
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {



  default_data <- reactive({
    read_csv("vol-rud_cal-phene-2023.csv")
  })

  input_data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })

  base_data <- reactive({
    if (is.null(input$file)) default_data() else input_data()
  })

  output$species_selector <- renderUI({
    species <- unique(base_data()$species)
    checkboxGroupInput("selected_species", "Select species to shift:", choices = species, selected = head(species, 5))
  })

  shifted_data <- eventReactive(input$apply_shift, {
    req(input$selected_species)
    base_data() %>%
      mutate(
        start_day = ifelse(species %in% input$selected_species, start_day - input$shift, start_day),
        end_day = ifelse(species %in% input$selected_species, end_day - input$shift, end_day)
      )
  })

  influence_before <- reactive({
    calculate_influence(base_data())
  })

  influence_after <- reactive({
    req(shifted_data())
    calculate_influence(shifted_data())
  })

  compare_data <- reactive({
    full_join(
      influence_before()$data %>% rename(before = downstream_count),
      influence_after()$data %>% rename(after = downstream_count),
      by = "species"
    ) %>% mutate(change = after - before)
  })

  output$influencePlot <- renderPlot({
    ggplot(compare_data(), aes(x = reorder(species, change), y = change)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Change in Downstream Influence", x = "Species", y = "Change") +
      theme_minimal(base_size = 24)
  })

  output$summaryTable <- renderTable({
    compare_data() %>% arrange(desc(abs(change))) %>% head(20)
  })

  output$timelinePlot <- renderPlot({
    df <- shifted_data()
    df$keystone <- ifelse(df$species %in% input$selected_species, "Shifted", "Other")

    ggplot(df, aes(x = start_day, xend = end_day, y = reorder(species, start_day), yend = species, color = keystone)) +
      geom_segment(linewidth = 2) +
      labs(title = "Flowering Timelines (Shifted Highlighted)", x = "Day of Year", y = "Species") +
      scale_color_manual(values = c("Shifted" = "red", "Other" = "gray")) +
      theme_minimal(base_size = 24)
  })

  network_metrics <- reactive({
    influence <- influence_after()
    edges_all <- list()

    for (sp in names(influence$downstream_edges)) {
      to_species <- influence$downstream_edges[[sp]]
      if (length(to_species) > 0) {
        edges_all[[sp]] <- data.frame(from = rep(sp, length(to_species)), to = to_species, stringsAsFactors = FALSE)
      }
    }
    edges <- do.call(rbind, edges_all)
    g <- graph_from_data_frame(edges, directed = TRUE)

    downstream_size <- sapply(V(g)$name, function(n) length(subcomponent(g, n, mode = "out")) - 1)
    upstream_size <- sapply(V(g)$name, function(n) length(subcomponent(g, n, mode = "in")) - 1)

    dist_mat <- distances(g, mode = "out")
    avg_latency <- apply(dist_mat, 1, function(x) mean(x[is.finite(x) & x != 0], na.rm = TRUE))
    temporal_closeness <- 1 / avg_latency

    data.frame(
      species = V(g)$name,
      downstream_set = downstream_size,
      upstream_set = upstream_size,
      avg_temporal_distance = round(avg_latency, 2),
      temporal_closeness = round(temporal_closeness, 3),
      stringsAsFactors = FALSE
    )
  })

  output$networkStats <- renderTable({
    network_metrics()
  })

  output$downloadMetrics <- downloadHandler(
    filename = function() { "temporal_network_metrics.csv" },
    content = function(file) {
      write.csv(network_metrics(), file, row.names = FALSE)
    }
  )

  output$networkSummary <- renderTable({
    influence <- influence_after()
    edges_all <- list()

    for (sp in names(influence$downstream_edges)) {
      to_species <- influence$downstream_edges[[sp]]
      if (length(to_species) > 0) {
        edges_all[[sp]] <- data.frame(from = rep(sp, length(to_species)), to = to_species, stringsAsFactors = FALSE)
      }
    }

    edges <- do.call(rbind, edges_all)
    g <- igraph::graph_from_data_frame(edges, directed = TRUE)

    dist_mat <- igraph::distances(g, mode = "out")
    avg_dist <- mean(dist_mat[is.finite(dist_mat) & dist_mat != 0], na.rm = TRUE)
    clustering <- igraph::transitivity(g, type = "global")
    reach_pct <- mean(is.finite(dist_mat)) * 100
    isolated <- sum(igraph::degree(g) == 0)

    data.frame(
      Metric = c("Avg. Temporal Distance", "Global Clustering Coefficient", "% Reachable Pairs", "Isolated Nodes"),
      Value = round(c(avg_dist, clustering, reach_pct, isolated), 3),
      check.names = FALSE
    )
  })

  output$networkPlot <- renderVisNetwork({
    influence <- influence_after()
    edges_all <- list()

    for (sp in names(influence$downstream_edges)) {
      to_species <- influence$downstream_edges[[sp]]
      if (length(to_species) > 0) {
        edges_all[[sp]] <- data.frame(from = rep(sp, length(to_species)), to = to_species, stringsAsFactors = FALSE)
      }
    }

    edges <- do.call(rbind, edges_all)
    node_ids <- unique(c(edges$from, edges$to))
    nodes <- data.frame(
      id = node_ids,
      label = node_ids,
      color = ifelse(node_ids %in% input$selected_species, "red", "lightgray"),
      stringsAsFactors = FALSE
    )

    visNetwork(nodes, edges, height = "500px", width = "100%") %>%
      visEdges(arrows = "to") %>%
      visInteraction(navigationButtons = TRUE, dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42, improvedLayout = TRUE) %>%
      visPhysics(enabled = FALSE)
  })
}

shinyApp(ui, server)
