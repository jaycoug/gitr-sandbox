# app.R — Kentucky Immunization Dashboard (prototype)
# Run: shiny::runApp()

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# ---- Prototype data (replace this with your real immunization dataset) ----
set.seed(42)

ky_counties <- c(
  "Adair","Allen","Anderson","Ballard","Barren","Bath","Bell","Boone","Bourbon","Boyd",
  "Boyle","Bracken","Breathitt","Breckinridge","Bullitt","Butler","Caldwell","Calloway",
  "Campbell","Carlisle","Carroll","Carter","Casey","Christian","Clark","Clay","Clinton",
  "Crittenden","Cumberland","Daviess","Edmonson","Elliott","Estill","Fayette","Fleming",
  "Floyd","Franklin","Fulton","Gallatin","Garrard","Grant","Graves","Grayson","Green",
  "Greenup","Hancock","Hardin","Harlan","Harrison","Hart","Henderson","Henry","Hickman",
  "Hopkins","Jackson","Jefferson","Jessamine","Johnson","Kenton","Knott","Knox","Larue",
  "Laurel","Lawrence","Lee","Leslie","Letcher","Lewis","Lincoln","Livingston","Logan",
  "Lyon","Madison","Magoffin","Marion","Marshall","Martin","Mason","McCracken","McCreary",
  "McLean","Meade","Menifee","Mercer","Metcalfe","Monroe","Montgomery","Morgan","Muhlenberg",
  "Nelson","Nicholas","Ohio","Oldham","Owen","Owsley","Pendleton","Perry","Pike","Powell",
  "Pulaski","Robertson","Rockcastle","Rowan","Russell","Scott","Shelby","Simpson","Spencer",
  "Taylor","Todd","Trigg","Trimble","Union","Warren","Washington","Wayne","Webster","Whitley",
  "Wolfe","Woodford"
)

vaccines   <- c("Influenza", "COVID-19", "MMR", "DTaP", "HPV")
age_groups <- c("0–4", "5–17", "18–49", "50–64", "65+")
months     <- seq.Date(as.Date("2023-01-01"), as.Date("2025-12-01"), by = "month")

immu <- tidyr::crossing(
  county = ky_counties,
  vaccine = vaccines,
  age_group = age_groups,
  month = months
) |>
  mutate(
    eligible = sample(200:6000, n(), replace = TRUE),
    doses = pmin(eligible, round(eligible * runif(n(), 0.30, 0.95))),
    coverage = doses / eligible
  )

# ---- Helpers ----
fmt_pct <- function(x) sprintf("%.1f%%", 100 * x)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Kentucky Immunization Dashboard (Prototype)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("vaccine", "Vaccine", choices = sort(unique(immu$vaccine)), selected = "Influenza"),
      selectInput("age", "Age group", choices = c("All", sort(unique(immu$age_group))), selected = "All"),
      selectInput("county", "County", choices = c("All", sort(unique(immu$county))), selected = "All"),
      dateRangeInput(
        "dates", "Date range",
        start = min(immu$month),
        end = max(immu$month),
        min = min(immu$month),
        max = max(immu$month)
      ),
      hr(),
      checkboxInput("show_ci", "Show simple uncertainty band (prototype)", value = FALSE),
      downloadButton("download_csv", "Download filtered data (CSV)")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Overview",
          fluidRow(
            column(4, wellPanel(h4("Coverage"), textOutput("kpi_coverage"))),
            column(4, wellPanel(h4("Doses"), textOutput("kpi_doses"))),
            column(4, wellPanel(h4("Eligible"), textOutput("kpi_eligible")))
          ),
          hr(),
          h4("Coverage over time"),
          plotOutput("ts_plot", height = 320),
          hr(),
          h4("County ranking (latest month in range)"),
          DTOutput("rank_table")
        ),
        tabPanel(
          "Geography",
          p("Prototype note: This tab shows a bar-based “map stand-in.” Replace with a real county choropleth using sf + tigris + ggplot/leaflet."),
          plotOutput("county_bar", height = 500)
        ),
        tabPanel(
          "Data",
          DTOutput("data_table")
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  filtered <- reactive({
    df <- immu |>
      filter(
        vaccine == input$vaccine,
        month >= input$dates[1],
        month <= input$dates[2]
      )
    
    if (input$age != "All") df <- df |> filter(age_group == input$age)
    if (input$county != "All") df <- df |> filter(county == input$county)
    
    df
  })
  
  # KPIs aggregated over current filters
  kpis <- reactive({
    df <- filtered()
    df |>
      summarise(
        doses = sum(doses),
        eligible = sum(eligible),
        coverage = ifelse(eligible == 0, NA_real_, doses / eligible)
      )
  })
  
  output$kpi_coverage <- renderText({
    x <- kpis()$coverage
    if (is.na(x)) "—" else fmt_pct(x)
  })
  output$kpi_doses <- renderText({ format(kpis()$doses, big.mark = ",") })
  output$kpi_eligible <- renderText({ format(kpis()$eligible, big.mark = ",") })
  
  output$ts_plot <- renderPlot({
    df <- filtered() |>
      group_by(month) |>
      summarise(doses = sum(doses), eligible = sum(eligible), .groups = "drop") |>
      mutate(coverage = ifelse(eligible == 0, NA_real_, doses / eligible))
    
    # quick-and-dirty binomial-ish band (purely illustrative)
    if (isTRUE(input$show_ci)) {
      df <- df |>
        mutate(
          se = sqrt(pmax(coverage * (1 - coverage) / pmax(eligible, 1), 0)),
          lo = pmax(coverage - 1.96 * se, 0),
          hi = pmin(coverage + 1.96 * se, 1)
        )
    }
    
    ggplot(df, aes(month, coverage)) +
      { if (isTRUE(input$show_ci)) geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) } +
      geom_line(linewidth = 1) +
      geom_point(size = 1.5) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
      labs(x = NULL, y = "Coverage", caption = "Prototype data") +
      theme_minimal(base_size = 12)
  })
  
  output$rank_table <- renderDT({
    df <- filtered()
    
    latest_month <- max(df$month)
    
    ranked <- df |>
      filter(month == latest_month) |>
      group_by(county) |>
      summarise(
        doses = sum(doses),
        eligible = sum(eligible),
        coverage = ifelse(eligible == 0, NA_real_, doses / eligible),
        .groups = "drop"
      ) |>
      arrange(desc(coverage)) |>
      mutate(coverage = ifelse(is.na(coverage), NA_character_, fmt_pct(coverage)))
    
    datatable(ranked, rownames = FALSE, options = list(pageLength = 10))
  })
  
  output$county_bar <- renderPlot({
    df <- filtered() |>
      group_by(county) |>
      summarise(doses = sum(doses), eligible = sum(eligible), .groups = "drop") |>
      mutate(coverage = ifelse(eligible == 0, NA_real_, doses / eligible)) |>
      arrange(desc(coverage)) |>
      slice_head(n = 30)
    
    ggplot(df, aes(reorder(county, coverage), coverage)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
      labs(x = NULL, y = "Coverage", title = "Top 30 counties by coverage (prototype)") +
      theme_minimal(base_size = 12)
  })
  
  output$data_table <- renderDT({
    df <- filtered() |>
      mutate(coverage = round(100 * coverage, 1))
    datatable(df, options = list(pageLength = 15))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("ky_immunization_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
