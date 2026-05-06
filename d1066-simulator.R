#' Risk-like Battle Simulator in Shiny
#'
#' This Shiny application allows you to explore the outcome of a simplified
#' Risk-like battle.  You can specify the number of attacking and defending
#' units, run a large number of stochastic simulations, and view the
#' resulting distribution of final unit counts.  The underlying mechanics
#' are based on the R code provided by the user: attackers roll dice of
#' various sizes (d6, d12, d20) and eliminate defending units on rolls of
#' 6 or higher; defenders likewise remove attacking units.  A battle
#' continues until all attackers or all defenders (including the castle)
#' have been defeated.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)

# ── Simulation helpers ───────────────────────────────────────────────────

reduce_units <- function(units, amount) {
  taken <- pmin(units, amount - c(0, head(cumsum(units), -1)))
  taken <- pmax(taken, 0)
  units - taken
}

simulate_battle <- function(units) {
  atk_rolls <- c(
    if (units[1] > 0) sample(1:6, units[1], replace = TRUE) else numeric(0),
    if (units[2] > 0) sample(1:12, units[2], replace = TRUE) else numeric(0),
    if (units[3] > 0) sample(1:20, units[3], replace = TRUE) else numeric(0)
  )
  def_rolls <- c(
    if (units[5] > 0) sample(1:6, units[5], replace = TRUE) else numeric(0),
    if (units[6] > 0) sample(1:12, units[6], replace = TRUE) else numeric(0),
    if (units[7] > 0) sample(1:20, units[7], replace = TRUE) else numeric(0)
  )
  atk_kills <- sum(atk_rolls >= 6)
  def_kills <- sum(def_rolls >= 6)
  def_units <- reduce_units(units[4:7], atk_kills)
  atk_units <- reduce_units(units[1:3], def_kills)
  c(atk_units, def_units)
}

battle_over <- function(units) {
  sum(units[1:3]) == 0 || sum(units[4:7]) == 0
}

run_simulations <- function(init_units, n_sims = 1000) {
  simulation_results <- replicate(n_sims, {
    state <- init_units
    while (!battle_over(state)) {
      state <- simulate_battle(state)
    }
    state
  })
  results_df <- as.data.frame(t(simulation_results))
  colnames(results_df) <- c("atk_d6", "atk_d12", "atk_d20",
                             "def_castle", "def_d6", "def_d12", "def_d20")
  results_df$winner <- ifelse(
    rowSums(results_df[, c("atk_d6", "atk_d12", "atk_d20")]) > 0,
    "Attacker", "Defender"
  )
  results_df <- results_df %>% dplyr::relocate(winner)
  results_df
}

# ── Theme ────────────────────────────────────────────────────────────────

app_theme <- bs_theme(
  version   = 5,
  bootswatch = "slate",
  primary   = "#e74c3c",
  secondary = "#3498db",
  success   = "#2ecc71",
  "font-size-base" = "0.92rem"
)

# ── Styled numeric input helper ──────────────────────────────────────────

unit_input <- function(id, label, value, icon_text = NULL) {
  numericInput(id, label, value = value, min = 0, step = 1)
}

# ── UI ───────────────────────────────────────────────────────────────────

ui <- function(request) page_sidebar(
  theme = app_theme,
  title = div(
    style = "display: flex; align-items: center; gap: 10px;",
    span(style = "font-size: 1.6rem;", "\u2694\uFE0F"),
    span("d1066 Battle Simulator")
  ),

  # ── Keep-alive heartbeat + custom disconnect overlay ──
  tags$head(
    tags$script(HTML("
      // Send a heartbeat every 30s to prevent idle timeout
      setInterval(function() {
        Shiny.setInputValue('heartbeat', new Date().getTime());
      }, 30000);

      // Replace the default grey disconnect screen with a friendly reload banner
      $(document).on('shiny:disconnected', function() {
        $('#shiny-disconnected-overlay').remove();
        if (!$('#custom-reconnect').length) {
          $('body').prepend(
            '<div id=\"custom-reconnect\" style=\"position:fixed;top:0;left:0;right:0;' +
            'z-index:99999;background:#e74c3c;color:white;text-align:center;' +
            'padding:12px;font-size:15px;font-family:sans-serif;\">' +
            'Connection lost. <a href=\"' + window.location.href + '\" ' +
            'style=\"color:white;text-decoration:underline;font-weight:bold;\">' +
            'Click here to reload</a> (your inputs are saved in the URL).</div>'
          );
        }
      });
    "))
  ),

  sidebar = sidebar(
    width = 320,
    title = "Battle Setup",

    # ── Attacker inputs ──
    card(
      card_header(
        class = "bg-danger text-white",
        span("\u2694\uFE0F Attackers")
      ),
      card_body(
        class = "pt-2 pb-1",
        layout_columns(
          col_widths = c(4, 4, 4),
          unit_input("atk_d6",  "d6",  2),
          unit_input("atk_d12", "d12", 2),
          unit_input("atk_d20", "d20", 2)
        )
      )
    ),

    # ── Defender inputs ──
    card(
      card_header(
        class = "bg-primary text-white",
        span("\uD83D\uDEE1\uFE0F Defenders")
      ),
      card_body(
        class = "pt-2 pb-1",
        numericInput("def_castle", "\uD83C\uDFF0 Castles", value = 0, min = 0, step = 1),
        layout_columns(
          col_widths = c(4, 4, 4),
          unit_input("def_d6",  "d6",  2),
          unit_input("def_d12", "d12", 2),
          unit_input("def_d20", "d20", 2)
        )
      )
    ),

    # ── Simulation controls ──
    numericInput("sims", "Simulations", value = 1000, min = 1, step = 100),
    actionButton("run", "\u25B6 Run Simulation",
                 class = "btn-success btn-lg w-100 mt-2")
  ),

  # ── Main panel ──────────────────────────────────────────────────────

  # Gauge + summary cards row
  layout_columns(
    col_widths = c(5, 7),
    card(
      card_header("Attacker Win Probability"),
      card_body(
        plotlyOutput("win_gauge", height = "250px")
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title    = "Attacker Win %",
        value    = textOutput("atk_pct", inline = TRUE),
        showcase = span(style = "font-size: 2.5rem;", "\u2694\uFE0F"),
        theme    = "danger"
      ),
      value_box(
        title    = "Defender Win %",
        value    = textOutput("def_pct", inline = TRUE),
        showcase = span(style = "font-size: 2.5rem;", "\uD83D\uDEE1\uFE0F"),
        theme    = "primary"
      ),
      value_box(
        title    = "Simulations Run",
        value    = textOutput("n_sims_display", inline = TRUE),
        showcase = span(style = "font-size: 2.5rem;", "\uD83C\uDFB2"),
        theme    = "secondary"
      )
    )
  ),

  # Chart + Table
  layout_columns(
    col_widths = c(5, 7),
    card(
      card_header("Outcome Distribution"),
      card_body(plotOutput("dist_plot", height = "400px"))
    ),
    card(
      card_header("Marginal Unit Value"),
      card_body(
        p(class = "text-muted small",
          "Win-rate change from adding +1 of each unit type to the current setup."),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class = "bg-danger text-white", "\u2694\uFE0F Attacker +1"),
            card_body(
              uiOutput("atk_marginal")
            )
          ),
          card(
            card_header(class = "bg-primary text-white", "\uD83D\uDEE1\uFE0F Defender +1"),
            card_body(
              uiOutput("def_marginal")
            )
          )
        ),
        hr(),
        uiOutput("recommendation")
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Allow automatic reconnection after a WebSocket drop
  session$allowReconnect("force")

  # Auto-update URL with current inputs so refresh preserves state
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)

  raw_results <- reactive({
    init_units <- c(input$atk_d6, input$atk_d12, input$atk_d20,
                    input$def_castle, input$def_d6, input$def_d12, input$def_d20)
    run_simulations(init_units, input$sims)
  }) %>% bindCache(input$atk_d6, input$atk_d12, input$atk_d20,
                   input$def_castle, input$def_d6, input$def_d12, input$def_d20,
                   input$sims)

  # ── Win probability gauge ──

  output$win_gauge <- renderPlotly({
    req(raw_results())
    win_pct <- mean(raw_results()$winner == "Attacker") * 100

    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = win_pct,
      number = list(suffix = "%", font = list(size = 36, color = "#dee2e6")),
      gauge = list(
        axis = list(
          range = list(0, 100),
          tickwidth = 2,
          tickcolor = "#adb5bd",
          tickfont = list(color = "#adb5bd"),
          dtick = 25
        ),
        bar = list(color = "#e74c3c", thickness = 0.7),
        bgcolor = "#3a3f44",
        borderwidth = 0,
        steps = list(
          list(range = c(0, 25),  color = "#2c3e50"),
          list(range = c(25, 50), color = "#34495e"),
          list(range = c(50, 75), color = "#34495e"),
          list(range = c(75, 100), color = "#2c3e50")
        ),
        threshold = list(
          line = list(color = "#2ecc71", width = 3),
          thickness = 0.8,
          value = 50
        )
      )
    ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        margin = list(t = 40, b = 20, l = 30, r = 30),
        font = list(color = "#dee2e6")
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── Value boxes ──

  output$atk_pct <- renderText({
    req(raw_results())
    pct <- mean(raw_results()$winner == "Attacker") * 100
    sprintf("%.1f%%", pct)
  })

  output$def_pct <- renderText({
    req(raw_results())
    pct <- mean(raw_results()$winner == "Defender") * 100
    sprintf("%.1f%%", pct)
  })

  output$n_sims_display <- renderText({
    req(raw_results())
    format(nrow(raw_results()), big.mark = ",")
  })

  # ── Distribution chart ──

  output$dist_plot <- renderPlot({
    req(raw_results())
    df <- raw_results()

    # Compute surviving units; defender wins shown as negative
    df$outcome <- ifelse(
      df$winner == "Attacker",
      df$atk_d6 + df$atk_d12 + df$atk_d20,
      -(df$def_d6 + df$def_d12 + df$def_d20 + df$def_castle)
    )

    plot_data <- data.frame(
      outcome = df$outcome,
      winner  = df$winner
    )

    # Compute axis range symmetrically
    max_abs <- max(abs(plot_data$outcome), 1)

    ggplot(plot_data, aes(x = outcome, fill = winner)) +
      geom_histogram(
        binwidth = 1, alpha = 0.85,
        color = "white", linewidth = 0.3
      ) +
      scale_fill_manual(values = c("Attacker" = "#e74c3c",
                                   "Defender" = "#3498db")) +
      scale_x_continuous(
        limits = c(-max_abs - 0.5, max_abs + 0.5),
        labels = function(x) abs(x)
      ) +
      geom_vline(xintercept = 0, color = "#adb5bd", linewidth = 0.6, linetype = "dashed") +
      annotate("text", x = -max_abs * 0.5, y = Inf, label = "\u2190 Defender wins",
               color = "#3498db", vjust = 2, size = 4.5, fontface = "bold") +
      annotate("text", x = max_abs * 0.5, y = Inf, label = "Attacker wins \u2192",
               color = "#e74c3c", vjust = 2, size = 4.5, fontface = "bold") +
      labs(x = "Surviving Units", y = "Frequency", fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "#272b30", color = NA),
        panel.background = element_rect(fill = "#272b30", color = NA),
        panel.grid.major = element_line(color = "#3a3f44"),
        panel.grid.minor = element_blank(),
        text             = element_text(color = "#dee2e6"),
        axis.text        = element_text(color = "#adb5bd"),
        legend.position  = "none"
      )
  })

  # ── Marginal benefit analysis ──

  base_win_rate <- reactive({
    req(raw_results())
    mean(raw_results()$winner == "Attacker")
  })

  # Helper: compute attacker win-rate for a modified unit vector
  marginal_win_rate <- function(init_units, n_sims) {
    res <- run_simulations(init_units, n_sims)
    mean(res$winner == "Attacker")
  }

  # Unit costs
  unit_cost <- c(d6 = 1, d12 = 2, d20 = 2)

  marginal_data <- reactive({
    req(raw_results())
    base <- base_win_rate()
    init <- c(input$atk_d6, input$atk_d12, input$atk_d20,
              input$def_castle, input$def_d6, input$def_d12, input$def_d20)
    n <- input$sims

    # Attacker +1 of each type
    atk_d6_rate  <- marginal_win_rate(init + c(1,0,0, 0,0,0,0), n)
    atk_d12_rate <- marginal_win_rate(init + c(0,1,0, 0,0,0,0), n)
    atk_d20_rate <- marginal_win_rate(init + c(0,0,1, 0,0,0,0), n)

    # Defender +1 of each type (castles are not purchasable)
    def_d6_rate     <- marginal_win_rate(init + c(0,0,0, 0,1,0,0), n)
    def_d12_rate    <- marginal_win_rate(init + c(0,0,0, 0,0,1,0), n)
    def_d20_rate    <- marginal_win_rate(init + c(0,0,0, 0,0,0,1), n)

    atk_raw <- c(d6 = atk_d6_rate - base, d12 = atk_d12_rate - base, d20 = atk_d20_rate - base)
    def_raw <- c(d6 = -(def_d6_rate - base),
                 d12 = -(def_d12_rate - base), d20 = -(def_d20_rate - base))

    # Per-cost efficiency
    atk_eff <- atk_raw / unit_cost[names(atk_raw)]
    def_eff <- def_raw / unit_cost[names(def_raw)]

    list(base = base, atk = atk_raw, def = def_raw, atk_eff = atk_eff, def_eff = def_eff)
  }) %>% bindCache(input$atk_d6, input$atk_d12, input$atk_d20,
                   input$def_castle, input$def_d6, input$def_d12, input$def_d20,
                   input$sims)

  # Format a single marginal row with cost info
  marginal_badge <- function(label, delta, efficiency, cost, color) {
    sign_char <- ifelse(delta >= 0, "+", "")
    pct_text  <- sprintf("%s%.1f%%", sign_char, delta * 100)
    eff_text  <- sprintf("%.1f%% per cost", efficiency * 100)
    bar_width <- min(abs(delta) * 100 / 0.5 * 100, 100)  # scale: 50pp = full bar
    tags$div(
      style = "margin-bottom: 12px;",
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 2px;",
        tags$span(style = "font-weight: 600; color: #dee2e6; font-size: 1.05rem;", label),
        tags$span(
          style = paste0("font-weight: 700; font-size: 1.15rem; color:", color, ";"),
          pct_text
        )
      ),
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;",
        tags$span(style = "color: #8e9297; font-size: 0.82rem;",
                  paste0("Cost: ", cost)),
        tags$span(style = "color: #8e9297; font-size: 0.82rem; font-style: italic;",
                  eff_text)
      ),
      tags$div(
        style = "background: #3a3f44; border-radius: 4px; height: 8px; overflow: hidden;",
        tags$div(style = paste0(
          "width:", bar_width, "%; height: 100%; background:", color, "; border-radius: 4px;"
        ))
      )
    )
  }

  output$atk_marginal <- renderUI({
    req(marginal_data())
    m   <- marginal_data()$atk
    eff <- marginal_data()$atk_eff
    best <- names(which.max(eff))
    tagList(
      marginal_badge(paste0("+1 d6",  if (best == "d6")  " \u2B50" else ""),  m["d6"],  eff["d6"],  unit_cost["d6"],  "#e74c3c"),
      marginal_badge(paste0("+1 d12", if (best == "d12") " \u2B50" else ""), m["d12"], eff["d12"], unit_cost["d12"], "#e74c3c"),
      marginal_badge(paste0("+1 d20", if (best == "d20") " \u2B50" else ""), m["d20"], eff["d20"], unit_cost["d20"], "#e74c3c")
    )
  })

  output$def_marginal <- renderUI({
    req(marginal_data())
    m   <- marginal_data()$def
    eff <- marginal_data()$def_eff
    best <- names(which.max(eff))
    tagList(
      marginal_badge(paste0("+1 d6",     if (best == "d6")     " \u2B50" else ""), m["d6"],     eff["d6"],     unit_cost["d6"],     "#3498db"),
      marginal_badge(paste0("+1 d12",    if (best == "d12")    " \u2B50" else ""), m["d12"],    eff["d12"],    unit_cost["d12"],    "#3498db"),
      marginal_badge(paste0("+1 d20",    if (best == "d20")    " \u2B50" else ""), m["d20"],    eff["d20"],    unit_cost["d20"],    "#3498db")
    )
  })

  output$recommendation <- renderUI({
    req(marginal_data())
    m <- marginal_data()
    # Best buy = highest efficiency (win-rate per cost)
    atk_best <- names(which.max(m$atk_eff))
    def_best <- names(which.max(m$def_eff))
    atk_val  <- sprintf("+%.1f%% per cost", max(m$atk_eff) * 100)
    def_val  <- sprintf("+%.1f%% per cost", max(m$def_eff) * 100)

    tags$div(
      style = "padding: 8px 0;",
      tags$p(
        style = "font-size: 1rem; color: #dee2e6; margin-bottom: 6px;",
        tags$strong("Best attacker buy: "),
        tags$span(style = "color: #e74c3c; font-weight: 700;",
                  paste0(atk_best, " (", atk_val, ")")),
      ),
      tags$p(
        style = "font-size: 1rem; color: #dee2e6; margin-bottom: 0;",
        tags$strong("Best defender buy: "),
        tags$span(style = "color: #3498db; font-weight: 700;",
                  paste0(def_best, " (", def_val, ")")),
      )
    )
  })
}

# ── Launch ───────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server, enableBookmarking = "url")
