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

# ── Greek alphabet (capitals) used as scenario tab labels ─────────────────
# Cap the number of scenarios to length(GREEK_LETTERS) = 24.
GREEK_LETTERS <- c(
  "\u0391", "\u0392", "\u0393", "\u0394", "\u0395", "\u0396",  # Α Β Γ Δ Ε Ζ
  "\u0397", "\u0398", "\u0399", "\u039A", "\u039B", "\u039C",  # Η Θ Ι Κ Λ Μ
  "\u039D", "\u039E", "\u039F", "\u03A0", "\u03A1", "\u03A3",  # Ν Ξ Ο Π Ρ Σ
  "\u03A4", "\u03A5", "\u03A6", "\u03A7", "\u03A8", "\u03A9"   # Τ Υ Φ Χ Ψ Ω
)
MAX_SCENARIOS <- length(GREEK_LETTERS)  # = 24

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

safe_int <- function(val, default = 0L, min_val = 0L) {
  # Valid only for plain whole-number decimal strings: "3" yes, "3.9"/"1e3"/"100`" no.
  if (is.null(val) || length(val) != 1L) return(list(value = default, valid = FALSE))
  n <- suppressWarnings(as.integer(val))
  valid <- !is.na(n) && identical(as.character(n), trimws(as.character(val))) && n >= min_val
  list(value = if (valid) n else default, valid = valid)
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

# ── Scenario module: one self-contained battle scenario ───────────────────

scenarioUI <- function(id, letter = NULL) {
  ns <- NS(id)
  # Sidebar layout per scenario: left = inputs, right = outputs.
  # We use layout_sidebar so each tab has its own sidebar.
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = "Battle Setup",

      # Attacker inputs
      card(
        card_header(class = "bg-danger text-white",
                    span("\u2694\uFE0F Attackers")),
        card_body(
          class = "pt-2 pb-1",
          div(class = "dice-row",
            unit_input(ns("atk_d6"),  "d6",  2),
            unit_input(ns("atk_d12"), "d12", 2),
            unit_input(ns("atk_d20"), "d20", 2)
          )
        )
      ),

      # Defender inputs
      card(
        card_header(class = "bg-primary text-white",
                    span("\uD83D\uDEE1\uFE0F Defenders")),
        card_body(
          class = "pt-2 pb-1",
          numericInput(ns("def_castle"), "\uD83C\uDFF0 Castles",
                       value = 0, min = 0, step = 1),
          div(class = "dice-row",
            unit_input(ns("def_d6"),  "d6",  2),
            unit_input(ns("def_d12"), "d12", 2),
            unit_input(ns("def_d20"), "d20", 2)
          )
        )
      ),

      numericInput(ns("sims"), "Simulations",
                   value = 1000, min = 1, step = 100),
      actionButton(ns("run"), "\u25B6 Run Simulation",
                   class = "btn-success btn-lg w-100 mt-2")
    ),

      # Main panel
      layout_columns(
        col_widths = breakpoints(md = c(5, 7), xs = c(12, 12)),
        card(
          card_header(
            span(
              class = "scenario-letter",
              style = "color: #adb5bd; font-weight: 700; margin-right: 8px;",
              textOutput(ns("scenario_letter"), inline = TRUE)
            ),
            "Attacker Win Probability"
          ),
          card_body(plotlyOutput(ns("win_gauge"), height = "250px"))
        ),
        layout_columns(
          col_widths = breakpoints(sm = c(4, 4, 4),
                                    xs = c(12, 12, 12)),
          value_box(
            title    = "Attacker Win %",
            value    = textOutput(ns("atk_pct"), inline = TRUE),
            showcase = span(style = "font-size: 2.5rem;", "\u2694\uFE0F"),
            theme    = "danger"
          ),
          value_box(
            title    = "Defender Win %",
            value    = textOutput(ns("def_pct"), inline = TRUE),
            showcase = span(style = "font-size: 2.5rem;", "\uD83D\uDEE1\uFE0F"),
            theme    = "primary"
          ),
          value_box(
            title    = "Simulations Run",
            value    = textOutput(ns("n_sims_display"), inline = TRUE),
            showcase = span(style = "font-size: 2.5rem;", "\uD83C\uDFB2"),
            theme    = "secondary"
          )
        )
      ),

      layout_columns(
        col_widths = breakpoints(md = c(5, 7), xs = c(12, 12)),
        card(
          card_header("Outcome Distribution"),
          card_body(plotOutput(ns("dist_plot"), height = "400px"))
        ),
        card(
          card_header("Marginal Unit Value"),
          card_body(
            p(class = "text-muted small",
              "Win-rate change from adding +1 of each unit type to the current setup."),
            layout_columns(
              col_widths = breakpoints(md = c(6, 6), xs = c(12, 12)),
              card(
                card_header(class = "bg-danger text-white",
                            "\u2694\uFE0F Attacker +1"),
                card_body(uiOutput(ns("atk_marginal")))
              ),
              card(
                card_header(class = "bg-primary text-white",
                            "\uD83D\uDEE1\uFE0F Defender +1"),
                card_body(uiOutput(ns("def_marginal")))
              )
            ),
            hr(),
            uiOutput(ns("recommendation"))
          )
        )
      )
  )
}

scenarioServer <- function(id, letter = NULL) {
  moduleServer(id, function(input, output, session) {

    output$scenario_letter <- renderText({
      if (is.null(letter)) "" else letter
    })

    raw_results <- reactive({
      atk_d6     <- safe_int(input$atk_d6)
      atk_d12    <- safe_int(input$atk_d12)
      atk_d20    <- safe_int(input$atk_d20)
      def_castle <- safe_int(input$def_castle)
      def_d6     <- safe_int(input$def_d6)
      def_d12    <- safe_int(input$def_d12)
      def_d20    <- safe_int(input$def_d20)
      sims       <- safe_int(input$sims, default = 1000L, min_val = 1L)

      all_valid <- all(
        atk_d6$valid, atk_d12$valid, atk_d20$valid,
        def_castle$valid, def_d6$valid, def_d12$valid, def_d20$valid,
        sims$valid
      )
      validate(need(all_valid, "Attacker dice, defender castles/dice, and Simulations must be whole numbers. Unit counts must be ≥ 0; Simulations must be ≥ 1."))

      init_units <- c(atk_d6$value, atk_d12$value, atk_d20$value,
                      def_castle$value, def_d6$value, def_d12$value, def_d20$value)
      run_simulations(init_units, sims$value)
    }) %>% bindCache(input$atk_d6, input$atk_d12, input$atk_d20,
                     input$def_castle, input$def_d6,
                     input$def_d12, input$def_d20,
                     input$sims)

    # ── Win probability gauge ──
    output$win_gauge <- renderPlotly({
      req(raw_results())
      win_pct <- mean(raw_results()$winner == "Attacker") * 100
      plot_ly(
        type = "indicator", mode = "gauge+number", value = win_pct,
        number = list(suffix = "%", font = list(size = 36, color = "#dee2e6")),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 2,
                      tickcolor = "#adb5bd",
                      tickfont = list(color = "#adb5bd"), dtick = 25),
          bar = list(color = "#e74c3c", thickness = 0.7),
          bgcolor = "#3a3f44", borderwidth = 0,
          steps = list(
            list(range = c(0, 25),  color = "#2c3e50"),
            list(range = c(25, 50), color = "#34495e"),
            list(range = c(50, 75), color = "#34495e"),
            list(range = c(75, 100), color = "#2c3e50")
          ),
          threshold = list(
            line = list(color = "#2ecc71", width = 3),
            thickness = 0.8, value = 50
          )
        )
      ) %>%
        layout(paper_bgcolor = "rgba(0,0,0,0)",
               plot_bgcolor  = "rgba(0,0,0,0)",
               margin = list(t = 40, b = 20, l = 30, r = 30),
               font = list(color = "#dee2e6")) %>%
        config(displayModeBar = FALSE)
    })

    # ── Value boxes ──
    output$atk_pct <- renderText({
      req(raw_results())
      sprintf("%.1f%%", mean(raw_results()$winner == "Attacker") * 100)
    })
    output$def_pct <- renderText({
      req(raw_results())
      sprintf("%.1f%%", mean(raw_results()$winner == "Defender") * 100)
    })
    output$n_sims_display <- renderText({
      req(raw_results())
      format(nrow(raw_results()), big.mark = ",")
    })

    # ── Distribution chart ──
    output$dist_plot <- renderPlot({
      req(raw_results())
      df <- raw_results()
      df$outcome <- ifelse(
        df$winner == "Attacker",
        df$atk_d6 + df$atk_d12 + df$atk_d20,
        -(df$def_d6 + df$def_d12 + df$def_d20 + df$def_castle)
      )
      plot_data <- data.frame(outcome = df$outcome, winner = df$winner)
      max_abs <- max(abs(plot_data$outcome), 1)

      ggplot(plot_data, aes(x = outcome, fill = winner)) +
        geom_histogram(binwidth = 1, alpha = 0.85,
                       color = "white", linewidth = 0.3) +
        scale_fill_manual(values = c("Attacker" = "#e74c3c",
                                     "Defender" = "#3498db")) +
        scale_x_continuous(limits = c(-max_abs - 0.5, max_abs + 0.5),
                           labels = function(x) abs(x)) +
        geom_vline(xintercept = 0, color = "#adb5bd",
                   linewidth = 0.6, linetype = "dashed") +
        annotate("text", x = -max_abs * 0.5, y = Inf,
                 label = "\u2190 Defender wins",
                 color = "#3498db", vjust = 2, size = 4.5,
                 fontface = "bold") +
        annotate("text", x = max_abs * 0.5, y = Inf,
                 label = "Attacker wins \u2192",
                 color = "#e74c3c", vjust = 2, size = 4.5,
                 fontface = "bold") +
        labs(x = "Surviving Units", y = "Frequency", fill = NULL) +
        theme_minimal(base_size = 14) +
        theme(plot.background  = element_rect(fill = "#272b30", color = NA),
              panel.background = element_rect(fill = "#272b30", color = NA),
              panel.grid.major = element_line(color = "#3a3f44"),
              panel.grid.minor = element_blank(),
              text             = element_text(color = "#dee2e6"),
              axis.text        = element_text(color = "#adb5bd"),
              legend.position  = "none")
    })

    # ── Marginal benefit analysis ──
    base_win_rate <- reactive({
      req(raw_results())
      mean(raw_results()$winner == "Attacker")
    })

    marginal_win_rate <- function(init_units, n_sims) {
      res <- run_simulations(init_units, n_sims)
      mean(res$winner == "Attacker")
    }

    unit_cost <- c(d6 = 1, d12 = 2, d20 = 2)

    marginal_data <- reactive({
      req(raw_results())
      base <- base_win_rate()
      init <- c(input$atk_d6, input$atk_d12, input$atk_d20,
                input$def_castle, input$def_d6,
                input$def_d12, input$def_d20)
      n <- input$sims

      atk_d6_rate  <- marginal_win_rate(init + c(1,0,0, 0,0,0,0), n)
      atk_d12_rate <- marginal_win_rate(init + c(0,1,0, 0,0,0,0), n)
      atk_d20_rate <- marginal_win_rate(init + c(0,0,1, 0,0,0,0), n)
      def_d6_rate  <- marginal_win_rate(init + c(0,0,0, 0,1,0,0), n)
      def_d12_rate <- marginal_win_rate(init + c(0,0,0, 0,0,1,0), n)
      def_d20_rate <- marginal_win_rate(init + c(0,0,0, 0,0,0,1), n)

      atk_raw <- c(d6 = atk_d6_rate - base,
                   d12 = atk_d12_rate - base,
                   d20 = atk_d20_rate - base)
      def_raw <- c(d6 = -(def_d6_rate - base),
                   d12 = -(def_d12_rate - base),
                   d20 = -(def_d20_rate - base))
      atk_eff <- atk_raw / unit_cost[names(atk_raw)]
      def_eff <- def_raw / unit_cost[names(def_raw)]
      list(base = base, atk = atk_raw, def = def_raw,
           atk_eff = atk_eff, def_eff = def_eff)
    }) %>% bindCache(input$atk_d6, input$atk_d12, input$atk_d20,
                     input$def_castle, input$def_d6,
                     input$def_d12, input$def_d20,
                     input$sims)

    marginal_badge <- function(label, delta, efficiency, cost, color) {
      sign_char <- ifelse(delta >= 0, "+", "")
      pct_text  <- sprintf("%s%.1f%%", sign_char, delta * 100)
      eff_text  <- sprintf("%.1f%% per cost", efficiency * 100)
      bar_width <- min(abs(delta) * 100 / 0.5 * 100, 100)
      tags$div(
        style = "margin-bottom: 12px;",
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 2px;",
          tags$span(style = "font-weight: 600; color: #dee2e6; font-size: 1.05rem;", label),
          tags$span(style = paste0("font-weight: 700; font-size: 1.15rem; color:", color, ";"),
                    pct_text)
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
            "width:", bar_width, "%; height: 100%; background:", color,
            "; border-radius: 4px;"
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
        marginal_badge(paste0("+1 d6",  if (best == "d6")  " \u2B50" else ""),
                       m["d6"],  eff["d6"],  unit_cost["d6"],  "#e74c3c"),
        marginal_badge(paste0("+1 d12", if (best == "d12") " \u2B50" else ""),
                       m["d12"], eff["d12"], unit_cost["d12"], "#e74c3c"),
        marginal_badge(paste0("+1 d20", if (best == "d20") " \u2B50" else ""),
                       m["d20"], eff["d20"], unit_cost["d20"], "#e74c3c")
      )
    })

    output$def_marginal <- renderUI({
      req(marginal_data())
      m   <- marginal_data()$def
      eff <- marginal_data()$def_eff
      best <- names(which.max(eff))
      tagList(
        marginal_badge(paste0("+1 d6",  if (best == "d6")  " \u2B50" else ""),
                       m["d6"],  eff["d6"],  unit_cost["d6"],  "#3498db"),
        marginal_badge(paste0("+1 d12", if (best == "d12") " \u2B50" else ""),
                       m["d12"], eff["d12"], unit_cost["d12"], "#3498db"),
        marginal_badge(paste0("+1 d20", if (best == "d20") " \u2B50" else ""),
                       m["d20"], eff["d20"], unit_cost["d20"], "#3498db")
      )
    })

    output$recommendation <- renderUI({
      req(marginal_data())
      m <- marginal_data()
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
                    paste0(atk_best, " (", atk_val, ")"))
        ),
        tags$p(
          style = "font-size: 1rem; color: #dee2e6; margin-bottom: 0;",
          tags$strong("Best defender buy: "),
          tags$span(style = "color: #3498db; font-weight: 700;",
                    paste0(def_best, " (", def_val, ")"))
        )
      )
    })

  })
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

ui <- function(request) page_fluid(
  theme = app_theme,
  title = "d1066 Battle Simulator",

  # Heartbeat + custom disconnect overlay (unchanged)
  tags$head(
    tags$meta(name = "viewport",
              content = "width=device-width, initial-scale=1, shrink-to-fit=no"),
    tags$style(HTML("
      /* ── Reset: strip page_fluid container padding so we can go edge-to-edge */
      html, body { overflow-x: hidden; }
      body > .container-fluid {
        padding-left: 0 !important;
        padding-right: 0 !important;
        max-width: 100% !important;
        overflow-x: hidden;
      }

      /* ── Title bar ──────────────────────────────────────────────────────── */
      .app-title-bar {
        display: flex;
        align-items: center;
        gap: 10px;
        padding: 8px 14px;
        border-bottom: 1px solid #3a3f44;
      }

      /* ── Fixed header: title bar + tab strip bar pinned to top of viewport ─ */
      /* Using position:fixed (not sticky) because navset_tab renders           */
      /* tab-content as a sibling of nav-tabs inside the same container, and    */
      /* overflow:hidden on a sticky ancestor would clip it. Fixed + a spacer   */
      /* div below the header is the only reliable approach.                    */
      .app-sticky-header {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        width: 100%;
        z-index: 1030;
        background: #272b30;
        box-shadow: 0 2px 6px rgba(0,0,0,0.5);
      }

      /* Spacer div pushes content below the fixed header.                      */
      /* JS sets its height to match .app-sticky-header's actual rendered height */
      #header-spacer {
        height: 89px;   /* title bar ~41px + tab bar 48px; JS will correct this */
      }

      /* ── Control bar row ────────────────────────────────────────────────── */
      .scenario-tabs-row {
        display: flex;
        align-items: flex-end;
        padding: 0;
        background: #272b30;
        border-bottom: 2px solid #3a3f44;
      }

      /* ── Blue action buttons ─────────────────────────────────────────────── */
      .tabs-row-btn {
        flex: 0 0 48px;
        width: 48px;
        height: 48px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        padding: 0 !important;
        margin: 0 !important;
        background: #3498db !important;
        color: #ffffff !important;
        border: 0 !important;
        border-radius: 0 !important;
        font-size: 1.5rem;
        font-weight: 700;
        line-height: 1;
        cursor: pointer;
        box-shadow: none !important;
        transition: background 0.12s;
        -webkit-tap-highlight-color: transparent;
      }
      .tabs-row-btn:hover  { background: #2980b9 !important; }
      .tabs-row-btn:active { background: #2471a3 !important; }
      .tabs-row-btn:focus  { outline: none; color: #fff !important; }
      .tabs-row-btn:focus-visible { outline: 2px solid #85c1e9; outline-offset: -2px; }
      .tabs-row-btn svg { width: 22px; height: 22px; fill: currentColor; display: block; }

      /* ── Tab strip wrapper: fills space between buttons ─────────────────── */
      .scenario-tabs-wrapper {
        flex: 1 1 0;
        min-width: 0;
        overflow: hidden;      /* clip tab-content inside wrapper; it still       */
        height: 48px;          /* renders in the DOM and Shiny can reach it;      */
      }                        /* only visually clipped inside the fixed header.  */

      /* ── nav-tabs ul ─────────────────────────────────────────────────────── */
      #scenario_tabs.nav-tabs {
        display: flex !important;
        flex-wrap: nowrap;
        overflow-x: auto;
        overflow-y: hidden;
        -webkit-overflow-scrolling: touch;
        scrollbar-width: none;
        border-bottom: 0 !important;
        margin: 0;
        padding: 0;
        height: 48px;
        align-items: flex-end;
        list-style: none;
      }
      #scenario_tabs.nav-tabs::-webkit-scrollbar { display: none; }
      #scenario_tabs.nav-tabs > li {
        display: flex;
        align-items: flex-end;
        flex-shrink: 0;
        margin: 0;
      }
      #scenario_tabs.nav-tabs > li > a {
        display: flex;
        align-items: center;
        justify-content: center;
        height: 42px;
        min-width: 78px;
        padding: 0 14px;
        font-size: 1.1rem;
        font-weight: 700;
        white-space: nowrap;
        border-radius: 6px 6px 0 0;
        border: 1px solid transparent;
        border-bottom: 0 !important;
        color: #7f8c8d;
        text-decoration: none;
        transition: color 0.12s, background 0.12s;
        -webkit-tap-highlight-color: transparent;
      }
      #scenario_tabs.nav-tabs > li.active > a,
      #scenario_tabs.nav-tabs > li > a.active {
        color: #ecf0f1;
        background: #32383e;
        border-color: #4a5056 #4a5056 #32383e !important;
      }
      #scenario_tabs.nav-tabs > li > a:hover:not(.active) {
        color: #bdc3c7;
        background: #2c3136;
      }
      #scenario_tabs.nav-tabs > li > a[data-value='__add_tab__'] {
        color: #2ecc71;
        font-size: 1.5rem;
        min-width: 48px;
      }
      #scenario_tabs.nav-tabs > li > a[data-value='__add_tab__']:hover {
        color: #27ae60;
        background: #2c3136;
      }

      /* ── tab-content and scenario content: no container framing ─────────── */
      .tab-content {
        border: 0 !important;
        background: transparent !important;
        padding: 0 !important;
      }
      .tab-content > .tab-pane {
        border: 0 !important;
        background: transparent !important;
        padding: 0 !important;
      }
      .bslib-sidebar-layout {
        border: 0 !important;
        background: transparent !important;
        box-shadow: none !important;
      }
      .bslib-sidebar-layout > .main { background: transparent !important; }
      .bslib-sidebar-layout > .collapse-toggle {
        visibility: hidden !important;
        width: 0 !important;
        padding: 0 !important;
        margin: 0 !important;
        pointer-events: none !important;
      }

      /* ── Dice input row: always 3 equal columns inside sidebar ─────────── */
      .dice-row {
        display: flex;
        gap: 8px;
      }
      .dice-row > div {
        flex: 1 1 0;
        min-width: 0;
      }

      /* ── Mobile ──────────────────────────────────────────────────────────── */
      @media (max-width: 575.98px) {
        .tabs-row-btn { flex: 0 0 44px; width: 44px; height: 44px; font-size: 1.3rem; }
        .tabs-row-btn svg { width: 20px; height: 20px; }
        .scenario-tabs-wrapper { height: 44px; }
        #scenario_tabs.nav-tabs { height: 44px; }
        #scenario_tabs.nav-tabs > li > a { height: 38px; min-width: 66px; padding: 0 10px; font-size: 1rem; }
        .app-title-bar { padding: 6px 12px; }
        #header-spacer { height: 85px; }

        /* Sidebar on mobile: bslib absolutely-positions the aside panel,
           but the grid row still reserves its height as a gap.
           Collapse that reserved space so only .main sets the height. */
        .bslib-sidebar-layout {
          --_sidebar-width: 90vw !important;
        }
        .bslib-sidebar-layout > aside {
          overflow-y: auto !important;
          max-height: calc(100dvh - 85px) !important;
        }
        /* When sidebar is OPEN on mobile the aside is position:absolute (out of
           flow), so the grid cell it occupied becomes a phantom gap. Span .main
           across the full grid to collapse that gap. Only do this when open —
           bslib adds .sidebar-collapsed when closed, removes it when open. */
        .bslib-sidebar-layout:not(.sidebar-collapsed) > .main {
          grid-row: 1 !important;
          grid-column: 1 / -1 !important;
        }
      }
    ")),
    tags$script(HTML("
      // Move tab-content out of the clipped fixed header into normal document
      // flow so it scrolls freely below the fixed bar.
      // Runs after Shiny is fully initialised (safe on mobile browsers).
      function hoistTabContent() {
        var spacer     = document.getElementById('header-spacer');
        var header     = document.querySelector('.app-sticky-header');
        var tabContent = document.querySelector('.scenario-tabs-wrapper .tab-content');
        if (tabContent && spacer && !spacer._hoisted) {
          spacer.parentNode.insertBefore(tabContent, spacer.nextSibling);
          spacer._hoisted = true;
        }
        // Always sync spacer height to actual fixed header height
        if (header && spacer) {
          spacer.style.height = header.offsetHeight + 'px';
        }
      }
      // Fire immediately, on Shiny ready, and on resize
      document.addEventListener('DOMContentLoaded', hoistTabContent);
      $(document).on('shiny:sessioninitialized', hoistTabContent);
      window.addEventListener('resize', hoistTabContent);

      Shiny.addCustomMessageHandler('scroll_active_tab', function(_) {
        setTimeout(function() {
          var el = document.querySelector('#scenario_tabs li.active > a, #scenario_tabs .nav-link.active');
          if (el && el.scrollIntoView) {
            el.scrollIntoView({behavior: 'smooth', inline: 'center', block: 'nearest'});
          }
        }, 50);
      });
      // Global sidebar toggle: forward clicks to the active tab's hidden bslib toggle
      document.addEventListener('click', function(e) {
        var btn = e.target.closest('#global_sidebar_toggle');
        if (!btn) return;
        e.preventDefault();
        var activePane = document.querySelector('.tab-content > .tab-pane.active');
        if (!activePane) return;
        var t = activePane.querySelector('.bslib-sidebar-layout > .collapse-toggle');
        if (t) t.click();
      });
      setInterval(function() {
        Shiny.setInputValue('heartbeat', new Date().getTime());
      }, 30000);
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

  # Sticky header: title bar + control bar never scroll away
  div(
    class = "app-sticky-header",

    div(
      class = "app-title-bar",
      span(class = "app-title-icon", style = "font-size: 1.4rem;", "\u2694\uFE0F"),
      span(class = "app-title-text",
           style = "font-size: 1.15rem; font-weight: 600;",
           "d1066 Battle Simulator")
    ),

    # Control bar: [sidebar toggle] [scrollable tab strip] [close]
    div(
      class = "scenario-tabs-row",
      tags$button(
        id = "global_sidebar_toggle",
        class = "tabs-row-btn sidebar-toggle-btn",
        type = "button",
        `aria-label` = "Toggle Battle Setup sidebar",
        HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" aria-hidden="true">
                <path d="M14 2a1 1 0 0 1 1 1v10a1 1 0 0 1-1 1H2a1 1 0 0 1-1-1V3a1 1 0 0 1 1-1zM2 1a2 2 0 0 0-2 2v10a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V3a2 2 0 0 0-2-2z"/>
                <path d="M3 4.5a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v7a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5z"/>
              </svg>')
      ),
      div(
        class = "scenario-tabs-wrapper",
        navset_tab(
          id = "scenario_tabs",
          nav_panel(
            title = GREEK_LETTERS[1],
            value = "scenario_alpha",
            scenarioUI("scenario_alpha", letter = GREEK_LETTERS[1])
          ),
          nav_panel(
            title = "+",
            value = "__add_tab__"
          )
        )
      ),
      actionButton(
        "global_close_scenario",
        label = HTML("&times;"),
        class = "tabs-row-btn close-scenario-btn",
        title = "Close active scenario"
      )
    )
  ),

  # Spacer: pushes page content below the fixed header.
  # JS measures the actual header height and sets this dynamically.
  tags$div(id = "header-spacer")
)

# ── Server ───────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  session$allowReconnect("force")

  # ── Track currently-open scenarios (stable internal id + display letter) ──
  # We always keep the original alpha scenario (started in UI).
  scenarios <- reactiveVal(
    data.frame(id = "scenario_alpha", letter = GREEK_LETTERS[1],
               stringsAsFactors = FALSE)
  )
  next_scenario_seq <- reactiveVal(1L)

  next_scenario_id <- function() {
    next_seq <- next_scenario_seq() + 1L
    next_scenario_seq(next_seq)
    sprintf("scenario_%04d", next_seq)
  }

  mount_scenario <- function(id, letter) {
    scenarioServer(id, letter = letter)
  }

  # Boot up the alpha scenario's server logic.
  mount_scenario("scenario_alpha", GREEK_LETTERS[1])

  # Global close: removes whichever scenario is currently active.
  observeEvent(input$global_close_scenario, {
    current <- scenarios()
    active  <- input$scenario_tabs
    if (is.null(active) || identical(active, "__add_tab__")) return()
    if (nrow(current) <= 1) {
      showNotification("Keep at least one scenario open.",
                       type = "warning", duration = 3)
      return()
    }
    remaining <- current[current$id != active, , drop = FALSE]
    if (nrow(remaining) == nrow(current)) return()

    nav_remove("scenario_tabs", target = active, session = session)
    scenarios(remaining)
    nav_select("scenario_tabs",
               selected = remaining$id[nrow(remaining)],
               session  = session)
  }, ignoreInit = TRUE)

  # ── Handle clicks on the "+" tab to add a new scenario ──
  observeEvent(input$scenario_tabs, {
    if (!identical(input$scenario_tabs, "__add_tab__")) return()

    current <- scenarios()
    available_letters <- base::setdiff(GREEK_LETTERS, current$letter)
    if (length(available_letters) == 0) {
      # Bounce the user off the "+" tab back to the last real scenario,
      # and tell them why nothing happened.
      nav_select("scenario_tabs",
                 selected = current$id[nrow(current)],
                 session  = session)
      showNotification(
        sprintf("Tab limit reached (%d scenarios - Greek alphabet exhausted).",
                MAX_SCENARIOS),
        type = "warning",
        duration = 4
      )
      return()
    }

    new_id    <- next_scenario_id()
    new_label <- available_letters[1]

    # Insert the new scenario tab BEFORE the "+" tab.
    nav_insert(
      id       = "scenario_tabs",
      target   = "__add_tab__",
      position = "before",
      nav      = nav_panel(
        title = new_label,
        value = new_id,
        scenarioUI(new_id, letter = new_label)
      ),
      session  = session
    )

    # Boot up the new scenario's server logic.
    mount_scenario(new_id, new_label)

    # Track it in our reactive list.
    scenarios(rbind(
      current,
      data.frame(id = new_id, letter = new_label, stringsAsFactors = FALSE)
    ))

    # Switch the user to the brand-new tab (don't leave them on "+").
    nav_select("scenario_tabs", selected = new_id, session = session)

    # Scroll the newly-active tab into view (helps on mobile when tabs overflow).
    session$sendCustomMessage("scroll_active_tab", list())
  }, ignoreInit = TRUE)

  # ── Bookmarking: keep current behavior (URL captures all inputs) ──
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)
}

# ── Launch ───────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server, enableBookmarking = "url")
