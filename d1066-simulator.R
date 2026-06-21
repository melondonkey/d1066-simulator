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

# ── Circled digits used as scenario tab labels ────────────────────────────
SCENARIO_NUMERALS <- c(
  "\u2460", "\u2461", "\u2462", "\u2463", "\u2464",
  "\u2465", "\u2466", "\u2467", "\u2468"
)
MAX_SCENARIOS <- length(SCENARIO_NUMERALS)  # = 9

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

# Server-side validation is the single source of truth for numeric input.
# Browser numeric inputs may allow scientific notation (e.g. "7e6"), and
# JS key filters would not cover pasted, restored, bookmarked, or query-string
# values. Keep this strict guard at the server consumption point.
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

scenarioUI <- function(id, label = NULL) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      id = ns("battle_setup"),
      width = 320,
      open = list(desktop = "open", mobile = "closed"),
      class = "scenario-sidebar",

      section_label("ATTACKERS"),
      div(class = "dice-row",
        unit_input(ns("atk_d6"),  "d6",  2),
        unit_input(ns("atk_d12"), "d12", 2),
        unit_input(ns("atk_d20"), "d20", 2)
      ),

      section_label("DEFENDERS"),
      numericInput(ns("def_castle"), "Castles",
                   value = 0, min = 0, step = 1),
      div(class = "dice-row",
        unit_input(ns("def_d6"),  "d6",  2),
        unit_input(ns("def_d12"), "d12", 2),
        unit_input(ns("def_d20"), "d20", 2)
      ),

      section_label("SIMULATION"),
      numericInput(ns("sims"), "Simulations",
                   value = 1000, min = 1, step = 100),
      input_task_button(ns("run"), "Run Simulation",
                        label_busy = "Simulating…",
                        class = "btn-run w-100 mt-2")
    ),

    div(class = "content-section probability-section",
      section_label("WIN PROBABILITY"),
      plotlyOutput(ns("win_gauge"), height = "220px"),
      uiOutput(ns("win_matchup")),
      div(class = "sr-only", role = "status", `aria-live` = "polite",
          textOutput(ns("win_summary")))
    ),

    layout_columns(
      col_widths = breakpoints(lg = c(7, 5), xs = c(12, 12)),
      div(class = "content-section",
        section_label("OUTCOME DISTRIBUTION"),
        plotOutput(ns("dist_plot"), height = "380px"),
        div(class = "sr-only", `aria-live` = "polite", textOutput(ns("dist_summary")))
      ),
      div(class = "content-section",
        section_label("MARGINAL UNIT VALUE"),
        p(class = "dashboard-note",
          "Win-rate change from adding +1 of each unit type to the current setup."),
        layout_columns(
          col_widths = breakpoints(md = c(6, 6), xs = c(12, 12)),
          div(
            section_label("ATTACKER +1", level = "h3"),
            uiOutput(ns("atk_marginal"))
          ),
          div(
            section_label("DEFENDER +1", level = "h3"),
            uiOutput(ns("def_marginal"))
          )
        ),
        hr(),
        uiOutput(ns("recommendation"))
      )
    )
  )
}

scenarioServer <- function(id, label = NULL) {
  moduleServer(id, function(input, output, session) {

    output$scenario_letter <- renderText({
      if (is.null(label)) "" else label
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
    }) %>%
      bindCache(input$atk_d6, input$atk_d12, input$atk_d20,
                input$def_castle, input$def_d6,
                input$def_d12, input$def_d20,
                input$sims) %>%
      bindEvent(input$run, ignoreNULL = FALSE)

    observeEvent(input$run, {
      toggle_sidebar("battle_setup", open = FALSE, session = session)
    }, ignoreInit = TRUE)

    # ── Win probability gauge (radial meter) ──
    output$win_gauge <- renderPlotly({
      req(raw_results())
      win_pct <- mean(raw_results()$winner == "Attacker") * 100
      plot_ly(
        type = "indicator", mode = "gauge+number", value = win_pct,
        number = list(suffix = "%", font = list(size = 36, color = "#c8cdc9")),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 2,
                      tickcolor = "#8a9490",
                      tickfont = list(color = "#8a9490"), dtick = 25),
          # Value arc fills the full ring thickness so it sits flush on the
          # base track and the two read as one continuous meter.
          bar = list(color = "#a02020", thickness = 1.0,
                     line = list(color = "#6e1616", width = 1)),
          # Opaque charcoal base track, lifted above the panel (#1a1e21) with a
          # subtle border defining both the inner and outer curves. A single
          # full-range step guarantees the unfilled portion stays visible even
          # at very low probabilities.
          bgcolor = "#333c41",
          bordercolor = "#475157", borderwidth = 1,
          steps = list(
            list(range = c(0, 100), color = "#333c41")
          ),
          threshold = list(
            line = list(color = "#3d7a52", width = 3),
            thickness = 1.0, value = 50
          )
        )
      ) %>%
        layout(paper_bgcolor = "rgba(0,0,0,0)",
               plot_bgcolor  = "rgba(0,0,0,0)",
               margin = list(t = 40, b = 20, l = 30, r = 30),
               font = list(color = "#c8cdc9")) %>%
        config(displayModeBar = FALSE)
    })

    # ── Comparative matchup readout (pairs with the gauge) ──
    # Leads with the result — favored side, margin, and simulation context —
    # so the matchup reads as a balanced attacker-vs-defender comparison.
    output$win_matchup <- renderUI({
      req(raw_results())
      df  <- raw_results()
      n   <- nrow(df)
      atk <- mean(df$winner == "Attacker") * 100
      def <- 100 - atk
      margin <- abs(atk - def)

      # Within ~2 points we call it even — a meaningful neutral state.
      fav <- if (margin < 2) "even" else if (atk > def) "atk" else "def"
      verdict <- switch(fav,
        atk  = sprintf("Attacker favored by %.0f points", margin),
        def  = sprintf("Defender favored by %.0f points", margin),
        even = "Even matchup"
      )
      tags$div(
        class = paste0("matchup fav-", fav),
        # Favored-side verdict bridges the meter to the numbers below it.
        tags$div(class = "verdict-chip", verdict),
        tags$div(
          class = "matchup-readout",
          tags$div(class = "mr-side mr-atk",
            tags$div(class = "mr-pct", sprintf("%.1f%%", atk)),
            tags$div(class = "mr-team", "Attacker")
          ),
          tags$div(class = "mr-side mr-sims",
            tags$div(class = "mr-pct", format(n, big.mark = ",")),
            tags$div(class = "mr-team", "Simulations")
          ),
          tags$div(class = "mr-side mr-def",
            tags$div(class = "mr-pct", sprintf("%.1f%%", def)),
            tags$div(class = "mr-team", "Defender")
          )
        )
      )
    })

    # Screen-reader summary of the gauge (plotly itself is not accessible).
    output$win_summary <- renderText({
      req(raw_results())
      win_pct <- mean(raw_results()$winner == "Attacker") * 100
      sprintf("Attacker win probability %.1f percent; defender %.1f percent.",
              win_pct, 100 - win_pct)
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
        scale_fill_manual(values = c("Attacker" = "#a02020",
                                     "Defender" = "#2a6080")) +
        scale_x_continuous(limits = c(-max_abs - 0.5, max_abs + 0.5),
                           labels = function(x) abs(x)) +
        # Add headroom above the tallest bar so the side labels sit clear of it.
        scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
        geom_vline(xintercept = 0, color = "#8a9490",
                   linewidth = 0.6, linetype = "dashed") +
        annotate("text", x = -max_abs * 0.5, y = Inf,
                 label = "\u2190 Defender wins",
                 color = "#2a6080", vjust = 1.3, size = 4.5,
                 fontface = "bold") +
        annotate("text", x = max_abs * 0.5, y = Inf,
                 label = "Attacker wins \u2192",
                 color = "#a02020", vjust = 1.3, size = 4.5,
                 fontface = "bold") +
        labs(x = "Surviving Units", y = "Frequency", fill = NULL) +
        theme_minimal(base_size = 14) +
        theme(plot.background  = element_rect(fill = "#1a1e21", color = NA),
              panel.background = element_rect(fill = "#1a1e21", color = NA),
              panel.grid.major = element_line(color = "#252b2e"),
              panel.grid.minor = element_blank(),
              text             = element_text(color = "#c8cdc9"),
              axis.text        = element_text(color = "#8a9490"),
              legend.position  = "none")
    })

    # Screen-reader text summary of the distribution chart.
    output$dist_summary <- renderText({
      req(raw_results())
      df <- raw_results()
      n  <- nrow(df)
      atk_wins <- sum(df$winner == "Attacker")
      def_wins <- n - atk_wins
      sprintf(
        paste0("Outcome distribution across %s simulations. ",
               "Attacker won %s (%.1f%%), defender won %s (%.1f%%). ",
               "Bars right of center show surviving attacker units; bars left show surviving defender units."),
        format(n, big.mark = ","),
        format(atk_wins, big.mark = ","), 100 * atk_wins / n,
        format(def_wins, big.mark = ","), 100 * def_wins / n
      )
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
    }) %>%
      bindCache(input$atk_d6, input$atk_d12, input$atk_d20,
                input$def_castle, input$def_d6,
                input$def_d12, input$def_d20,
                input$sims) %>%
      bindEvent(input$run, ignoreNULL = FALSE)

    marginal_badge <- function(label, delta, efficiency, cost, fill, text_color) {
      sign_char <- ifelse(delta >= 0, "+", "")
      pct_text  <- sprintf("%s%.1f%%", sign_char, delta * 100)
      eff_text  <- sprintf("%.1f%% per cost", efficiency * 100)
      bar_width <- min(abs(delta) * 100 / 0.5 * 100, 100)
      tags$div(
        style = "margin-bottom: 12px;",
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 2px;",
          tags$span(style = "font-weight: 600; color: var(--text); font-size: 1.05rem;", label),
          tags$span(style = paste0("font-weight: 700; font-size: 1.15rem; color:", text_color, ";"),
                    pct_text)
        ),
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;",
          tags$span(style = "color: var(--text-muted); font-size: 0.82rem;",
                    paste0("Cost: ", cost)),
          tags$span(style = "color: var(--text-muted); font-size: 0.82rem; font-style: italic;",
                    eff_text)
        ),
        tags$div(
          style = "background: var(--surface); border-radius: 4px; height: 8px; overflow: hidden;",
          tags$div(style = paste0(
            "width:", bar_width, "%; height: 100%; background:", fill,
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
        marginal_badge(paste0("+1 d6",  if (best == "d6")  " \u2190 best" else ""),
                       m["d6"],  eff["d6"],  unit_cost["d6"],  "#a02020", "#e07a6e"),
        marginal_badge(paste0("+1 d12", if (best == "d12") " \u2190 best" else ""),
                       m["d12"], eff["d12"], unit_cost["d12"], "#a02020", "#e07a6e"),
        marginal_badge(paste0("+1 d20", if (best == "d20") " \u2190 best" else ""),
                       m["d20"], eff["d20"], unit_cost["d20"], "#a02020", "#e07a6e")
      )
    })

    output$def_marginal <- renderUI({
      req(marginal_data())
      m   <- marginal_data()$def
      eff <- marginal_data()$def_eff
      best <- names(which.max(eff))
      tagList(
        marginal_badge(paste0("+1 d6",  if (best == "d6")  " \u2190 best" else ""),
                       m["d6"],  eff["d6"],  unit_cost["d6"],  "#2a6080", "#6fa8c7"),
        marginal_badge(paste0("+1 d12", if (best == "d12") " \u2190 best" else ""),
                       m["d12"], eff["d12"], unit_cost["d12"], "#2a6080", "#6fa8c7"),
        marginal_badge(paste0("+1 d20", if (best == "d20") " \u2190 best" else ""),
                       m["d20"], eff["d20"], unit_cost["d20"], "#2a6080", "#6fa8c7")
      )
    })

    output$recommendation <- renderUI({
      req(marginal_data())
      m <- marginal_data()
      atk_best <- names(which.max(m$atk_eff))
      def_best <- names(which.max(m$def_eff))
      meta <- function(gain, cost, eff) {
        sprintf("%+.1f%% win  ·  cost %d  ·  %+.1f%% per cost",
                gain * 100, cost, eff * 100)
      }
      tags$div(
        class = "rec-callouts",
        tags$div(
          class = "rec-card rec-card-atk",
          tags$div(class = "rec-card-title", "Best Attacker Buy"),
          tags$div(class = "rec-card-unit", atk_best),
          tags$div(class = "rec-card-meta",
                   meta(m$atk[[atk_best]], unit_cost[[atk_best]], max(m$atk_eff)))
        ),
        tags$div(
          class = "rec-card rec-card-def",
          tags$div(class = "rec-card-title", "Best Defender Buy"),
          tags$div(class = "rec-card-unit", def_best),
          tags$div(class = "rec-card-meta",
                   meta(m$def[[def_best]], unit_cost[[def_best]], max(m$def_eff)))
        )
      )
    })

  })
}

# ── Theme ────────────────────────────────────────────────────────────────

app_theme <- bs_theme(
  version   = 5,
  bootswatch = "slate",
  primary   = "#a02020",
  secondary = "#2a6080",
  success   = "#3d7a52",
  "font-size-base" = "0.92rem"
)

# ── Styled numeric input helper ──────────────────────────────────────────

unit_input <- function(id, label, value, icon_text = NULL) {
  numericInput(id, label, value = value, min = 0, step = 1)
}

section_label <- function(text, level = "h2") {
  # Renders as a real heading (h2/h3) styled as a divider, for semantic structure.
  tag(level, list(class = "section-divider", tags$span(text)))
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
      :root {
        --header-height: 89px;
        /* ── Surfaces & text ── */
        --bg: #1a1e21;
        --surface: #252b2e;
        --surface-2: #2e3538;
        --border: #2e3538;
        --text: #c8cdc9;
        --text-muted: #8a9490;   /* AA for normal text on --bg */
        /* ── Team colors: deep fills vs AA-compliant accent text ── */
        --atk-fill: #a02020;
        --def-fill: #2a6080;
        --atk-text: #e07a6e;     /* lighter red — AA as text on dark */
        --def-text: #6fa8c7;     /* lighter blue — AA as text on dark */
        --ok: #3d7a52;
        /* ── 8px spacing scale ── */
        --s1: 8px; --s2: 16px; --s3: 24px; --s4: 32px;
        --radius: 8px;
        --shell-max: 1280px;
      }

      .sr-only {
        position: absolute !important; width: 1px; height: 1px;
        padding: 0; margin: -1px; overflow: hidden; clip: rect(0,0,0,0);
        white-space: nowrap; border: 0;
      }

      /* ── Reset: strip page_fluid container padding so we can go edge-to-edge */
      html, body {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        overflow: hidden;
        overflow-x: hidden;
        overscroll-behavior: none;
      }
      body > .container-fluid {
        display: flex;
        flex-direction: column;
        padding-left: 0 !important;
        padding-right: 0 !important;
        max-width: 100% !important;
        width: 100%;
        height: 100%;
        overflow: hidden;
        overflow-x: hidden;
      }

      /* ── Title bar ──────────────────────────────────────────────────────── */
      /* Header bar is full-bleed; its inner content is centered to --shell-max */
      .app-title-bar {
        display: flex;
        align-items: center;
        gap: 10px;
        padding: 8px 14px;
        border-bottom: 1px solid var(--border);
        max-width: var(--shell-max);
        width: 100%;
        margin: 0 auto;
      }
      .app-title-bar .app-title-spacer { flex: 1 1 auto; }
      .app-settings-btn {
        display: inline-flex; align-items: center; justify-content: center;
        width: 36px; height: 36px; padding: 0;
        background: transparent; color: var(--text-muted);
        border: 1px solid var(--border); border-radius: var(--radius);
        cursor: pointer; transition: color 0.12s, border-color 0.12s;
      }
      .app-settings-btn:hover { color: var(--text); border-color: var(--text-muted); }
      .settings-help-list { margin: 0; padding-left: 1.1em; font-size: 0.85rem; }
      .settings-help-list li { margin-bottom: 6px; }

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
        background: #1a1e21;
        box-shadow: 0 2px 6px rgba(0,0,0,0.5);
      }

      /* Spacer div pushes content below the fixed header.                      */
      /* JS sets its height to match .app-sticky-header's actual rendered height */
      #header-spacer {
        flex: 0 0 var(--header-height);
        height: var(--header-height);
      }

      /* ── Control bar row ────────────────────────────────────────────────── */
      .scenario-tabs-row {
        display: flex;
        align-items: flex-end;
        width: 100%;
        max-width: var(--shell-max);
        margin: 0 auto;
        min-width: 0;
        overflow: hidden;
        padding: 0;
        background: var(--bg);
        border-bottom: 2px solid var(--border);
      }

      /* ── Control button ──────────────────────────────────────────────── */
      .tabs-row-btn {
        flex: 0 0 48px;
        width: 48px;
        height: 48px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        padding: 0 !important;
        margin: 0 !important;
        background: #2a5568 !important;
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
      .tabs-row-btn:hover  { background: #1f4454 !important; }
      .tabs-row-btn:active { background: #194050 !important; }
      .tabs-row-btn:focus  { outline: none; color: #fff !important; }
      .tabs-row-btn:focus-visible { outline: 2px solid #85c1e9; outline-offset: -2px; }
      .tabs-row-btn svg { width: 22px; height: 22px; fill: currentColor; display: block; }
      #global_sidebar_toggle {
        flex: 0 0 64px;
        width: 64px;
      }

      /* ── Tab strip wrapper: fills space between buttons ─────────────────── */
      .scenario-tabs-wrapper {
        flex: 1 1 0;
        min-width: 0;
        max-width: 100%;
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
        box-sizing: border-box;
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
        /* All tabs share one charcoal surface; selection is shown by a thin
           bottom accent + slightly lighter fill, not a colored block. */
        background: #252b2e;
        color: #99a2a6;
        box-shadow: inset 0 -2px 0 transparent;
        text-decoration: none;
        transition: color 0.12s, background 0.12s, box-shadow 0.12s;
        -webkit-tap-highlight-color: transparent;
      }
      #scenario_tabs.nav-tabs > li.active > a,
      #scenario_tabs.nav-tabs > li > a.active {
        color: #ffffff;
        background: #2e3538;
        box-shadow: inset 0 -2px 0 #cdd3cf;
      }
      #scenario_tabs.nav-tabs > li > a:hover:not(.active) {
        color: #c8cdc9;
        background: #2a3033;
      }
      #scenario_tabs.nav-tabs > li > a[data-value='__add_tab__'] {
        color: #3d7a52;
        font-size: 1.5rem;
        min-width: 48px;
      }
      #scenario_tabs.nav-tabs > li > a[data-value='__add_tab__']:hover {
        color: #2f6042;
        background: #252b2e;
      }

      /* ── tab-content and scenario content: no container framing ─────────── */
      .tab-content {
        flex: 1 1 auto;
        min-height: 0;
        border: 0 !important;
        background: transparent !important;
        padding: 0 !important;
        overflow-y: auto !important;
        overflow-x: hidden;
        -webkit-overflow-scrolling: touch;
        overscroll-behavior: contain;
      }
      .tab-content.sidebar-open {
        overflow-y: hidden !important;
      }
      .tab-content > .tab-pane {
        min-height: 100%;
        max-width: var(--shell-max);
        margin: 0 auto;
        width: 100%;
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

      /* Trim headroom above the first heading in the probability panel */
      .probability-section { padding-top: 6px; }
      .probability-section > .section-divider:first-child { margin-top: 0; }

      /* ── Section dividers — rendered as semantic headings ────────────── */
      .section-divider {
        display: flex; align-items: center; gap: 8px;
        margin: 14px 0 10px;
        color: var(--text-muted); font-size: 0.72rem; font-weight: 700;
        letter-spacing: 0.09em; text-transform: uppercase;
      }
      .section-divider::after {
        content: ''; flex: 1; height: 1px; background: var(--border);
      }

      /* ── Win-probability supporting details (tucked under the meter) ───── */
      .matchup {
        margin-top: 4px;
        border-top: 1px solid var(--border);
        padding-top: 14px;
      }
      /* Favored-side verdict bridges meter → numbers */
      .verdict-chip {
        text-align: center; font-size: 1rem; font-weight: 700;
        color: var(--text); margin-bottom: 12px;
      }
      .fav-atk .verdict-chip { color: var(--atk-text); }
      .fav-def .verdict-chip { color: var(--def-text); }
      /* Three aligned columns: attacker · simulations · defender */
      .matchup-readout {
        display: flex; align-items: flex-start;
      }
      .mr-side {
        flex: 1 1 0; min-width: 0; text-align: center;
        padding: 0 8px; display: flex; flex-direction: column; gap: 2px;
      }
      .mr-atk, .mr-def { position: relative; }
      /* Hairline separators between the three groups */
      .mr-sims { border-left: 1px solid var(--border); border-right: 1px solid var(--border); }
      .mr-pct {
        font-size: 1.7rem; font-weight: 800; line-height: 1.05;
        font-variant-numeric: tabular-nums; color: var(--text);
      }
      .mr-atk .mr-pct { color: var(--atk-text); }
      .mr-def .mr-pct { color: var(--def-text); }
      .mr-team {
        font-size: 0.7rem; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.07em; color: var(--text-muted);
      }
      /* The favored side leads; the trailing side recedes (kept readable) */
      .matchup.fav-atk .mr-def .mr-pct,
      .matchup.fav-def .mr-atk .mr-pct { opacity: 0.55; }

      /* ── Visible focus for keyboard users ────────────────────────────── */
      .form-control:focus-visible,
      .btn:focus-visible,
      .accordion-button:focus-visible,
      #scenario_tabs.nav-tabs > li > a:focus-visible,
      .tabs-row-btn:focus-visible {
        outline: 2px solid var(--def-text) !important;
        outline-offset: 2px;
        box-shadow: none !important;
      }

      /* ── Generic muted helper text ───────────────────────────────────── */
      .dashboard-note { font-size: 0.85rem; color: var(--text-muted); margin-bottom: 10px; }

      /* ── Recommendation callout cards ────────────────────────────────── */
      .rec-callouts { display: flex; flex-direction: column; gap: var(--s1); margin-top: var(--s1); }
      .rec-card {
        background: var(--surface); border: 1px solid var(--border);
        border-left-width: 4px; border-radius: var(--radius);
        padding: 12px 14px;
      }
      .rec-card-atk { border-left-color: var(--atk-fill); }
      .rec-card-def { border-left-color: var(--def-fill); }
      .rec-card-title {
        font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.07em;
        font-weight: 700; color: var(--text-muted); margin-bottom: 4px;
      }
      .rec-card-unit { font-size: 1.15rem; font-weight: 800; }
      .rec-card-atk .rec-card-unit { color: var(--atk-text); }
      .rec-card-def .rec-card-unit { color: var(--def-text); }
      .rec-card-meta { font-size: 0.85rem; color: var(--text); margin-top: 2px; }

      /* ── Content sections (replace card wrappers in main panel) ──────── */
      .content-section { padding: 16px; }

      /* ── Run button — muted military green ───────────────────────────── */
      .btn-run {
        background: #3d7a52 !important; color: #fff !important;
        border: none !important; font-weight: 600; letter-spacing: 0.03em;
        padding: 10px !important; border-radius: 3px !important;
      }
      .btn-run:hover  { background: #326645 !important; }
      .btn-run:active { background: #285438 !important; }

      /* ── Mobile ──────────────────────────────────────────────────────────── */
      @media (max-width: 575.98px) {
        .tabs-row-btn { flex: 0 0 44px; width: 44px; height: 44px; font-size: 1.3rem; }
        #global_sidebar_toggle { flex: 0 0 56px; width: 56px; }
        .tabs-row-btn svg { width: 20px; height: 20px; }
        .scenario-tabs-wrapper { height: 44px; }
        #scenario_tabs.nav-tabs { height: 44px; }
        #scenario_tabs.nav-tabs > li > a { height: 38px; min-width: 58px; padding: 0 8px; font-size: 1rem; }
        #scenario_tabs.nav-tabs > li > a[data-value='__add_tab__'] { min-width: 44px; }
        .app-title-bar { padding: 6px 12px; }

        /* Sidebar on mobile: bslib absolutely-positions the aside panel,
           but the grid row still reserves its height as a gap.
           Collapse that reserved space so only .main sets the height. */
        .bslib-sidebar-layout {
          --_sidebar-width: 90vw !important;
        }
        .bslib-sidebar-layout[data-collapsible-mobile='true'] > aside {
          height: calc(100dvh - var(--header-height)) !important;
          max-height: calc(100dvh - var(--header-height)) !important;
          overflow-y: auto !important;
          overflow-x: hidden !important;
          -webkit-overflow-scrolling: touch;
          overscroll-behavior: contain;
        }
        .bslib-sidebar-layout[data-collapsible-mobile='true'] > aside > .sidebar-content {
          height: auto !important;
          max-height: none !important;
          overflow: visible !important;
          padding-bottom: calc(1rem + env(safe-area-inset-bottom));
        }
        .bslib-sidebar-layout[data-collapsible-mobile='true'] > aside .card,
        .bslib-sidebar-layout[data-collapsible-mobile='true'] > aside .card-body,
        .bslib-sidebar-layout[data-collapsible-mobile='true'] > aside .sidebar-content {
          max-height: none !important;
          overflow: visible !important;
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
      function syncSidebarScrollState() {
        var tabContent = document.querySelector('.tab-content');
        if (!tabContent) return;
        var isMobile = window.matchMedia('(max-width: 575.98px)').matches;
        var activeLayout = document.querySelector(
          '.tab-content > .tab-pane.active .bslib-sidebar-layout[data-collapsible-mobile=\"true\"]'
        );
        var sidebarOpen = isMobile && activeLayout &&
          !activeLayout.classList.contains('sidebar-collapsed');
        tabContent.classList.toggle('sidebar-open', !!sidebarOpen);
      }

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
          var headerHeight = header.offsetHeight + 'px';
          document.documentElement.style.setProperty('--header-height', headerHeight);
          spacer.style.height = headerHeight;
        }
        syncSidebarScrollState();
      }
      // Fire immediately, on Shiny ready, and on resize
      document.addEventListener('DOMContentLoaded', hoistTabContent);
      $(document).on('shiny:sessioninitialized', hoistTabContent);
      window.addEventListener('resize', hoistTabContent);
      document.addEventListener('bslib.sidebar', syncSidebarScrollState, true);
      document.addEventListener('shown.bs.tab', syncSidebarScrollState, true);

      Shiny.addCustomMessageHandler('scroll_active_tab', function(_) {
        setTimeout(function() {
          var el = document.querySelector('#scenario_tabs li.active > a, #scenario_tabs .nav-link.active');
          if (el && el.scrollIntoView) {
            el.scrollIntoView({behavior: 'smooth', inline: 'center', block: 'nearest'});
          }
          syncSidebarScrollState();
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
            'z-index:99999;background:#a02020;color:white;text-align:center;' +
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
      tags$h1(class = "app-title-text",
              style = "font-size: 1.15rem; font-weight: 600; margin: 0;",
              "d1066 Battle Simulator"),
      div(class = "app-title-spacer")
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
            title = SCENARIO_NUMERALS[1],
            value = "scenario_alpha",
            scenarioUI("scenario_alpha", label = SCENARIO_NUMERALS[1])
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

  # ── Track currently-open scenarios (stable internal id + display label) ──
  # We always keep the original alpha scenario (started in UI).
  scenarios <- reactiveVal(
    data.frame(id = "scenario_alpha", label = SCENARIO_NUMERALS[1],
               stringsAsFactors = FALSE)
  )
  next_scenario_seq <- reactiveVal(1L)

  next_scenario_id <- function() {
    next_seq <- next_scenario_seq() + 1L
    next_scenario_seq(next_seq)
    sprintf("scenario_%04d", next_seq)
  }

  mount_scenario <- function(id, label) {
    scenarioServer(id, label = label)
  }

  # Boot up the alpha scenario's server logic.
  mount_scenario("scenario_alpha", SCENARIO_NUMERALS[1])

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
    available_labels <- base::setdiff(SCENARIO_NUMERALS, current$label)
    if (length(available_labels) == 0) {
      # Bounce the user off the "+" tab back to the last real scenario,
      # and tell them why nothing happened.
      nav_select("scenario_tabs",
                 selected = current$id[nrow(current)],
                 session  = session)
      showNotification(
        sprintf("Tab limit reached (%d scenarios).", MAX_SCENARIOS),
        type = "warning",
        duration = 4
      )
      return()
    }

    new_id    <- next_scenario_id()
    new_label <- available_labels[1]

    # Insert the new scenario tab BEFORE the "+" tab.
    nav_insert(
      id       = "scenario_tabs",
      target   = "__add_tab__",
      position = "before",
      nav      = nav_panel(
        title = new_label,
        value = new_id,
        scenarioUI(new_id, label = new_label)
      ),
      session  = session
    )

    # Boot up the new scenario's server logic.
    mount_scenario(new_id, new_label)

    # Track it in our reactive list.
    scenarios(rbind(
      current,
      data.frame(id = new_id, label = new_label, stringsAsFactors = FALSE)
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
