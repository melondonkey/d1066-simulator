# d1066 Battle Simulator

A [Shiny](https://shiny.posit.co/) app that models the dice-resolution step of a
battle in **[Dragons of 1066](https://dragonsof1066.com/)**. Configure attacking
and defending forces, run thousands of simulations, and view win probabilities,
the outcome distribution, and the marginal value of buying one more of each unit
type.

> This is an unofficial, fan-made tool — not affiliated with or endorsed by the
> makers of Dragons of 1066. It references the game only to analyze it, and
> includes none of the game's artwork, audio, or rules text.

## About the game

[**Dragons of 1066**](https://dragonsof1066.com/) is a turn-based fantasy
strategy board game for 2–4 players, set in a Mythic Europe of the year 1066.
Each turn you move pieces to create battles, resolve them by rolling dice, then
reposition and recruit; you win by capturing all four Fortresses on the board. A
digital version is available with online/local play and cross-play across iOS,
Android, and Steam. See <https://dragonsof1066.com/> for rules, FAQs, and to
play.

## How a battle is modeled

Each side fields three dice-typed units — **d6**, **d12**, and **d20** — and the
defender may also hold one or more **castles**. Combat resolves in rounds:

- Attacker and Defender units (but not castles) each roll their die each round.
- A roll of **6 or higher** scores a kill against the opposing side.
- Defender losses are applied in rules order: castles first, then d6/d12/d20
  units. Attacker losses are assigned by the attacker in the board game; this
  simulator uses a simple lowest-die-first casualty policy.

The simulator models a single complete battle from start to finish, following the
dice-resolution rules: castles don't roll, they die when hit, and defender
casualties die in rules order. Once a battle ends, we're done (we don't simulate
the post-battle restoration, which doesn't affect the outcome of that single
battle).

Larger dice kill more reliably, but **unit costs** are `d6 = 1`, `d12 = 2`,
`d20 = 2`, so cost-effectiveness isn't obvious. See
[d1066-unit-buying-guide.md](d1066-unit-buying-guide.md) for the rules of thumb
(short version: +1 d6 is usually the best buy per cost).

## True Roll and prediction accuracy

The digital game resolves dice with **True Roll**: you physically throw the dice,
and your throw is combined with a pre-committed random "True Table" to produce
results — no computer RNG once play begins. The algorithm is public and the table
is revealed afterward, so rolls are auditable. The point is that True Roll is
designed to be **fair and unbiased**.

That matters here because the simulator models each die as a fair, uniform
d6/d12/d20 with **independent** rolls (`sample(1:n, replace = TRUE)`). If True
Roll's results are statistically indistinguishable from independent uniform draws,
then a real die behaves like the simulator's model die, and the app's win rates
are good estimates of the actual battle odds. A fair, unbiased system gives you
the *uniform* half of that for free; the *independence* half is a separate
assumption (see caveats below).

A few honest caveats about what "accurate" means:

- **These are estimates, not exact odds.** The app uses Monte Carlo simulation, so
  each win rate is a sample estimate with sampling error that shrinks roughly like
  1/√N as you raise the simulation count. More simulations → a tighter estimate,
  never a guarantee.
- **Distribution, not destiny.** The estimated *probability* can be right while any
  single fight still goes either way — a heavy favorite can lose one battle.
- **Uniform *and* independent are assumptions.** "Unbiased" covers the per-roll
  distribution; the model also assumes rolls don't influence each other. True
  Roll's rolling-seed design is built to be fair, but the model is only as good as
  that independence assumption holds.
- **Dice step only.** Board position, recruitment, and alliances are out of scope,
  so treat the numbers as combat odds, not a prediction of who wins the game.
- **Attacker casualty choice is simplified.** The board game lets attackers choose
  which units take hits; this simulator always removes attacker units from lowest
  die to highest die.

## Features

- Win probability gauge and Attacker/Defender win-% value boxes.
- Outcome distribution histogram of surviving units.
- **Marginal Unit Value** panel: estimated win-rate change from +1 of each unit
  type, normalized by cost, with a recommended best buy. (When two options are
  close, the recommendation can shift between runs due to simulation noise — raise
  the simulation count to sharpen it.)
- Multiple scenarios in tabs (up to 24) to compare compositions side by side.
- Shareable URLs — input state is captured in the query string via Shiny
  bookmarking.

## Running locally

Requires R (4.x) and the packages below:

```r
install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "plotly"))
shiny::runApp("d1066-simulator.R")
```

Adjust the Battle Setup fields, then click **▶ Run Simulation**. Editing inputs
(including spinner arrows) does not recompute until you click Run; the app
auto-runs once on load.

Numeric fields are validated server-side and accept **whole numbers only** (unit
counts ≥ 0, simulations ≥ 1). Invalid values (e.g. `3.9`, `7e6`) show an inline
message instead of crashing. This check is the single source of truth and covers
typed, pasted, bookmarked, and query-string input.

## Deployment

Deployed to [shinyapps.io](https://www.shinyapps.io/) (metadata in `rsconnect/`
and `shinyapps.io/`). Redeploy with:

```r
rsconnect::deployApp(appPrimaryDoc = "d1066-simulator.R")
```

## Project layout

| Path | Description |
| --- | --- |
| [d1066-simulator.R](d1066-simulator.R) | The complete Shiny app (UI, server, simulation logic). |
| [d1066-unit-buying-guide.md](d1066-unit-buying-guide.md) | Strategy guide from a simulation sweep. |
| `rsconnect/`, `shinyapps.io/` | Deployment metadata. |
