# d1066 Unit Buying Guide — Rules of Thumb

Based on a systematic sweep across hundreds of army compositions and budget levels, with thousands of simulated battles per composition (see the note at the end for methodology).

**Unit costs:** d6 = 1, d12 = 2, d20 = 2

---

## The Big Finding: d6 Is Usually the Best Buy Per Cost

Across most scenarios tested, **+1 d6 delivers the highest win-rate improvement per cost spent.** The broad pattern held for both attackers and defenders, though d20 can overtake it in some high-pressure cases: when you are badly outnumbered, when attacking defended fortresses, or in some larger-army comparisons where the difference is close enough to treat as matchup-dependent.

Typical marginal values per cost:

| Scenario | +1 d6 / cost | +1 d12 / cost | +1 d20 / cost |
|---|---|---|---|
| Small armies (2v2) | ~30% | ~24% | ~25% |
| Medium armies (4v4) | ~22% | ~18% | ~21% |
| Large armies (8v8) | ~17% | ~21% | ~23% |

**The d6 advantage comes from unit count, not raw firepower.** Two d6s cost the same as one d12 or one d20. On *expected kills per round*, the single bigger die actually wins: two d6 average 2 × 1/6 ≈ 0.33 kills, versus 7/12 ≈ 0.58 for a d12 and 3/4 = 0.75 for a d20. The d6 edge comes from **survivability**: two bodies absorb two hits before dying, while one d12/d20 absorbs only one. Under the simulator's lowest-die-first attacker casualty policy, that extra body keeps rolling in many multi-round fights — so two d6 often out-trade one bigger die across the whole fight even though they lose the single-round expected-kill comparison.

---

## When d20 Becomes Competitive

d20 closes the gap and occasionally wins in two specific scenarios:

### 1. When You're Heavily Outnumbered
When facing an army roughly double your size, the raw power of d20 matters more because you need high-impact rolls to overcome the body-count disadvantage. At 2 attackers vs 4 defenders, +1 d20 gives +33%/cost vs +18%/cost for d6. **The worse your position, the more d20 helps** — it's a "Hail Mary" unit.

### 2. Against Castles
Castles absorb hits before other defender units do, making battles last longer. This favors d20s because their higher hit probability (75% vs 17% for d6) is more valuable when you need to chew through extra HP. With a castle in play, d20 efficiency rises to roughly match or slightly beat d6.

---

## d12: The Forgotten Middle Child

d12 rarely wins the efficiency contest in this sweep. It costs the same as d20 (2) but has worse hit probability (58% vs 75%). The main advantage d12 has over d20 is the mobility bonus (moves two squares), which is a *strategic* consideration that doesn't show up in the combat simulator. **Buy d12 primarily for mobility, not for combat efficiency.**

---

## Optimal Army Compositions by Budget

The sweep found consistent patterns for the best army composition at each budget level:

| Budget | Best Attacker Comp | Pattern |
|---|---|---|
| 1 | 1×d6 | Only option at cost 1 |
| 2 | 1×d20 | d20 wins at minimum viable army |
| 3 | 1×d6 + 1×d20 | Splash one d20 for punch |
| 4 | 2×d6 + 1×d20 | Core pattern emerges |
| 5 | 3×d6 + 1×d20 | Add d6 bodies |
| 6–7 | Nd6 + 1×d20 | Keep stacking d6, keep the d20 |
| 8+ | Almost all d6 | Swarm wins, d20 becomes unnecessary |

**The pattern:** At low budgets (2–5), include exactly one d20 for its raw power, then fill the rest with d6. At higher budgets (6+), the d6 swarm becomes so overwhelming that even the d20 slot is better spent on two more d6.

---

## The Five Rules of Thumb

1. **Default to d6.** When in doubt, buy d6. It's the best value per cost in the majority of situations.

2. **One d20 at low budgets.** If your total budget is 2–6, include exactly one d20. It's your heavy hitter when body count is low.

3. **All d6 at high budgets.** Once your budget exceeds ~7, go full d6 swarm. The sheer number of dice and hit absorption overwhelms any composition with fewer, stronger units.

4. **Buy d12 mostly for mobility.** The d12 rarely looks combat-optimal in this simulator. Its main advantage is moving two squares. If positioning matters, it earns its keep strategically, not statistically.

5. **Underdogs should buy d20.** If you're outnumbered or facing a castle, d20 becomes more efficient. When you're already losing the numbers game, you need the higher hit probability to have any chance.

---

## Storming the Fortress (5 Castles)

Fortresses completely change the math. With 5 castles acting as a damage sponge in front of a defending garrison, **d20 becomes much more important** — the normal "just buy d6" advice doesn't apply cleanly here.

### Why Fortresses Flip the Script

Castles absorb hits before any garrison units die. That means your first 5 kills each round are wasted on castle HP rather than eliminating defenders who shoot back. This creates a brutal attunement war where you need to sustain high damage output across many rounds. d6 units (17% hit rate) simply don't generate enough kills per round to chew through castles efficiently, while d20s (75% hit rate) tear through them.

### The Fortress Formula: d6 + d20 Core

Against fortresses, the optimal composition consistently follows a pattern: **roughly 50–60% of your budget in d6, the rest in d20.** The d6s provide bodies to absorb return fire, and the d20s provide the sustained damage to crack the castle walls.

| Fortress Garrison | Min Budget for >50% Win | Optimal Comp |
|---|---|---|
| No garrison | 1 | Trivial — any unit wins |
| 2×d6 | 4 | 2×d6 + 1×d20 |
| 4×d6 | 6 | 2×d6 + 2×d20 |
| 2×d6 + 1×d12 | 7 | 3×d6 + 2×d20 |
| 2×d6 + 1×d20 | 8 | 4×d6 + 2×d20 |
| 4×d6 + 2×d12 | 11 | 5×d6 + 3×d20 |
| 4×d6 + 1×d12 + 1×d20 | 12 | 6×d6 + 3×d20 |
| 6×d6 + 2×d12 + 1×d20 | 16 | 8×d6 + 4×d20 |

### Fortress Marginal Values

The marginal efficiency numbers show why d20 is so valuable against defended castles. When your army is smaller than the fortress garrison, d20 wins per cost handily:

| Scenario | +1 d6 / cost | +1 d20 / cost | Winner |
|---|---|---|---|
| 4×d6 vs Fort + 2×d6 | +28.6% | +31.8% | **d20** |
| 6×d6 vs Fort + 4×d6 | +22.0% | +32.3% | **d20** |
| 10×d6 vs Fort + 4×d6+2×d12 | +8.3% | +23.0% | **d20** |

But once you already have enough firepower and are winning comfortably (>80% win rate), d6 goes back to being the better marginal buy — you don't need more punch, you need more bodies to guarantee the win.

| Scenario | +1 d6 / cost | +1 d20 / cost | Winner |
|---|---|---|---|
| 6×d6 vs Fort + 2×d6 (81% wr) | +12.0% | +9.4% | **d6** |
| 8×d6 vs Fort + 2×d6 (98% wr) | +1.4% | +0.9% | **d6** |

### Fortress Rules of Thumb

1. **Expect to spend well above the garrison's budget.** In this sweep, a fortress with a 4-cost garrison (e.g. 4×d6) needed about budget 6 to crack, while a 10-cost garrison needed budget 16+. The 5 castles substantially increase the effective defense.

2. **Bring d20s against defended fortresses.** Unlike many field battles, d20 is often the most efficient unit per cost when assaulting a fortress with units behind it. Plan for roughly half your budget in d20.

3. **The composition is usually d6 + d20, with little d12.** d12 rarely appears in optimal fortress assault compositions in this sweep. It doesn't hit hard enough for the castle-cracking role and doesn't provide enough bodies for the meat-shield role.

4. **At very high budgets, d6 swarm takes over again.** Once your budget exceeds ~14–16 against a moderate garrison, the sheer mass of d6 units eventually overwhelms even a fortress. But you need a LOT of them.

5. **An ungarrisoned fortress is meaningless.** 5 castles with no troops can be taken by a single d6 with 100% success. Castles only matter when there are units behind them dealing damage while you chew through the walls.

---

## Defender vs Attacker — Any Difference?

The same rules mostly apply to defenders, with one nuance: **the defender's d6 advantage often looks even stronger** than the attacker's. In mirror matchups from this sweep, the defender's marginal d6 outperformed d12/d20 by a wider margin than on the attack side. This is because defenders benefit more from absorbing hits (they're trying to survive, not push through), and extra bodies = extra absorption.

---

## Methodology and caveats

*Analysis: ~3,000 simulated battles per data point, ~400+ compositions tested
across budgets 1–20 against 16 defender setups including 8 fortress
configurations.*

These are **Monte Carlo estimates**, not exact probabilities. Every win rate and
marginal value above carries sampling error that shrinks roughly like 1/√N with
the number of simulations. At ~3,000 runs, sampling error is roughly ±1–2
percentage points per estimate, but marginal values compare multiple simulated
rates and can stack those errors — so recommended "winners" in close matchups can
shift between runs. Consequences to keep in mind:

- **Close calls are noise-sensitive.** Where two units are within a couple of
  points per cost, the "winner" can flip between sweeps. Treat near-ties as ties.
- **Hit probabilities are exact, the rest is estimated.** The per-die hit chances
  (d6 = 1/6 ≈ 17%, d12 = 7/12 ≈ 58%, d20 = 3/4 = 75%) are arithmetic facts of the
  rules. The optimal-composition tables and marginal values are empirical results
  of one simulation sweep and have not been independently re-derived here.
- **Attacker casualty choice is simplified.** The board game lets attackers choose
  which units take hits. The simulator assumes attacker casualties are assigned
  from lowest die to highest die, matching the cheap-body strategy but not every
  possible player choice.
- **Strategic factors are out of scope.** The sweep models only the dice-resolution
  step, so mobility, positioning, recruitment, and alliances don't appear in these
  numbers (this is why d12's mobility edge never shows up).
