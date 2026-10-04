# Code and calculation review: `Multi_objective_satisfaction_model.R`

Response to issue #1 ("Help to check function and code accuracy").
Reviewed 2026-10-04 against commit `adeeef0` and `input_estimates.xlsx`.
The script was run as it is (`n_runs = 200`, `set.seed(2026)`). It runs from start to finish in about 65 s.

## Short answer

The R code runs correctly: loops, naming, reshaping, probabilities, OI* selection and figures all
do what the comments say. The problem lies in a few biophysical calculations rather than the R
mechanics. **OI* is high mainly because the model makes maize and soybean strongly P-limited, so
the yield target can only be reached by adding a lot of OI. The thresholds are not the main cause.**
SOC gains are also too large, because almost all of the added carbon is kept in the soil.

What the current run shows (Viability targets, median of 200 runs):

| | OI = 0 | OI = 2 | OI = 5 | OI = 10 |
|---|---|---|---|---|
| Maize mid, mean yield (t/ha) | 1.05 | 2.23 | 3.73 | 4.62 |
| Maize mid, SOC change (%) | -26 | +21 | +80 | +145 |
| Maize mid, P(yield ok) | 0.00 | 0.14 | 0.92 | 1.00 |
| Maize mid, P(SOC ok) | 0.01 | 0.88 | 1.00 | 1.00 |

The yield target is almost always the binding constraint: `Yieldok` ≈ `Jointok` in every case.
Even under Viability, OI* ranges from 3.75 to 6.75 t DM/ha/yr. Under Base, maize OI* is 8–9.5.

## 1. Main issue: P supply vs. demand (this drives OI* upward)

With mid-point parameter values (maize, mid slope, year 1):

| OI (t DM/ha) | N index | P index | K index |
|---|---|---|---|
| 0 | 0.63 | **0.21** | 0.51 |
| 2 | 0.69 | **0.37** | 0.98 |
| 5 | 0.74 | **0.56** | 1.00 |

Because `nutrient_index <- min(N_index, P_index, K_index)`, P sets the yield. At OI = 0, with the full
mineral fertiliser package, the model gives about 1 t/ha maize. That is below your own
`maize_baseline_yield` (1.5–2.2 t/ha, which is defined but never used). Three things combine to cause this:

1. **Soil P and K units (lines 452–453).** `P_soil_*` and `K_soil_*` are in **mg/kg**, but they are
   added to fertiliser and OI P, which are in **kg/ha**. For 20 cm depth, 1 mg/kg ≈
   `bulk_density × 2` kg/ha (≈ 3 kg/ha at BD 1.5). The current soil-P term is about 0.35 kg P/ha, which in
   practice means "soil supplies no P". Either convert to kg/ha, or (if you mean soil-test P to be
   the plant-available pool) rethink the `P_soil_availability` = 2–5 % factor. Applying 2–5 % to a
   pool that is already Bray/Olsen-available makes the factor too small twice over.
2. **P₂O₅ / K₂O vs. elemental P / K (lines 448–450).** `maize_P_rate`, `soybean_P_rate` are entered as
   kg **P₂O₅**/ha and `*_K_rate` as kg **K₂O**/ha, but `P_demand` / `K_demand` and all OI
   concentrations are elemental P and K. The conversions are P = P₂O₅ × 0.436 and K = K₂O × 0.830. The cost
   lines (295–299) are consistent because the price is also per kg P₂O₅. Only the biophysical use
   needs the conversion. Note: this fix makes P limitation **worse**, so it has to be done together
   with point 1.
3. **Demand is set at `ypot`** (`cp$ypot * cp$P_demand`). That works if the index is read as "fraction
   of potential". Just check that the `P_demand` values (4–7 kg P per t grain for maize) mean total
   plant uptake per t grain and not grain P removal only.

**Suggested check:** after fixing, yield at OI = 0 (with the fertiliser package) should be at or
above `*_baseline_yield`. You could add this as an explicit calibration check, because `y_base` is
already in `crop_pars` but never used.

## 2. SOC gains are too large: nearly all input C becomes SOC (lines 525–527)

```r
C_to_stable <- hum_eff * C_input_total
C_to_active <- C_input_total - C_to_stable
```

All of the input carbon goes into SOC. The non-humified part goes into the active pool, which then
decays at `k_active` (0.15–0.25/yr). At steady state, 5 t DM/yr adds about 6 t C/ha to the active pool
alone, on an initial stock of about 15 t C/ha. That is why SOC rises 70–80 % at 5 t and 130–155 % at
10 t. Humification coefficients are usually defined as the fraction of added C still present after
about one year, with the rest respired as CO₂. A pool-consistent version would be something like:

```r
C_to_stable <- hum_eff * C_input_total
C_to_active <- active_retention * (C_input_total - C_to_stable)  # rest is respired
```

Here `active_retention` would be a new, literature-based parameter. Alternatively, send `hum_eff × C_input` to
SOC (split between active and stable) and respire the rest. The same split should then apply to
`N_pool_input` (lines 535–536).

**Related:** saturation (line 516) only scales `hum_eff`. Because `C_to_active = C_input − C_to_stable`,
any C that saturation keeps out of the stable pool goes to the active pool instead. **Saturation
therefore never limits total SOC.** Fixing the respiration step above also fixes this.

Because of this, the SOC target is reached far too easily. Once P is fixed and yield stops being the
binding constraint, SOC will matter more, so fix both together.

## 3. N accounting is inconsistent (lines 441–465)

- Fertiliser N is multiplied by `N_recovery` (0.3–0.5), but OI-available N (`N_org`), mineralised soil N
  (`N_soil`) and BNF go into `N_tot` at 100 %. This makes a kg of OI N worth 2–3× a kg of fertiliser N.
  That may be intended (the availability coefficients already discount OI N), but it should be a
  deliberate choice. As it stands, soil-mineralised N has no loss term at all.
- `N_half` is described as the "N **rate** achieving 50 % of potential yield", which is an applied-N
  parameter. In the code it is used against total N supply (soil + OI + fertiliser + BNF). The
  description and the use should match.

## 4. Economics: family labour is free (lines 560–562)

Only `hired_days` (labour above `hh_labour_available`) is costed. OI labour (2–4 days/t spreading,
plus 3–6 days/t carting without a cart) costs nothing until household capacity runs out. This
makes high OI rates look cheap. You could add a shadow wage for family labour, at least as a sensitivity
run. Also note that `interest_rate_input` applies to seed/fertiliser/pesticide but not to `oi_cash_cost`.

## 5. Decision rule depends on the edge of the grid

`select_oi_robust()` picks the lowest OI with P(joint) ≥ 95 % of the **maximum over the grid**. When
P(joint) keeps rising up to 10 t (it does for maize Base/Stringent: max_prob 0.02–0.47), OI* sits
next to the grid edge and would change if `oi_sweep` changed. Suggestions:

- Flag the cases where `max_prob` is reached at the last grid point, or where `max_prob` is low, and
  report them as "not attainable within the realistic range" rather than as an optimum.
- The script comment says 5 t DM is the resource-efficient range you meant to test, but the rule uses the full
  0–10 grid. Either restrict the decision range to ≤ 5 t, or state clearly that OI* > 5 is outside it.
- There is no OI **availability** constraint. Farm-level biomass supply is probably the real limit
  in northern Ghana and is worth discussing in the manuscript.

## 6. Smaller points

- Line 602–603 and 626: comments say "Base = primary decision analysis; Minimum, Stringent =
  robustness", but line 103 says **Viability** is primary, and there is no "Minimum" scenario.
- `drought_year` (`dryspell_damage`) comes on top of `rain_index`, which already varies around 1
  with a floor. Check that this does not count drought twice. It may be intended (a mid-season dry spell is
  not the same as a low seasonal total), but say so in the methods.
- `water_supply` adds an absolute rain term to a relative AWC change. That is fine as a heuristic. Note that
  upper-slope water_index averages about 0.67 even in a normal year, because of `1 - runoff`.
- `n_runs <- 200`: probabilities move in steps of 0.005, so OI* can jump by one grid step between
  seeds. Use 10 000 for the manuscript, as you planned.
- `make_variables(input_estimates)` at top level puts all variables in the global environment.
  This is harmless now, but if a parameter is ever removed from the spreadsheet the model would silently
  use the stale global value. Run `rm(list = parameters$variable)` before `mcSimulation`, or
  run the model in a fresh session.

## What is correct (checked)

- SOC stock conversion `soc_fraction × BD × depth(m) × 10 000` = t C/ha ✔
- Blend normalisation and share-weighted C, N, P, K ✔
- Pool decomposition with Q10 and N released at pool C:N ✔
- Paired common random numbers across OI rates, crops and slopes ✔
- Discounting, NPV, DROI = NPV / PV(costs) ✔
- Target tests, joint indicator, reshaping regexes, probability summaries, OI* join for Figure 2 ✔

## Suggested order of work

1. Fix soil P/K units and the P₂O₅/K₂O conversion. Check that OI = 0 yields ≈ baseline yields.
2. Add respiration of non-humified C. Check SOC change at 0, 2 and 5 t against long-term trial data.
3. Decide on the N recovery treatment and on costing family labour.
4. Then look again at the thresholds. They may turn out to be reasonable once 1–2 are fixed.
