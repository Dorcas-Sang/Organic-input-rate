# Check input values in input_estimates.xlsx

Most values look reasonable. The ones below should be checked against your sources before the next run. The first group has the biggest effect on OI* and SOC.

**1. Likely wrong (start here)**
- [ ] `P_soil_*`, `K_soil_*` × `P/K_soil_availability`: mg/kg is not kg/ha. Soil-test P is already the available pool, so the 2–5 % factor shrinks it twice.
- [ ] `*_P_rate`, `*_K_rate`: entered as P₂O₅ and K₂O. Convert to P (× 0.436) and K (× 0.830) for the biophysical part.
- [ ] `k_active` (0.15–0.25 /yr): slow for an active pool. Fresh C stays too long and inflates SOC.
- [ ] `awc_per_tC` (0.001–0.003): probably 2–5× too high. 1 t C/ha in 20 cm is only about 0.03 % C.
- [ ] `Navail_residue` (0.3–0.5): too high for maize stover (C:N > 50), which usually ties up N in year 1.
- [ ] `runoff_upper` (0.25–0.40): high. It makes the upper slope about 30 % water-short every year.

**2. Costs and prices**
- [ ] Seed (`maize_seed`, `soybean_seed`) and `maize_pesticide` look low.
- [ ] `c_cart_hire` and `oi_material_cash_cost_per_t` look cheap.
- [ ] Make sure all prices, costs and wages come from the same year (maize 1,800–2,500 GHS/t may be out of date).
- [ ] `discount_rate` (0.10) and `interest_rate_input` (0.15–0.30) are low for Ghana. Worth a sensitivity run.

**3. Definitions and units**
- [ ] `hh_labour_available`: per household or per ha? The model uses it per ha.
- [ ] `*_K_demand`, `*_P_demand`: grain removal or total plant uptake? Most maize K is in the stover.
- [ ] `CN_crop` (30–50): one value for both crops. Maize stover is usually > 50, soybean lower.
- [ ] `residue_retained_fraction` (0.3–0.6): may be high for northern Ghana. Also check the same stover is not counted both here and in the OI blend.
- [ ] `soybean_ypot` (3.2–4.2 t/ha), `N_manure`, `P_residue`: on the high side.

These are expert judgements, not checked against references yet. Please confirm each against your literature before changing it. See also the `# CW NOTE:` comments on the `review/code-accuracy` branch.
