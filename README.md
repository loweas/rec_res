# Unequal Recreational Losses? Revealed Preference Evidence of Campsite Closures in Hawaiʻi 
---
Please find this [Github Page](https://loweas.github.io/rec_res/) to accompany the work in the following paper.

## Folder Structure

```
project/
│
├── README.md                          ← this file
│
├── data/
│   ├── raw/                           ← original unmodified data (do not edit)
│   │   ├── ZIP_TRACT_122025.xlsx      ← HUD ZIP-to-tract crosswalk (Dec 2025)
│   │   ├── dishawaii.rds              ← EJScreen tract-level data for Hawaii
│   │   ├── reservation_data.csv       ← raw DOFAW reservation records 2018–2023
│   │   └── intra_island_distance/     ← driving distance matrix (zip × campsite)
│   │
│   ├── processed/                     ← cleaned data objects saved from R
│   │   ├── final_data_YYYY.rds        ← annual choice data (2018–2023)
│   │   ├── final_comparison_master.rds   ← welfare estimates merged across years
│   │   ├── final_comparison_master1.rds  ← above + EJScreen indicators
│   │   ├── dishawaii_zip.rds          ← ZIP-level EJScreen composites
│   │   └── income_lookup.rds          ← individual id → income → income_block
│   │
│   └── tables/                        ← exported HTML and Excel tables
│       ├── Welfare_Comparison_Table_mdcev_*.html
│       ├── Welfare_Comparison_Table_frequency_*.html
│       ├── swait_louviere_stability.html
│       ├── ji_temporal_reliability_summary.csv
│       └── trips_by_income_block.xlsx
│
├── figures/                           ← all saved plots (300 dpi PNG)
│   ├── hawaii_welfare_loss_map.png        ← Figure 3: spatial welfare map
│   ├── distributional_figure4.png         ← Figure 4: welfare by distribution
│   ├── appendix_A_welfare_2018_2020.png   ← Appendix A: years 2018–2020
│   ├── appendix_B_welfare_2021_2023.png   ← Appendix B: years 2021–2023
│   ├── option_value_gap_decomposition.png ← sensitivity analysis
│   ├── geographic_constraint_tests.png    ← distance selection tests
│   ├── ji_reliability_dotplot.png         ← Ji et al. stability dotplot
│   └── stability_lineplot.png             ← sequential stability line
│
├── _site/                            ← File is set up to have a github page 
│                                         There are many more files in this folder for the website
│                                          https://loweas.github.io/rec_res/
│   
├── models/                            ← estimated model objects
│   ├── model_list_obs.rds             ← observed RUM models (Year_2018:Year_2023)
│   ├── model_list_52.rds              ← frequency RUM models (52wk_2018:52wk_2023)
│   └── mdcev_models/                 ← MDCEV model objects by year
│
└── qmd/                               ← Quarto analysis files (run in order)
    ├──index.qmd
    ├──mdcev.qmd
    └── rum.qmd
```

## Main Files

### `index.qmd`
In this main file we pull the data from MDCEV and RUM and do comparsion analysis. Firt we will consider the Welfare measurements across models, EJ distributions by examining income, social vulnerabilities and environmental burdens. We also include some temporal spatial stability. 

## Welfare Comparsion 

Comparision across all MDCEV and RUM.

## EJ Distribution

Crosswalks EJScreen indicators from census tract to ZIP code level using the HUD ZIP-to-tract crosswalk file (population-weighted means via `TOT_RATIO`). Constructs:

- `env_composite`: mean of 11 environmental burden indicators (PM2.5 and ozone excluded — limited Hawaii data)
- `soci_composite`: mean of 8 social vulnerability indicators
- `env_block` / `soci_block`: percentile blocks (0–20th through 95–100th)
- `is_disadvantaged_50`: ZIP flagged if 100% of population in DAC tracts. The matching process provides population weights.

**Key outputs:** `dishawaii_zip`, updated `final_comparison_master1`


Examines welfare losses across three distributional dimensions: household income, social vulnerability (EJScreen), and environmental burden (EJScreen). Produces Figure 4 and appendix distributional figures. Includes sensitivity analysis of the income gradient in the frequency RUM — decomposing the option value gap into beta scaling, ASC, and geographic constraint components.

**Key outputs:** Figure 4, appendix figures A and B, `all_avg`, `all_avg_appendix`

## Stability Questions

Implements two temporal stability tests following Ji, Keiser and Kling (2020):

1. **Pairwise t-tests** across all 15 year combinations using `combn(2018:2023, 2)`
2. **Sequential % change** year-over-year at site level
3. **Transfer error** — % difference when using one year's estimates to predict another
4. **Swait-Louviere (1993) test** — likelihood ratio test of parameter stability (2018 vs 2019 and 2018 vs 2023 only — 2020–2022 fail due to COVID balanced-parks requirement)

**Key outputs:** `stability_all`, `transfer_errors`, `sl_results`, stability figures

---

### `mdcev.qmd`

Shows code to generate data but data is de-identified and cleaned for individual-level panel data. Key steps: parse check-in dates, create year variable, recode zip code 96812 → 96813, calculate average days visited per individual per year, build `final_data_YYYY` objects with visit counts per individual-campsite combination, assign campsites to islands via `island_park`, and flag inter-island visits.

### Notes on Travel Cost
Constructs the individual and site-specific travel cost variable used as the price variable in all welfare models. Price has four components:

| Component | Formula | Notes |
|---|---|---|
| Driving cost | `distance × cost_per_km / 3` | AAA per-km cost divided by avg party size 3 |
| Value of time driving | `(income × 0.75 / 2080) × distance / 50` | 75% of wage rate, 50 km/h speed |
| Inter-island flight cost | Fixed by route and year | AAA/BLS inflation-adjusted |
| Value of time flying | `(income × 0.75 / 2080) × flight_hours × 2` | Round trip |

Also adds car rental, parking, and permit fees by island-route combination. Travel cost is calculated separately for each year using AAA driving cost brochures (2018–2023).

**Key outputs:** `price` column added to all `final_data_YYYY` objects

Estimates the Multiple Discrete-Continuous Extreme Value (MDCEV) model for each year using the `rmdcev` package with gamma specification, MLE algorithm, and 100 simulation draws. System closure is modelled by setting all site prices to 999,999. Individual welfare is extracted from simulation draws and averaged across draws.

**Key outputs:** `welfare_YYYY_ind` objects, `welfare_YYYY` site-level summaries

---

### `rum.qmd`
Estimates two Random Utility Model specifications for each year:

- **Observed RUM:** conditions welfare on observed trip occasions only. Stay-at-home alternative included but never chosen — coefficient effectively unidentified.
- **Frequency RUM (52-week):** creates 52 weekly choice occasions per individual. Stay-at-home IS chosen in non-trip weeks — coefficient identified at 5.90***.

Both models use `mlogit(choice ~ price | 1)` via the `mlogit` and `dfidx` packages.

**Key outputs:** `model_list_obs`, `model_list_52`


Applies the log-sum welfare formula to each model's estimated coefficients:

$$CV_{it} = \frac{\ln \sum_{j \neq k} \exp(V_{ijt}) - \ln \sum_{j} \exp(V_{ijt})}{-\hat{\beta}_p}$$

Individual annual welfare = sum of per-trip CV across all observed trips. Welfare is calculated for each of the 22 campsite closures plus system closure. Results merged into `final_comparison_master` and `final_comparison_master1`.

**Key outputs:** `final_comparison_master`, `final_comparison_master1`



## Data Sources

| Source | Description | Access |
|---|---|---|
| DOFAW reservation records | Campsite reservation data 2018–2023 | DOFAW administrative data d-identified |
| HUD ZIP-TRACT crosswalk | `ZIP_TRACT_122025.xlsx` Dec 2025 release | [huduser.gov](https://www.huduser.gov/portal/datasets/usps_crosswalk.html) |
| EJScreen | Census tract environmental justice indicators | [epa.gov/ejscreen](https://www.epa.gov/ejscreen) |
| AAA Driving Costs | Annual per-mile cost brochures 2018–2023 | AAA newsroom |
| OSRM Routing | Driving distance and duration zip → campsite |OSRM API https://project-osrm.org/ |

---

## Citation

If using this analysis please cite:

Unequal Recreational Losses? Revealed Preference Evidence of Campsite Closures in Hawaiʻi 
Ashley Lowe Mackenzie1, Anders Dugstad2,3*, Kirsten L. L. Oleson1

1 Department of Natural Resources and Environmental Management, University of Hawaiʻi at Mānoa, 1910 East-West Road, Honolulu, HI 96822, USA. 

2 School of Economics and Business, Norwegian University of Life Sciences (NMBU), Chr. Magnus Falsens vei 18, 1433 Ås, Norway. 

3 Research Department, Statistics Norway, Akersveien 26, 0177 Oslo, Norway. 

---

## Contact

For questions about the data or code contact alowemac@Hawaii.edu or anders.dugstad@nmbu.no.
