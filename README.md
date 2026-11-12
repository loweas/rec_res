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
│   │
│   ├── reservation_data_merged_id_complete.dta  ← de-identified and cleaned individual-level panel data (2018–2023)
│   ├── intra_island_distance2.csv                 ← intra-island distance and duration (zip → campsite)
│   ├── final_comparison_master.csv                ← final comparison data for all models (2018–2023)
│   ├── final_data_2018.rds                              ← final data for 2018 (individual-level panel)
│   ├── final_data_2019.rds                              ← final data for 2019 (individual-level panel)
│   ├── final_data_2020.rds                              ← final data for 2020 (individual-level panel)
│   ├── final_data_2021.rds                              ← final data for 2021 (individual-level panel)
│   ├── final_data_2022.rds                              ← final data for 2022 (individual-level panel)
│   ├── final_data_2023.rds                              ← final data for 2023 (individual-level panel)
│   │
│   ├── mdcev/                             ← results from mdcev models rmdecv package
│   │   ├── mdcev_model_list.rds           ← list of MDCEV models (2018–2023) 
│   │   ├── welfare_2018.rds               ← 2018 welfare estimates                          
│   │   └── ................               ← repeat for 2019–2023 
│   │
│   ├── tc_constant/                              ← results from rerunning model constant VTT and D
│   │   ├── mdcev_cf_v2_2018_both_equal.rds       ← 2018 Distance and VTT constant  
│   │   ├── mdcev_cf_v2_2018_equal_distance.rds   ← 2018 Distance constant         
│   │   ├── mdcev_cf_v2_2018_equal_vot.rds        ← 2018 VTT constant                      
│   │   └── ................                      ← repeat for 2019–2023 
│   │
│   ├── satiation/                    ← results from rerunning mdcev changing gamma
│   │   ├── welfare_2018_x2.rds       ← doubling gamma for 2018
│   │   ├── ..................        ← repeat for 2019–2023 
│   │   ├── welfare_2018_x4.rds.      ← quadrupling gamma for 2018
│   │   └── ..................        ← repeat for 2019–2023 
│   │
│   └── rum/                           ← results from rum models
│       ├── model_list_52.rds          ← frequency RUM models (52wk_2018:52wk_2023)    
│       ├── kr_list_52.rds	           ← Krinsky-Robb confidence intervals for frequency RUM models
│       ├── welfare_rum_list.rds       ← welfare estimates for frequency RUM models
│       └── rum_flight_models_2018.rds ← flight RUM models (2018–2023)
│
├── figures/                           ← all saved plots (300 dpi PNG)
│   ├── Oahu.png,Hawaii.png,Molakai.png,Kauai.png   ← Figure 1: Island Campsite locations
│   ├── figure2_average.png                         ← Figure 2: Average welfare by year and model 
│   ├── figureb1average.png                         ← Appendix Figure B1: Average welfare by year and model for participants
│   ├── figure3_elasticities.png                    ← Figure 3: Elasticities by model and year
│   ├── figureb2_elasticities.png                   ← Appendix Figure B2: Elasticities by model and year with the satation parameter
│   ├── figure4_distributional.png                  ← Figure 4: Distributional welfare for 2018 & 2023
│   ├── figureb3_distributional.png                 ← Appendix Figure B3: Distributional welfare for 2019 to 2022 with the satation parameter
│   ├── figureb4_income.png                         ← Appendix Figure B4: Income distribution of participants
│   ├── figureb5rum_counterfactual.png              ← Appendix Figure B5: RUM counterfactuals
│   └── figureb6_mdcevcounterfactual.png            ← Appendix Figure B6: MDCEV counterfactuals
│
├── tables/                                  ← tables in html
│   ├── table_1_mdcev_2018_2023.html         ← table 1: MDCEV Results 2018 2023
│   ├── table_2_rum_2018_2023.html           ← table 2: RUM Results 2018 2023
│   ├── table_3_welfare_2018_2023.html       ← table 3: Compare MDCEV and RUM Results from table 1 & 2 
│   ├── table_B1_mdcev_2019_2020.html        ← table B1 MDCEV Results 2019 2020
│   ├── table_B2_mdcev_2021_2022.html        ← table B2 MDCEV Results 2021 2022
│   ├── table_B3_rum_2019_2022.html          ← table B3 RUM Results 2019 2022
│   ├── table_B4_welfare_2019_2020.html      ← table B4: Welfare Results 2019 2020 
│   ├── table_B5_welfare_2021_2022.html      ← table B5: Welfare Results 2021 2022
│   ├── tableb6_temporal_stability.html      ← table B6: Temporal Stability Results
│   ├── table_B7_gamma_2018_2019.html        ← table B6: MDCEV Results 2018 2019 with gamma = 2 & 4
│   ├── table_B8_gamma_2020_2021.html        ← table B7: MDCEV Results 2020 2021 with gamma = 2 & 4 
│   ├── table_B9_gamma_2022_2023.html        ← table B8: MDCEV Results 2022 2023 with gamma = 2 & 4
│   ├── table_B10_rum_flight_2018.html       ← table B9: RUM Flight Results 2018  
│   └── tableB.11_income.html                ← table B10: Income significantly different form top and bottom
│
├── _site/                            ← File is set up to have a github page 
│                                         There are many more files in this folder for the website
│                                          https://loweas.github.io/rec_res/
│   
│
└── index.qmd                            ← Quarto analysis files (run in order)
└── models.qmd    
└── README.md
└── rec_res.Rproj
```

## Main Files

### `index.qmd`
This main file creates all tables and figures in the paper. It pulls the data createad when running model.gmd. *Note running model.qmd can take a few days given it both runs all MDCEV simulations including satiation and how constant travel cost assumptions effect the simulations.*

Results are constructed from data in folder and provide comparision across all MDCEV and RUM.

---

### `model.qmd`

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
Estimates repeated Random Utility Model specifications for each year:

- **Frequency RUM (52-week):** creates 52 weekly choice occasions per individual. Stay-at-home IS chosen in non-trip weeks — coefficient identified at 5.90***.

use `mlogit(choice ~ price | 1)` via the `mlogit` and `dfidx` packages.

**Key outputs:**  `model_list_52`


Applies the log-sum welfare formula to each model's estimated coefficients:

$$CV_{it} = \frac{\ln \sum_{j \neq k} \exp(V_{ijt}) - \ln \sum_{j} \exp(V_{ijt})}{-\hat{\beta}_p}$$

Individual annual welfare = sum of per-trip CV across all observed trips. Welfare is calculated for each of the 22 campsite closures plus system closure. Results merged into `final_comparison_master` and `final_comparison_master1`.

**Key outputs:** `final_comparison_master`, 



## Data Sources

| Source | Description | Access |
|---|---|---|
| DOFAW reservation records | Campsite reservation data 2018–2023 | DOFAW administrative data d-identified |
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
