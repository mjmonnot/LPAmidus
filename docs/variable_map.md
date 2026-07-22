# Paper-facing variable map

Construct → MIDUS variable names as used in the preprint Method.  
This is a **transparency map**, not a full analysis codebook or item-level MIDI dictionary.

**Keys**

| Sample | Person ID | Notes |
|---|---|---|
| Core (MIDUS 1–3) | `M2ID` | Merge person-level on `M2ID` (never row order) |
| Core family | `M2FAMNUM` | Clustering (twins/siblings) |
| Refresher | `MRID` | Independent sample; **not** merged into the core panel |

Wave prefixes follow MIDUS conventions (`A1` = MIDUS 1, `B1` = MIDUS 2, `C1` = MIDUS 3, `RA1` = Refresher).

---

## 1. Latent profile indicators (MIDI personality scales)

Official scale scores (not item-level adjectives).

| Construct | MIDUS 1 | MIDUS 2 | MIDUS 3 | Refresher |
|---|---|---|---|---|
| Neuroticism | `A1SNEURO` | `B1SNEURO` | `C1SNEURO` | `RA1SNEURO` |
| Extraversion | `A1SEXTRA` | `B1SEXTRA` | `C1SEXTRA` | `RA1SEXTRA` |
| Openness | `A1SOPEN` | `B1SOPEN` | `C1SOPEN` | `RA1SOPEN` |
| Agreeableness | `A1SAGREE` | `B1SAGREE` | `C1SAGREE` | `RA1SAGREE` |
| Conscientiousness | `A1SCONS` | `B1SCONS1` | `C1SCONS1` | `RA1SCONS1` |
| Agency | `A1SAGENC` | `B1SAGENC` | `C1SAGENC` | `RA1SAGENC` |

**Note:** Cross-wave conscientiousness uses the M1-parallel `CONS1` forms at MIDUS 2/3/Refresher for comparability with MIDUS 1 (`A1SCONS`).

---

## 2. Work attitudes and roles

| Construct | MIDUS 1 | MIDUS 2 | MIDUS 3 | Refresher |
|---|---|---|---|---|
| Job satisfaction (single-item) | `A1SI2` | `B1SF1` | `C1SF1` | `RA1SG1` |
| Job demands | `A1SJCDS` | `B1SJCDS` | `C1SJCDS` | `RA1SJCDS` |
| Skill discretion | `A1SJCSD` | `B1SJCSD` | `C1SJCSD` | `RA1SJCSD` |
| Decision authority | `A1SJCDA` | `B1SJCDA` | `C1SJCDA` | `RA1SJCDA` |
| Coworker support | `A1SJCCS` | `B1SJCCS` | `C1SJCCS` | `RA1SJCCS` |
| Supervisor support | `A1SJCSS` | `B1SJCSS` | `C1SJCSS` | `RA1SJCSS` |
| Negative work→family spillover | `A1SNEGWF` | `B1SNEGWF` | `C1SNEGWF` | `RA1SNEGWF` |
| Supervises others | `A1PB8_2` | `B1PB7` | `C1PB7` | `RA1PB7` |
| Number supervised | `A1PB8_3` | `B1PB7A` | `C1PB7A` | `RA1PB7A` |
| Occupation code | `A1POCC` | `B1POCC` (`B1PSOC` also available) | `C1POCC` | `RA1POCC` |

Coworker/supervisor support can be structurally missing when respondents have no coworkers/supervisor.

---

## 3. Economic and prestige criteria

| Construct | MIDUS 1 | MIDUS 2 | MIDUS 3 | Refresher |
|---|---|---|---|---|
| Personal income | `A1SJ8M` | `B1SRINC1` | `C1SRINC` | `RA1SRINC` |
| Household income | `A1SHHTOT` | `B1STINC1` | `C1STINC` | `RA1STINC` |

Occupational prestige / SEI scores are **derived** from occupation codes via published GSS crosswalks (details in preprint Method). Source occupation variables are listed above.

---

## 4. Psychological well-being and SDT-related scales

Ryff psychological well-being short forms (and related control/mastery scales) as used in the paper:

| Construct | MIDUS 1 | MIDUS 2 | MIDUS 3 | Refresher |
|---|---|---|---|---|
| Purpose in life | `A1SPWBU` | `B1SPWBU1` | `C1SPWBU1` | `RA1SPWBU1` |
| Autonomy | `A1SPWBA` | `B1SPWBA1` | `C1SPWBA1` | `RA1SPWBA1` |
| Positive relations (relatedness) | `A1SPWBR` | `B1SPWBR1` | `C1SPWBR1` | `RA1SPWBR1` |
| Environmental mastery | `A1SPWBE` | `B1SPWBE1` | `C1SPWBE1` | `RA1SPWBE1` |
| Self-acceptance | `A1SPWBS` | `B1SPWBS1` | `C1SPWBS1` | `RA1SPWBS1` |
| Personal growth | `A1SPWBG` | `B1SPWBG1` | `C1SPWBG1` | `RA1SPWBG1` |
| Generativity | `A1SGENER` | `B1SGENER` | `C1SGENER` | `RA1SGENER` |
| Personal mastery | `A1SMASTE` | `B1SMASTE` | `C1SMASTE` | `RA1SMASTE` |
| Perceived constraints | `A1SCONST` | `B1SCONST` | `C1SCONST` | `RA1SCONST` |
| Sense of control | `A1SCTRL` | `B1SCTRL` | `C1SCTRL` | `RA1SCTRL` |
| Positive reappraisal | `A1SREAPP` | `B1SREAPP` | `C1SREAPP` | `RA1SREAPP` |

---

## 5. Cognition (BTACT; Cognitive Project)

Analytic cognitive composites are taken from the MIDUS Cognitive Project deposits and constructed as described in the preprint (national-sample *z* / recommended longitudinal metrics). Primary ICPSR studies:

| Wave | ICPSR study | Example official composites |
|---|---|---|
| MIDUS 2 | [25281](https://doi.org/10.3886/ICPSR25281) | e.g. `B3TCOMPZ1`, `B3TEMZ1`, `B3TEFZ1` |
| MIDUS 3 | [37095](https://doi.org/10.3886/ICPSR37095) | e.g. `C3TCOMP`, `C3TEM`, `C3TEF` |
| Refresher | [37081](https://doi.org/10.3886/ICPSR37081) | e.g. `RA3TCOMPZ1`, `RA3TEMZ1`, `RA3TEFCZ1` |

---

## 6. Covariates and employment screening

| Construct | MIDUS 1 | MIDUS 2 | MIDUS 3 | Refresher |
|---|---|---|---|---|
| Age | `A1PAGE_M2` | `B1PAGE_M2` | `C1PRAGE` | `RA1PRAGE` |
| Sex | `A1PRSEX` | `B1PRSEX` | `C1PRSEX` | `RA1PRSEX` |
| Education | `A1PB1` | `B1PB1` | `C1PB1` | `RA1PB1` |
| Race | `A1SS7R` | `B1PF7A` | `C1PF7A` | `RA1PF7A` |
| Marital status | `A1PB17` | `B1PB19` | `C1PB19` | `RA1PB19` |
| Currently working | `A1PB3A` | `B1PB3A` | `C1PB3A` | `RA1PB3A` |
| Self-employed | `A1PB3B` | `B1PB3B` | `C1PB3B` | `RA1PB3B` |
| Sample design | `SAMPLMAJ` | `SAMPLMAJ` | `SAMPLMAJ` | — |

Survey weights are wave-specific (e.g. `A1SWGHT6`, `B1PWGHT10`, `C1PWGHT10`, `RA1SWGHT6`); see MIDUS documentation and preprint Method for which weight was applied in each analysis.

---

## 7. Daily diary / event-sampled corroboration

Diary analyses use the daily stress / diary projects listed in the README (ICPSR 26841, 37083, 38529), linked on the appropriate person IDs. Variable-level diary batteries are documented in those ICPSR codebooks and in the preprint Method; they are omitted here to keep this map limited to the main panel constructs.

---

## What is intentionally omitted

- Item-level MIDI adjective lists  
- Full missing-code registries and cleaning scripts  
- Derived analysis-file column names from the private pipeline  
- Exploratory constructs not retained in the preprint  

For verification beyond this map, contact the author (see README).
