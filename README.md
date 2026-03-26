# CBMS Tabulation Script — Group 3

## Group Information

**Group Number:** 3

**Group Members:** 
- Shiela G. Sinconiegue
- Marie Beth A. Echavez
- Reyna Rose D. Bancale
- Jomari M. Daison



---

## Description of the Codebase

This project is an R-based data processing and tabulation pipeline built on top of the **Community-Based Monitoring System (CBMS)** dataset. It reads encrypted RCDF data files, performs area-level joins, and generates a structured Excel output with multiple tabulation sheets aligned with CBMS specifications.

The codebase analyzes the following key indicators:

1. **PWDs Living Alone (Table 1.1)** — Identifies persons with disabilities (PWDs) living in alone and household with pwd, with barangay-level counts and proportions.
2. **Water Access Status (Table 1.2)** — Classifies households by drinking water service level (Basic/Safely Managed vs. Limited/Unimproved) and computes access gaps per barangay.
3. **Unregistered Senior Citizens (Table 2.1)** — Counts seniors aged 60+ without a Senior Citizen ID, disaggregated by sex and barangay.
4. **PhilSys ID Demographics (Table 2.2)** — Analyzes PhilSys (national ID) registration rates by age group and sex.
5. **4Ps Exclusion Analysis (Table 3)** — Cross-references overcrowded households against 4Ps beneficiary status to identify uncovered vulnerable households per barangay.

The final output is an Excel workbook (`.xlsx`) saved to the configured output directory, with a table list and reference sheet included.

### File Structure

| File | Purpose |
|------|---------|
| `index.R` | Entry point — sources `core.R` to initialize the pipeline |
| `core.R` | Core setup — loads packages, reads config, loads and preprocesses RCDF data, then sources the main analysis script |
| `scripts/2024/official/main.R` | Main analysis script — defines all tabulations and generates the Excel output |
| `config.yaml` | Configuration file — specifies area codes, data paths, output directories, and script paths |
| `.env` | Environment file — stores private RSA key paths and passwords for RCDF decryption |

---

## Prerequisites

Make sure the following are installed before running the code:

- **R** (version 4.1 or higher recommended)
- **Rstudio**
- The following R packages:
  - `cli`, `dplyr`, `tidyr`, `stringr`, `openxlsx`, `yaml`, `glue`
  - `rcdf` — for reading encrypted RCDF data files
  - `phscs` — CBMS helper package
  - `tsg` — tabulation and output generation utilities

Install CRAN packages with:

```r
install.packages(c("cli",
        "dplyr",
        "tidyr",
        "rcdf",
        "phscs",
        "tsg",
        "stringr",
        "openxlsx",
        "yaml",
        "glue"))
```


---

## How to Run the Code

### Step 1 — Set Up the Environment File

Edit the `.env` file in the project root and replace the placeholder paths and passwords with the actual RSA key file paths and passphrases for your area:

```
PRIVATE_KEY_PATH_1001319="data/2024/RSA Keys/1001319-private-key.pem"
PRIVATE_KEY_PW_1001319="your_actual_password_here"
```

Add one `PRIVATE_KEY_PATH_<area_code>` and one `PRIVATE_KEY_PW_<area_code>` entry per area code listed in `config.yaml`.

### Step 2 — Configure `config.yaml`

Open `config.yaml` and verify the settings match your target area and data: (MODIFY IF NEEDED)

```yaml
cbms_round: "2024" 
area_code: "1001319"
area_name: "Sumilao, Bukidnon"
input_data_type: "rcdf"
aggregation:
  level: "barangay"   
path:
  input_data: ~       # Leave as ~ to use the default path convention
  output: "output"
  script: "scripts/2024/official/main.R"
```

### Step 3 — Place Input Data

By default, RCDF data files should be placed at:

```
data/2024/rcdf/<area_code> <area_name>.rcdf
```

For example:
```
data/2024/rcdf/1001319 Sumilao, Bukidnon.rcdf
```

If a custom path is needed, set `path.input_data` in `config.yaml`.

### Step 4 — Place the Reference File

The tabulation reference file must be present at:

```
references/2024-cbms-tabulation-specs2.xlsx
```

### Step 5 — Run the Script

Open R or RStudio, set the working directory to the project root, and run:

```r
source("index.R")
```

### Step 6 — Retrieve the Output

The generated Excel file will be saved in:

```
output/<cbms_round>/
```

The filename follows this pattern:

```
2024 CBMS Tabulation - Sumilao, Bukidnon (YYYY-MM-DD).xlsx
```

---

## Notes

- Do **not** share or commit the `.env` file — it contains sensitive private key credentials.
- The main analysis script (`main.R`) must not modify the code below the `#Don't change the codes after this comment` section.
