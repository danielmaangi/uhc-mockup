# UHC Indicator Reference Guide

Interactive R Shiny application serving as the reference guide for UHC indicators — documenting definitions, methodologies, and data requirements for ERP claims dashboards.

## Indicators

| Indicator | Category | Access |
|-----------|----------|--------|
| Unpaid Claims in ERP | Claim Flow Analysis | SHA, DHA |
| Claims TAT | Claim Flow Analysis | SHA, DHA |
| Claims Ageing | Claim Flow Analysis | SHA, DHA |
| SHA Deliveries | Health Services | SHA, DHA |
| Cancer Patients & SHA Payments | Health Services | SHA, DHA |

### Unpaid Claims in ERP
Claims that have been approved and are payment-ready but not yet disbursed. Broken down by fund (SHIF, ECCIF, POMSF) with an Overall tab, drillable to county and provider level.

### Claims Turnaround Time (TAT)
Measures time from claim creation to payment date (TAT = Date Paid − Date Created). Tracks min, median, and max TAT alongside the share of claims settled within 30 and 90 days.

### Claims Ageing
Tracks outstanding approved-but-unpaid claims by age bucket, showing how long claims have been pending disbursement across funds and providers.

### SHA Deliveries
Monitors SHA-funded deliveries across counties and facilities, tracking volumes and payment amounts for maternity-related benefit packages.

### Cancer Patients & SHA Payments
Tracks SHA Hemato-Oncology (SHA-06, benefit codes 21–33) treatment payments. Patients are identified by CR Number. Covers unique patients, new patients, and total amounts paid by SHA.

## Running the App

**Prerequisites:** R ≥ 4.2, with [`renv`](https://rstudio.github.io/renv/) for dependency management.

```r
# Restore dependencies
renv::restore()

# Launch
shiny::runApp()
```

## Project Structure

```
app.R                  # Main app — navigation, layout, CSS/JS
R/
  helpers.R            # Shared formatting and UI helpers
  definitions.R        # Indicator definitions and metadata
  indicator_def.R      # Indicator definition UI module
  approved_unpaid.R    # Unpaid Claims in ERP indicator
  claims_tat.R         # Claims TAT indicator
  claims_ageing.R      # Claims Ageing indicator
  sha_deliveries.R     # SHA Deliveries indicator
  cancer_payments.R    # Cancer Payments indicator
definitions/           # Per-indicator QMD definition documents
requirements/          # Indicator specs
renv.lock              # Pinned package versions
```

## Data

All data is randomly generated seed-based mock data. No real patient or claims data is used.
