# UHC Mockups — Phase 1

Interactive R Shiny mockups for communicating ERP claims dashboard requirements to developers.

## Indicators

| Indicator | Category | Access |
|-----------|----------|--------|
| Unpaid Claims in ERP | Claim Flow Analysis | SHA, DHA |
| Claims TAT | Claim Flow Analysis | SHA, DHA |
| Cancer Patients & SHA Payments | Health Services | SHA, DHA |

### Unpaid Claims in ERP
Claims that have been approved and are payment-ready but not yet disbursed. Broken down by fund (SHIF, ECCIF, POMSF) with an Overall tab, drillable to county and provider level.

### Claims Turnaround Time (TAT)
Measures time from claim creation to payment date (TAT = Date Paid − Date Created). Tracks min, median, and max TAT alongside the share of claims settled within 30 and 90 days.

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
  approved_unpaid.R    # Unpaid Claims in ERP indicator
  claims_tat.R         # Claims TAT indicator
  cancer_payments.R    # Cancer Payments indicator
requirements/          # Indicator specs
renv.lock              # Pinned package versions
```

## Data

All data is randomly generated seed-based mock data. No real patient or claims data is used.
