# SHA Deliveries Dashboard — Functional Specification

## Summary

Implement Summary Cards, Monthly Trend Charts, and County Comparison Table for the proportion of deliveries paid by SHA. Users need to see summaries, a temporal trend to identify coverage gaps, and a detailed county-by-county breakdown with variance indicators.

---

## Data Sources

### SHA Claims System

| Code | Intervention Name |
|---|---|
| `SHA-08-005` | Vaginal delivery |
| `SHA-08-006` | Caesarean section |
| `SHA-08-007` | Multiple births delivery |

### KHIS (DHIS2)

Source: [https://hiskenya.dha.go.ke/](https://hiskenya.dha.go.ke/)

| Indicator Name | Indicator ID |
|---|---|
| Total deliveries | `BDsWWkJHuce` |

### Join Logic

- Join on **county** and **created date** matched to KHIS period.
- Pull data by **month** from DHIS2.
- Include counties with zero values from either source.

---

## Global Filters

The following filters apply across all dashboard sections. All components must react to filter changes.

| Filter | Description |
|---|---|
| **Created date** | Date range filter; matched to KHIS period on join |
| **County** | Single or multi-select county filter |

---

## Section 1 — National Summary Cards

Display a top-level summary section containing three primary KPI cards.

| Card | Metric | Definition |
|---|---|---|
| **Total Deliveries (D)** | Aggregated count | Sum of KHIS total deliveries across all counties in scope |
| **Total SHA Claimed Deliveries (N)** | Aggregated count | Sum of SHA claimed deliveries (codes SHA-08-005, SHA-08-006, SHA-08-007) |
| **Proportion** | Percentage | N ÷ D, expressed as a percentage |

---

## Section 2 — Monthly Trend Comparison

A dual-line chart showing the national monthly trend for total deliveries versus SHA claimed deliveries, with a shaded gap area between the two lines.

### Chart Configuration

| Property | Specification |
|---|---|
| **Chart type** | Dual-line chart |
| **X-axis** | Month (chronological order) |
| **Y-axis** | Delivery count; starts at 0 |
| **Line 1** | Total deliveries (KHIS) |
| **Line 2** | SHA claimed deliveries |
| **Area fill** | Neutral shaded area rendered between the two lines to highlight the gap |

### Tooltips

On hover over any data point, display:

- Month
- Total delivery count
- SHA claimed count
- Proportion (%)
- Variance (absolute difference between total and SHA count)

### Data Integrity — Outlier Handling

KHIS data is subject to reporting anomalies. Outliers are detected and replaced as follows:

1. **Detection method:** Interquartile Range (IQR).
   - Compute Q1 and Q3 for the series.
   - Lower fence = Q1 − 1.5 × IQR
   - Upper fence = Q3 + 1.5 × IQR
   - Any data point falling outside the fences is flagged as an outlier.
2. **Replacement:** Flagged outliers are replaced with the nearest fence value (lower or upper, whichever is closer to the original value).
3. Outlier replacement applies to KHIS data only and is performed before rendering and before proportion calculations.

---

## Section 3 — County Comparison Table

A sortable table showing the delivery coverage profile for each county.

### Column Definitions

| Column | Description |
|---|---|
| **County Name** | Name of the county |
| **KHIS Delivery Count** | Total deliveries from KHIS for the county |
| **SHA Claimed Count** | Total SHA claimed deliveries for the county |
| **Proportion** | SHA Claimed Count ÷ KHIS Delivery Count, expressed as a percentage |

### Data Handling

- Include all counties, including those with zero values in either source.
- Where KHIS Delivery Count is zero, Proportion should display as `—` (not a division-by-zero error).

### Default Sort

Default sort order is **Proportion ascending** (lowest coverage counties first).

### Sorting

- All columns are sortable by clicking the column header.
- Sort direction toggles between ascending and descending on successive clicks.
- Table reacts to global date and county filters.

### Conditional Formatting — Proportion Column

| Indicator | Condition | Meaning |
|---|---|---|
| 🔴 Red | Proportion < 50% | Low coverage — requires attention |
| 🟡 Amber | Proportion 50%–79% | Moderate coverage — monitor closely |
| 🟢 Green | Proportion ≥ 80% | Adequate coverage |
