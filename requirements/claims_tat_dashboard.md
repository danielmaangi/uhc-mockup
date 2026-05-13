# Claims Turnaround Time (TAT) Dashboard — Feature Specification

## Summary

Implement Summary Cards, Monthly Trend Charts, and a County Comparison Table for claims turnaround time. Summary cards will display total number of claims, number of paid claims, minimum TAT, median TAT, maximum TAT, % paid within 30 days, % paid within 90 days, and % paid after 90 days.

## Description

This task involves building a dashboard to visualise the duration taken to settle claims (turnaround time). Dashboard users need to see summaries, a temporal trend, and a detailed county-by-county breakdown.

### TAT Calculation

```
TAT = Date Paid – Date Created  (result in days)
```

---

## AC1 — Summary Cards

Display a top-level summary section containing the following KPIs. Each card must include a **delta indicator** showing the increase or decrease relative to the previous period.

| Card | Description |
|---|---|
| Number of Claims | Total count of claims |
| Minimum TAT | Shortest turnaround time across all claims |
| Median TAT | Median turnaround time across all claims |
| Maximum TAT | Longest turnaround time across all claims |
| % Paid within 30 days | Share of claims settled within 30 days |
| % Paid within 90 days | Share of claims settled within 90 days |
| % Paid after 90 days | Share of claims settled after 90 days |

---

## AC2 — Monthly Trend Comparison (Dual-Line Charts)

### Chart 1 — TAT Trends

| Line | Metric |
|---|---|
| Line 1 | Minimum TAT trend |
| Line 2 | Median TAT trend |
| Line 3 | Maximum TAT trend |

**Tooltip (on hover):** Month, claims count, respective TAT value.

### Chart 2 — Payment Period Distribution Trends

| Line | Metric |
|---|---|
| Line 1 | % of claims paid within 30 days |
| Line 2 | % of claims paid within 90 days |
| Line 3 | % of claims paid after 90 days |

**Tooltip (on hover):** Month, % of claims paid for the respective period.

---

## AC3 — County Comparison Table

### Column Definitions

| Column | Description |
|---|---|
| County Name | Name of the county |
| Number of Claims | Total claims count for the county |
| Minimum TAT | Shortest TAT for the county |
| Median TAT | Median TAT for the county |
| Maximum TAT | Longest TAT for the county |
| % Paid within 30 days | Share of claims settled within 30 days |
| % Paid within 90 days | Share of claims settled within 90 days |
| % Paid after 90 days | Share of claims settled after 90 days |

### Table Behaviour

- All columns are sortable
- Table reacts to global filters

---

## AC4 — Global Filters

The following filters apply across all components (summary cards, charts, and table):

| Filter | Description |
|---|---|
| Created Date | Date range filter based on claim creation date |
| County | Filter by county |
| Level | Filter by facility level |
| Facility | Filter by specific facility |
