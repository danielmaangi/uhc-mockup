# Cancer Patients & SHA Chemotherapy Payments Dashboard — Feature Specification

## Summary

Implement Summary Cards, Monthly Trend Charts, and a County Comparison Table for the unique number of cancer patients and the amount SHA paid for chemotherapy services. Summary cards will display the unique count of patients treated within the last 3 months and new patients starting cancer treatment.

## Description

This task involves building a dashboard to visualise the number of cancer patients that SHA paid for their treatment and the associated amounts. Users need to see summaries, a temporal trend, and a detailed county-by-county breakdown.

### Data Source Guide

| Code | Intervention Name |
|---|---|
| SHA – 06 (21 – 33) | Hemato-Oncology Treatment |

**Patient Identifier:** Client Registry (CR) Number

---

## AC1 — Summary Cards

Display a top-level summary section containing the following KPIs. Each card must include a **delta indicator** showing the increase or decrease relative to the previous period.

| Card | Description |
|---|---|
| Unique cancer patients treated (last 3 months) | Aggregated unique count of patients treated within the last 3 months |
| New cancer patients treated this month | Aggregated count of patients appearing for the very first time in cancer treatment, based on the CR number |
| Amount paid by SHA | Sum of amounts paid for cancer treatment |

---

## AC2 — Monthly Trend Comparison (Dual-Line Charts)

### Chart 1 — Patient Volume Trends

| Line | Metric |
|---|---|
| Line 1 | Unique count of cancer patients that SHA paid for |
| Line 2 | New patients that SHA paid for |

**Tooltip (on hover):** Month, unique patient count, new patient count, proportion of new patients.

### Chart 2 — Payment Amount Trends

| Line | Metric |
|---|---|
| Line 1 | Total amount SHA paid for cancer treatment |
| Line 2 | Total amount SHA paid for new cancer patients |

**Tooltip (on hover):** Month, total amount, amount paid for new patients, proportion.

---

## AC3 — County Comparison Table

### Column Definitions

| Column | Description |
|---|---|
| County Name | Name of the county |
| Total Cancer Patients | Total unique cancer patients in the county |
| New Patients | Count of new cancer patients in the county |
| Amount SHA Paid | Total amount paid by SHA for cancer treatment in the county |

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
