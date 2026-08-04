# Ambulance Claims — Indicators Functional Specification

## Overview

The Ambulance Claims dashboard gives operations and leadership a quick sanity check on the ambulance/EMS claims book: does the submitted amount reconcile to what's been paid, rejected, and still outstanding; how does the rejection rate look by count vs by value; how fast are paid claims actually settling once outliers are accounted for; and how old is the pending backlog.

Every claim carries one of **four pipeline statuses**:

| Status | Meaning | Reconciliation leg |
|---|---|---|
| `UNDER_REVIEW` | Being adjudicated — no decision yet | Outstanding |
| `RETURNED` | Sent back to vendor pending more info — can re-enter the pipeline | Outstanding |
| `REJECTED` | Denied — terminal | Rejected |
| `PAID` | ERP-confirmed disbursement — terminal | Paid |

**Submitted is not a fifth status.** No claim record ever has `status = SUBMITTED` — every filed claim is already at minimum `UNDER_REVIEW`. "Submitted" is a derived total: the sum of all four statuses above, i.e. every claim that has ever been filed.

> **Total Submitted = Paid + Rejected + Outstanding**
> **Outstanding = Under Review + Returned**
> **Total Submitted = Under Review + Returned + Rejected + Paid**

This identity holds at every level of aggregation shown on the dashboard — the overall figure, and every vendor/county row — since Outstanding, Rejected and Paid are mutually exclusive and exhaustive by construction. It does not automatically hold within an arbitrary *filtered* view (e.g. filtering to a single status), since a filter narrows which claims are counted in the first place; the identity is a property of the full claims population at whatever grouping level you're looking at (overall, vendor, or county), not a promise about every possible filter combination.

---

## AC1 — KPI Cards

### Count — one card per status

Five cards: **Submitted, Under Review, Returned, Rejected, Paid** — each showing the **claim count**. Submitted's value is always the sum of the other four (it is a total, not a peer category). The Rejected card additionally carries a **% of claims** rate badge.

### Amount — one card per status

The same five labels again, this time showing the **claim value (KES)** for each. Submitted is again the sum of the other four. The Rejected card additionally carries a **% of value** rate badge.

Showing both Count and Amount for every status is what lets a reviewer see rejection-rate divergence directly: the Rejected card's share of the Count section vs its share of the Amount section are expected to differ — a small number of high-value claims (e.g. long-distance inter-facility transfers) can move % of value independently of % of count. The mock data intentionally biases rejected claims toward higher average value to demonstrate this.

**Definitions footnote** (rendered as an info banner directly under the filter bar): states the reconciliation identity and clarifies that it holds at every aggregation level shown on the page (overall, by vendor, by county) — not a claim that any arbitrary filtered view will tie out, since this is a static mockup and the filter bar does not currently recompute the figures. **Unique Patients** and **Unique Vendors** are surfaced here as prose, not as separate KPI cards, since they aren't part of the Count/Amount status breakdown.

---

## AC2 — Monthly Trend

### Layout

One combo chart: bar series for claims volume, line series (secondary y-axis) for claims value (KES), 12-month x-axis.

### Toggles

Two independent local toggles above the chart, both client-side dataset swaps (no page reload), and they combine — any status can be viewed against either date field:

- **Status** pill toggle: Submitted / Under Review / Returned / Rejected / Paid. Submitted (the default) shows the total; the other four show that status's own monthly volume/value.
- **Created Date / Incurred Date** pill toggle: reflects that the same claim can look like it was filed in a different month depending on which date drives the view.

---

## AC3 — Turnaround Time (Submission → Payment)

Positioned directly below the Monthly Trend chart.

| Card | Value | Note |
|---|---|---|
| Average TAT | Mean days, submission to payment | Sensitive to outliers |
| Median TAT | Median days, submission to payment | Flagged as the preferred operational benchmark |
| Minimum TAT | Fastest submission-to-payment turnaround observed | — |
| Maximum TAT | Slowest submission-to-payment turnaround observed | Flags the worst-case outlier |

**Only claims with status `PAID` are included** — TAT is submission date to payment date, so a claim needs an actual payment date to have a TAT. `REJECTED` claims are never paid and so never get one; `UNDER_REVIEW` and `RETURNED` claims haven't reached a decision yet either way.

---

## AC4 — Aging of Pending Claims

### Scope

Only **Outstanding** claims (Under Review + Returned) are aged. Paid and Rejected claims are resolved and excluded.

### Bucket Definitions

| Bucket | Definition |
|---|---|
| **0–7 days** | Pending for a week or less — normal dispatch-to-decision window |
| **8–30 days** | Approaching threshold; monitor |
| **31–60 days** | Overdue; requires follow-up |
| **60+ days** | Critically stalled; escalation required |

More granular near-term than other claim-type ageing reports (which use 0–30/31–60/61–90/90+), since ambulance turnaround is expected to be fast.

### Toggle — real recomputation, not a filter

A **Created Date / Incurred Date** pill toggle recomputes which bucket each pending claim falls into. Incurred-date buckets skew older than created-date buckets for the same population, because the incident date always precedes (or equals) the date the claim was filed — this is the whole point of offering both views: regulatory/reporting bodies tend to care about incurred-date ageing (service delivery), while ops teams track created-date ageing (submission pipeline).

### Charts

Two bar charts side by side: claim count by bucket, and claim value (KES) by bucket. Same four buckets, same colour convention (green → amber → orange → red).

---

## AC5 — Vendor / County Breakdown Table

### Layout

One table, toggled between **By Vendor** and **By County** groupings via pill tabs (same pattern as the fund-type pills on the Unpaid Claims page).

### Columns

| Column | Description |
|---|---|
| Vendor / County | Group name |
| Volume | Total claim count in the group (= Submitted for that group) |
| Value (KES) | Total claim value in the group |
| Rejection Rate (% Claims) | Rejected count ÷ total count |
| Rejection Rate (% Value) | Rejected value ÷ total value |
| Avg TAT | Average submission-to-payment days for paid claims in the group |

### Sorting

Every column is sortable by clicking the header (reuses the shared `.sortable-col` handler already wired up in `app.R`). Default sort: Volume descending.

---

## AC6 — Global Filters

| Filter | Input type | Field |
|---|---|---|
| Created Date | Single-select (month) | `date_created` |
| Incurred Date | Single-select (month) | `date_incurred` |
| County | Multi-select | `county` |
| Vendor | Multi-select | `vendor` |

Status is **not** a global filter — it's surfaced instead as the Monthly Trend chart's status toggle (AC2), since that's the one place on the page a status breakdown over time is actually useful.

---

## Appendix: Business Rules & Data Dictionary

### Status → Reconciliation Leg

| Raw Status | Reconciliation Leg | Notes |
|---|---|---|
| `UNDER_REVIEW` | Outstanding | Being adjudicated, no decision yet |
| `RETURNED` | Outstanding | Not terminal — can re-enter the pipeline |
| `REJECTED` | Rejected | Terminal |
| `PAID` | Paid | Terminal — ERP-confirmed disbursement |
| *(Submitted)* | *n/a — derived total* | Sum of all four rows above; not a status a claim record carries |

### Colour Palette

| Category | Hex | Use |
|---|---|---|
| Submitted (total) | `#0f172a` | Slate — visually distinct from the four real statuses below |
| Under Review | `#f59e0b` | Amber |
| Returned | `#f97316` | Orange |
| Rejected | `#dc2626` | Red |
| Paid | `#16a34a` | Green |
| Age bucket 0–7 days | `#22c55e` | Green |
| Age bucket 8–30 days | `#f59e0b` | Amber |
| Age bucket 31–60 days | `#f97316` | Orange |
| Age bucket 60+ days | `#ef4444` | Red |

### Data Model

- **Raw grain**: one row = one ambulance claim (joined: Payor System submission/status/vendor/patient/county × ERP payment confirmation, including `date_paid` where the claim has been paid).
- **Modelled grain**: one row = one vendor, aggregated claim counts/values by status, rejection rates, and average TAT for the selected period.

> **Note:** All figures in this mockup are synthetic (`set.seed()`-based), constructed so the reconciliation identity holds exactly and the vendor/county rollups sum to the overall totals. Replace with a live query against the claims mart once available.
