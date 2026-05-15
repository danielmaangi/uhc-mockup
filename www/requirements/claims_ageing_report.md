# Claims Ageing Report — Functional Specification

## Overview

The claims ageing report shows how long live claims have been in the system without payment being confirmed. It covers all claims that have not been terminated or confirmed as paid, giving SHA a full picture of the outstanding backlog across the entire claims lifecycle.

The primary purpose is **prioritisation**: operations teams and SHA leadership can identify stalled claims, detect bottlenecks, and escalate where necessary. Ageing is calculated from the date the claim was created in the system to today's date.

---

## AC1 — Claim Scope and Exclusion Logic

Determining which claims appear in the ageing report is a two-step process.

### Step 1 — Exclude Terminal and Provider-Side Claims

The following statuses are excluded outright. These claims are either closed (no further action expected) or currently with the provider rather than SHA.

| Status | Reason for Exclusion |
|---|---|
| `REJECTED` | Claim denied — terminal |
| `DECLINE` | Claim denied — terminal (legacy status) |
| `CANCELED` | Claim withdrawn — terminal |
| `SENT_BACK` | Returned to provider — awaiting provider action |
| `MISSING_DOCUMENTS` | Awaiting documents from provider |
| `PAYMENT_PARTIAL` | Excluded pending resolution of partial payment model |

### Step 2 — Check ERP Payment Status

For all remaining claims (those not excluded in Step 1), the ERP is queried by claim ID to determine whether payment has been confirmed. The ERP is the source of truth for payment status due to potential lag between ERP processing and claims system status updates.

| ERP Lookup Result | Action |
|---|---|
| ERP record found, status = `PAID` | Exclude from ageing — payment confirmed |
| ERP record found, status = `UNPAID` | Include in ageing — in pipeline but not yet settled |
| No ERP record found | Include in ageing — claim has not yet reached ERP |

---

## AC2 — Ageing Buckets

### Ageing Calculation

For all claims that pass the two-step inclusion check, ageing is calculated as follows:

> **Age (days) = Today's date − Claim created date**

- **Created date** is the timestamp the claim was first entered into the system. It never changes regardless of subsequent status transitions.
- Age is always calculated relative to today's date.
- Age is expressed in **whole days** (floor, not rounded).

### Bucket Definitions

All live claims are grouped into four mutually exclusive ageing buckets:

| Bucket | Definition | Implication |
|---|---|---|
| **0–30 days** | Created within the last 30 days | Normal processing window |
| **31–60 days** | Created 31–60 days ago | Approaching threshold; monitor closely |
| **61–90 days** | Created 61–90 days ago | Overdue; requires follow-up action |
| **90+ days** | Created more than 90 days ago | Critically stalled; escalation required |

---

## Section 1 — Ageing Bucket Cards

### Layout

Display four cards in a single horizontal row, one per ageing bucket, spanning the full width of the dashboard. Cards are equal width.

### Card Content

| Element | Specification |
|---|---|
| **Primary display** | Total value of live claims in this bucket (KES), formatted with thousand separators (e.g. KES 4,210,000) |
| **Bucket label** | Bucket range shown as the card title: 0–30 days, 31–60 days, 61–90 days, 90+ days |
| **Hover / tooltip** | On hover, display the count of claims in this bucket (e.g. 142 claims) |
| **Colour** | Each card is coloured according to the bucket colour convention: green, amber, orange, red |
| **Active state** | When a global filter is applied, card values update to reflect the filtered dataset |

---

## Section 2 — Ageing Bar Charts

### Layout

Display two bar charts side by side on the same row, each occupying 50% of the dashboard width. Both charts share the same x-axis structure (four ageing buckets) and the same colour scheme, but measure different metrics.

### Chart 1 — Claim Count by Bucket

| Property | Specification |
|---|---|
| **Title** | Number of claims by ageing bucket |
| **X-axis** | Four ageing buckets: 0–30, 31–60, 61–90, 90+ |
| **Y-axis** | Claim count; labelled with whole numbers; axis starts at 0 |
| **Bar colour** | Each bar uses the bucket colour (green, amber, orange, red) |
| **Bar labels** | Claim count displayed above each bar |
| **Tooltip on hover** | Bucket range, claim count, and percentage share of total live claims |

### Chart 2 — Total Value by Bucket

| Property | Specification |
|---|---|
| **Title** | Total value (KES) by ageing bucket |
| **X-axis** | Four ageing buckets: 0–30, 31–60, 61–90, 90+ |
| **Y-axis** | Total value in KES; formatted in millions (e.g. 4.2M) for readability; axis starts at 0 |
| **Bar colour** | Each bar uses the bucket colour (green, amber, orange, red) |
| **Bar labels** | KES value displayed above each bar (abbreviated, e.g. KES 4.2M) |
| **Tooltip on hover** | Bucket range, full KES value, and percentage share of total live claims value |

---

## Section 3 — County Ageing Table

### Purpose

A sortable, expandable table showing the ageing profile of live claims grouped by county. Each row represents one county. Expanding a county row reveals a provider-level breakdown within that county, using the same columns.

### Column Definitions

| Column | Description |
|---|---|
| **County** | Name of the county |
| **Total claims** | Count of all live claims in the county |
| **Total value (KES)** | Sum of claim values for the county |
| **0–30 days** | Count of live claims in the 0–30 day bucket |
| **31–60 days** | Count of live claims in the 31–60 day bucket |
| **61–90 days** | Count of live claims in the 61–90 day bucket |
| **90+ days** | Count of live claims in the 90+ day bucket |

### Sorting

- Every column is sortable by clicking the column header.
- Default sort order is **total claims descending** (highest backlog county first).
- Sort direction toggles between descending and ascending on successive clicks.
- When a county row is expanded, the provider sub-rows sort independently within that county by the same column that is active at the county level.

### Expandable County Rows

- Each county row has an expand control (e.g. chevron icon) on the left.
- Clicking the expand control reveals **provider sub-rows** indented beneath the county row.
- Provider sub-rows use the same columns as the county row: provider name, total claims, total value (KES), and one count column per ageing bucket.
- Multiple counties can be expanded simultaneously.
- Collapsing a county row hides the provider sub-rows but retains the expanded state if filters change.
