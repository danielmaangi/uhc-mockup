# Claims Under Review
**Breakdown by Review Stage · SHA & DHA View**

*Claims Analytics Team · Generated: 24 May 2026*

---

## Contents

1. [AC1 · Summary Cards — Under Review](#ac1--summary-cards--under-review)
2. [AC1 · Medical & Clinical Review — Detail Charts](#ac1--medical--clinical-review--detail-charts)
3. [AC2 · Monthly Trend — Medical & Clinical Review](#ac2--monthly-trend--medical--clinical-review)
4. [AC3 · Status-Level Breakdown Table](#ac3--status-level-breakdown-table)
5. [Appendix: Business Rules & Data Dictionary](#appendix-business-rules--data-dictionary)

---

## AC1 · Summary Cards — Under Review

> All statuses classified as Under Review · `DECLINE` reclassified from Rejected → Under Review

### Reference Figures (from reconciliation sheet)

| Review Category | Claims | Value (KES) |
|---|---|---|
| **Total Under Review** | **2,605,788** | **44,106,156,472** |
| Medical Review *(DHA CEO)* | 113,976 | 7,169,668,077 |
| Clinical Review *(DHA CEO)* | 101,816 | 614,248,915 |
| Manual Review | 355,808 | 7,090,139,853 |
| Other Under Review | 2,034,188 | 29,232,099,627 |

**Backend spec — Summary Cards:**

Under Review is the union of these statuses:
`CLINICAL_REVIEW` + `MANUAL_REVIEW` + `MEDICAL_REVIEW` + `APPROVED` + `PAYMENT_PARTIAL` + `QUEUED` + `RESUBMITTED_MISSING_DOCUMENTS` + `SENT_FOR_PAYMENT_PROCESSING` + `SENT_TO_SURVEILLANCE` + `DECLINE`

> **⚠ Change from current:** `DECLINE` was previously classified as **Rejected**.
> Must update `dashboard_status` derivation in `materialized_views.claim_flow_mart_v3_agg_table` so `DECLINE` maps to `Under_Review`.
>
> Cards 3 & 4 (Medical Review, Clinical Review) are the DHA CEO-requested metrics. Both SHA and DHA users see all four cards — no role-based column hiding.

---

## AC1 · Medical & Clinical Review — Detail Charts

> DHA CEO requested metrics · accessible to both SHA and DHA users

**Frontend spec:**

- Layout: Two bar charts side by side (50% each)
- Chart 1: Claim **count** for Medical vs Clinical Review (+ other categories for context)
- Chart 2: Claim **value** (KES B) for Medical vs Clinical Review (+ other categories)
- Bar colours:
  - Medical Review → Blue `#1A56DB`
  - Clinical Review → Purple `#7E3AF2`
  - Manual Review → Amber `#D97706`
  - Other Under Review → Gray `#6B7280`
- Highlight: Medical and Clinical bars at full opacity; Manual and Other at 60% opacity
- Tooltip: Category | Count/Value | % of total Under Review
- Both charts react to global filters

---

## AC2 · Monthly Trend — Medical & Clinical Review

Claims counts and values over time, with Medical and Clinical Review as the primary series.

### Claim Count Trend

**Frontend spec:**

- One line per review category
- Medical (blue) and Clinical (purple): solid lines, larger markers
- Manual Review and Other: thinner, more muted lines
- Tooltip: Month | Category | Count | % of that month's Under Review total
- Time range: Jan – Dec 2024 (wired to `input$period` date range filter)

### Claim Value Trend

**Frontend spec:**

- Same structure as count trend; y-axis in KES Billions
- Tooltip: Month | Category | Value (KES) | % of that month's total value

---

## AC3 · Status-Level Breakdown Table

Full breakdown of all statuses that make up Under Review, including the reclassified `DECLINE`.

### Status Totals

| Status | Review Category | Claims | Value (KES) | % Count | % Value |
|---|---|---|---|---|---|
| SENT_FOR_PAYMENT_PROCESSING | Other Under Review | 1,582,489 | 20,530,246,498 | 60.7% | 46.5% |
| MANUAL_REVIEW | Manual Review | 355,808 | 7,090,139,853 | 13.7% | 16.1% |
| SENT_TO_SURVEILLANCE | Other Under Review | 143,414 | 3,114,525,417 | 5.5% | 7.1% |
| DECLINE *(↑ reclassified from Rejected)* | Other Under Review | 129,945 | 3,539,952,039 | 5.0% | 8.0% |
| MEDICAL_REVIEW | Medical Review | 113,976 | 7,169,668,077 | 4.4% | 16.3% |
| CLINICAL_REVIEW | Clinical Review | 101,816 | 614,248,915 | 3.9% | 1.4% |
| APPROVED | Other Under Review | 171,495 | 1,358,747,406 | 6.6% | 3.1% |
| RESUBMITTED_MISSING_DOCUMENTS | Other Under Review | 5,129 | 568,687,828 | 0.2% | 1.3% |
| PAYMENT_PARTIAL | Other Under Review | 1,567 | 118,066,717 | 0.1% | 0.3% |
| QUEUED | Other Under Review | 149 | 2,073,722 | <0.1% | <0.1% |

**Frontend spec:**

- Sortable by all columns (click header); default sort: Claims descending
- `Review Category` column: colour-coded badge matching card colours
- `DECLINE` row: annotated to signal reclassification from Rejected
- `% Count` conditional highlighting: ≥20% → blue; ≥10% → amber
- Table reacts to all global filters

**Backend spec:**

```sql
-- Source
SELECT * FROM materialized_views.claim_flow_mart_v3_agg_table
WHERE dashboard_status = 'Under_Review'
-- NOTE: requires DECLINE → Under_Review reclassification applied in the view
```

---

## Appendix: Business Rules & Data Dictionary

### Under Review — Status Classification

| Raw Status | Review Category | Change from current? |
|---|---|---|
| `MEDICAL_REVIEW` | Medical Review | No change |
| `CLINICAL_REVIEW` | Clinical Review | No change |
| `MANUAL_REVIEW` | Manual Review | No change |
| `APPROVED` | Other Under Review | No change |
| `PAYMENT_PARTIAL` | Other Under Review | No change |
| `QUEUED` | Other Under Review | No change |
| `RESUBMITTED_MISSING_DOCUMENTS` | Other Under Review | No change |
| `SENT_FOR_PAYMENT_PROCESSING` | Other Under Review | No change |
| `SENT_TO_SURVEILLANCE` | Other Under Review | No change |
| `DECLINE` | **Other Under Review** | **⚠ Changed — was Rejected** |

### DHA CEO Acceptance Criteria — Field Mapping

| AC | Metric | Source field |
|---|---|---|
| Count under medical review | `n_claims` where `status = 'MEDICAL_REVIEW'` | `claim_flow_mart_v3_agg_table` |
| Value under medical review | `SUM(claim_value)` where `status = 'MEDICAL_REVIEW'` | `claim_flow_mart_v3_agg_table` |
| Count under clinical review | `n_claims` where `status = 'CLINICAL_REVIEW'` | `claim_flow_mart_v3_agg_table` |
| Value under clinical review | `SUM(claim_value)` where `status = 'CLINICAL_REVIEW'` | `claim_flow_mart_v3_agg_table` |

> **Visibility:** All four DHA CEO metrics are visible to both SHA and DHA users — no role-based restriction.

### Global Filters

All sections react to the following filters:

| Filter | Input type | Field |
|---|---|---|
| `input$period` | Date range | `period_month` |
| `input$county` | Multi-select | `county` |
| `input$facility` | Multi-select (cascades from county) | `facility` |

### Colour Palette

| Category | Hex | Use |
|---|---|---|
| Medical Review | `#1A56DB` | Blue |
| Clinical Review | `#7E3AF2` | Purple |
| Manual Review | `#D97706` | Amber |
| Other Under Review | `#6B7280` | Gray |

### Required Backend Change

The `dashboard_status` derivation in `materialized_views.claim_flow_mart_v3_agg_table` must be updated:

```sql
-- CURRENT (incorrect)
WHEN status = 'DECLINE' THEN 'Rejected'

-- CORRECTED
WHEN status = 'DECLINE' THEN 'Under_Review'
```

> **Note:** All figures in the original mockup are synthetic (`set.seed(42)`), calibrated to the reconciliation sheet values. Replace with a live query against the claims mart after the `DECLINE` reclassification is applied.
