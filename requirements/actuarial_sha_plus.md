# SHA Plus — Membership & Claims Analysis — Indicators Functional Specification

## Overview

This page gives DHA leadership and the actuarial/pricing function a reconciling view of active membership and claims, split by **Informal** and **Formal** sector — the analysis that underpins the "SHA Plus" top-up product pricing work. It originates from an external data request (Minet Kenya, in partnership with Transnep Insurance Brokers Limited, addressed to SHA's Deputy Director, Benefits — see `requirements/other/Revamp Indicators V2.xlsx`, `Cover` and `Data Request Tracker` sheets for the original context). **This mockup covers only the two quantitative schedules that were approved for sharing** — Schedule 1 (membership & claims by age band) and the Monthly Contributions vs Claims trend — not the request-tracking sheet itself, which is a governance artifact, not a claims-reporting UI.

Two source periods are in play, and every section states which:

| Section | Period | Source |
|---|---|---|
| Schedule 1 (age-band table) | 1 Jan – 31 Dec 2025 | `member_contribution_combined.csv` × `household_dependent_combined.csv` × `230626_util.csv`, joined on `cr_id` |
| Monthly Contributions vs Claims | Oct 2024 – Jun 2026 (to date) | Same three files, bucketed by month |

**Figures on this page are transcribed directly from the source workbook, not synthetically generated.** This is a real actuarial analysis snapshot being communicated to developers, not an operational claims-flow mockup with seeded placeholder data.

---

## AC1 — Sector Toggle and Headline Cards

### Layout

Pill toggle: **Informal / Formal / Overall**, reusing the fund-pill pattern already used elsewhere in this app (`apx`). Each pane shows seven metric cards:

| Card | Definition |
|---|---|
| Contributors | Distinct contributors active in the sector for the period |
| Contributions | Sum of contribution_amount for the period; sponsor members excluded |
| Dependants | Attributed to the principal contributor's cohort via household_number |
| Claims | Claims admitted in 2025 for the cohort (all stages) |
| Amount Claimed | SHIF FUND + ECCIF FUND, aggregated |
| Avg Claim / Contributor | Amount Claimed ÷ Contributors |
| **Loss Ratio** | Amount Claimed ÷ Contributions, shown as a multiple (e.g. `11.8x`) |

**Loss Ratio is a derived indicator, not a workbook column** — computed directly from the two totals the workbook already reports side by side. It is the single most important actuarial read of this schedule: it directly shows whether a sector's claims are running above or below what it contributes, which is exactly the cross-subsidy question the SHA Plus pricing work needs answered.

### Overall pane

Combined Informal + Formal totals only — the age-band schedule itself is not duplicated on the Overall tab (see the Informal/Formal tabs for the full breakdown), since Schedule 1 doesn't define a combined-cohort age-band split.

---

## AC2 — Table: Schedule 1 — Age-Band Membership & Claims

Per sector (Informal, Formal): 10 age-band rows (18-25 through >65) + a **TOTAL** row + an **Age not captured** line, sortable.

| Column | Definition |
|---|---|
| Age Band | Contributor age as at 31 Dec 2025 — dependants are not independently age-banded |
| Contributors | No. of contributors in the band |
| Contributions | Sum of contribution_amount for the band |
| Dependants | Attributed via household_number |
| Claims | Claims admitted in 2025 for the cohort |
| Amount Claimed | SHIF FUND + ECCIF FUND |
| Avg Claim / Contributor | Amount Claimed ÷ Contributors, within the band |

**TOTAL must equal the sum of the ten age-band rows** — enforced by construction since the figures are the workbook's own reported totals. **Age not captured** (contributors whose DOB is blank in the source, so they cannot be age-banded) is shown as a separate line and is **excluded** from both the age bands and the TOTAL row, matching the source workbook's own convention rather than silently dropping those records.

---

## AC3 — Insight: Cross-Subsidy Between Sectors

A banner at the top of the page states the loss-ratio comparison directly: the Informal sector's claims run at a multiple of contributions far above the Formal sector's — meaning the Formal sector is, in effect, cross-subsidising Informal-sector claims. This is computed live from `act_sector_totals`, not a hardcoded figure, so it stays correct if the underlying schedule numbers are ever updated.

---

## AC4 — Chart and Table: Monthly Contributions vs Claims

### Layout

One combo line chart — four series (Formal Contributions, Formal Claims, Informal Contributions, Informal Claims) over 21 months (Oct 2024 – Jun 2026), followed by the full underlying data table.

### Columns

Month, Formal CRs, Formal Contributions, Formal Claims, Formal Amount, Informal CRs, Informal Contributions, Informal Claims, Informal Amount, plus a TOTAL row.

### The one non-additive column

**No. of CRs (distinct contributors who contributed that month) is not summed to a TOTAL.** A contributor who contributes in multiple months is counted once in each of those months, so summing across months would overcount — this matches the source workbook, which explicitly shows no total for this column while totalling every other column.

---

## AC5 — No Interactive Filter Bar

This page has no filter bar. Both source periods (calendar year 2025 for Schedule 1; Oct 2024 – Jun 2026 for the monthly trend) are fixed and stated in the section headers — the source data doesn't support finer-grained interactive slicing than what's already shown, so a filter bar would misrepresent it as more granular than it is.

---

## Appendix: Business Rules & Data Dictionary

### Reconciliation Identities

| Identity | Holds because |
|---|---|
| Schedule 1 TOTAL row = Σ(age-band rows), per sector | Both are the workbook's own reported figures, transcribed as-is |
| Overall = Informal + Formal, per metric | Computed as a sum in `act_sector_totals$Overall` |
| Loss Ratio = Amount Claimed ÷ Contributions | Derived at render time from the two totals above, never hardcoded |
| Monthly trend TOTAL = Σ(21 monthly values) | Except **No. of CRs**, which is intentionally not summed (see AC4) |

### Colour Palette

| Series | Hex | Use |
|---|---|---|
| Formal Contributions | `#0284c7` | Blue, solid line |
| Formal Claims | `#7dd3fc` | Light blue, dashed line |
| Informal Contributions | `#16a34a` | Green, solid line |
| Informal Claims | `#dc2626` | Red, dashed line |
| Loss ratio > 1 (claims exceed contributions) | `#dc2626` | Red metric-card icon |
| Loss ratio ≤ 1 | `#16a34a` | Green metric-card icon |

### Data Model

- **Raw grain**: one row = one contributor-month (joined: `member_contribution_combined` × `household_dependent_combined` × `util`, on `cr_id`).
- **Modelled grain**: one row = one sector × age band, aggregated contributors/contributions/claims for the schedule period — this is Schedule 1 itself.

> **Note:** Unlike every other module in this app, the reference figures here are **not synthetic** — they are transcribed directly from `requirements/other/Revamp Indicators V2.xlsx`. Only the illustrative `act_raw_sample` contributor-level rows (Data Model tab) are constructed examples of the row grain; the aggregated schedule and monthly-trend figures themselves are real.
