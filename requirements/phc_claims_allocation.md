# PHC Claims & Allocation — Indicators Functional Specification

## Overview

The PHC Claims & Allocation page gives DHA leadership and county health teams a single reconciling view of Primary Health Care claims and fund allocation: how much has been claimed nationally, where it concentrates by facility tier and geography, how allocation is actually flowing to facilities month by month, and what's driving utilisation clinically. It follows the Digital Health Agency's standard PHC table template (`requirements/other/phc-table-templates[21].docx`) as closely as a single-page mockup allows.

**Reporting period**: 1 Jul 2025 – 30 Jun 2026 (FY2025/26). **PHC maternity claims are retained in every table and panel** — they are never separated out or excluded; a maternity-specific view, if needed, would be an additional panel, not a filter that removes maternity from the base population.

Two different bases are in play on this page, and the caption on every table says which:

| Base | Tables | What it measures |
|---|---|---|
| Month of **service** | Headline cards, Tables 1–3, 5–8 | Claims filed for care actually delivered in the period |
| Month of **allocation** | Table 4 | Funds actually disbursed to facilities, which can lag the service month |

> **Fix the denominator before reading any single cell.** Headline cards, Table 1 and Table 2 all reconcile to the same national claim count, facility count and total amount — they're built from one underlying seeded grid, rolled up three different ways. Table 3 reconciles to Table 2 by county: every county row sums to its Total column, and the Total column sums to the National row. Table 4 does **not** reconcile to the headline total — it sits on the allocation base, not the service base.

Records with a missing level, ownership, county, diagnosis or intervention are retained in an explicit **"not stated" / "not classified"** category rather than dropped, so every column still sums to its stated total.

---

## AC1 — Headline Indicator Cards

Five cards: **Total PHC Amount, Transacting Facilities, Claims, Unique Members, Visit Frequency.**

| Card | Definition |
|---|---|
| Total PHC Amount | Sum of the claimed amount for all PHC transactions in the period, **KES '000** (precise value, not abbreviated to "M"/"B") |
| Transacting Facilities | Distinct facility IDs with ≥1 claim in the period — **not** the number of contracted or licensed facilities |
| Claims | Count of claim records for the period |
| Unique Members | Distinct beneficiary identifiers with ≥1 claim |
| Visit Frequency | Claims ÷ Unique Members ÷ number of months in the period |

**Unique Members is not a simple sum of the county-level figures in Table 2.** A member claiming in two counties is counted once per county and once nationally, so the national total is modelled as a fixed overlap discount below the sum of county totals (see Appendix, Data Model) — not an artefact, a direct consequence of how the source docx defines the metric.

---

## AC2 — Table 1: Facilities, Claims and Total Amount by Facility Level and Ownership

### Layout

Six facility-level groups (**Level 2 — Dispensary** through **Level 6 — National referral hospital**, plus **Level not classified**), each a bolded header row directly followed by its five ownership rows (**Public, FBO, Private, NGO, Ownership not stated**) — all rows visible without needing to expand anything — plus an always-visible **All levels** total row at the bottom. Not sortable: the level/ownership grouping is structural, and a column sort would scramble it.

### Columns

| Column | Sub-columns |
|---|---|
| Facilities | n, % of national total |
| Claims | n, % of national total |
| Total amount | **KES '000**, % of national total |
| Amount per claim | full KES |

Percentages are column percentages on the national total for that column, to one decimal place. Amount per claim is total amount ÷ claims within the same row. **Total amount figures are precise KES '000 values** (matching the source docx's declared unit), never abbreviated to "M"/"B".

---

## AC3 — Table 2: Facilities, Claims and Total Amount by County

Flat, sortable table — all 47 counties plus a **National** total row. Columns: County, Facilities, Claims, **Total amount (KES '000)**, Share of amount (%), Amount per claim (full KES), Unique members. County is assigned from the facility's registered county, not member residence.

---

## AC4 — Table 3: Total Amount by County and Facility Level

Cross-tab: 47 county rows × 6 facility-level columns (Level 2–6, not-classified folded in) + a **Total** column, plus a **National** row. Every row sums to its Total column; the Total column sums to the National row.

---

## AC5 — Spend Concentration Charts

Two bar charts summarising Tables 1–2: **Total Amount by Facility Level** (6 bars) and **Top 10 Counties by Total Amount**. Static — no toggle, matching the fidelity of the other summary charts already in this app (e.g. `apx`'s fund charts).

---

## AC6 — Table 4: Month-on-Month Allocation per Facility, by Payment Status

### Layout

One block per facility (a bolded facility/county/level header row directly followed by its three data rows — **Paid, Not paid, All** — no expand/collapse required) across the 12 rolling months (Jul–Jun) plus a Total column. A **National total** block (same shape) follows the facility blocks. Not sortable: the facility grouping is structural. Month columns are precise **KES '000** values.

### Scope reduction

The source docx shows this table for 4 facilities before noting it "repeats for every transacting facility." This mockup follows the same convention with **5 representative facilities**; a real implementation repeats the block for every transacting facility.

### Columns

| Column | Note |
|---|---|
| Month (×12, Jul–Jun) | KES '000 allocated in that calendar month |
| Total | Sum across the 12 months |

Status is the position **as at the extraction date** — Paid means the allocation has been settled, Not paid covers allocations approved but not yet settled. Because it's a snapshot, the extraction date must always accompany this table.

---

## AC7 — Tables 5–8: Diagnosis and Intervention Rankings

### Shared shape

Rank, Code, Title (+ Chapter for diagnoses), Claims (n, %), Unique members (n), Claims per member (mean). **No amount column, by design** — mixing a count ranking with an amount column invites the reader to treat the top of the list as the top cost driver, which it isn't. A separate table ranked by amount would need to be built if cost ranking is required. Each table ends with a **Top 20 subtotal** and an **All** row.

| Table | Scope | Note |
|---|---|---|
| Table 5 | Top 20 ICD-11 diagnoses, national | Full — 20 rows + subtotal + all |
| Table 6 | Top 20 diagnoses, by county | 3 representative counties shown (docx convention: repeats for all 47) |
| Table 7 | Top 20 diagnoses, by facility | 3 representative facilities shown (docx convention: repeats for every transacting facility) |
| Table 8 | Top 20 interventions, national | Full — volume only; a claim can carry >1 intervention, so intervention-record counts exceed claim counts and the % denominator is the record total, not the claim total |

**Claims per member is the frequency measure used consistently across all four tables**, reported alongside (not instead of) the share-of-claims percentage — narrative text should pick one and stay with it rather than switching between the two.

**Diagnosis codes/titles are illustrative placeholders**, not a real ICD-11 stem-code extract — flagged for replacement once the real diagnosis dictionary mapping is available (see Appendix).

---

## AC8 — No Interactive Filter Bar

Unlike `amb`/`apx`, this page has **no filter bar**. The reporting period (1 Jul 2025 – 30 Jun 2026) and extraction date are fixed and stated in the page header, matching how the source docx itself frames every table — a static reporting-period snapshot, not an ad-hoc query surface.

---

## Appendix: Business Rules & Data Dictionary

### Facility Level Labels

| Key | Label |
|---|---|
| `l2` | Level 2 — Dispensary |
| `l3` | Level 3 — Health centre |
| `l4` | Level 4 — Primary / county hospital |
| `l5` | Level 5 — County referral hospital |
| `l6` | Level 6 — National referral hospital |
| `lnc` | Level not classified |

### Ownership Categories

Public (MoH / county government), Faith-based (FBO), Private, NGO, Ownership not stated.

### Data Model

- **Raw grain**: one row = one PHC claim (facility × member × diagnosis × intervention).
- **Modelled grain**: one row = one county, aggregated facility/claim counts and total amount for the reporting period.

> **Note:** All figures are synthetic — one seeded county × facility-level grid rolled up three ways so headline cards and Tables 1–3 reconcile exactly by construction; Table 4's monthly allocation series and Tables 5–8's diagnosis/intervention rankings are generated independently (weighted, seeded) and are not required to tie out to the headline total. Facility counts, cost-per-claim figures and the diagnosis/intervention dictionaries are illustrative, not sourced from KMHFR or a real claims/ICD-11 extract — replace with live queries against the PHC claims mart and the real diagnosis/intervention dictionaries once available.
