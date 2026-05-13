# Approved vs Unpaid Claims — Feature Specification

## Summary

This feature surfaces the gap between claims that have been approved and claims that have been paid in the ERP system. This view identifies claims that are approved and ready for disbursement but have not yet been paid — giving operations visibility into the payment backlog.

---

## AC1 — Summary Metric Cards

At the top of the page, display three summary metric cards reflecting the current state of all payment-ready claims. These cards are **not subject to any date range filter** and always reflect real-time data at page load.

### Metric Card Definitions

| Metric | Description |
|---|---|
| Total claims | Number of all claims in ERP |
| Total value | Value of all claims in ERP |
| Unpaid claims | Number of unpaid claims in ERP |
| Value of unpaid claims | Claims with unpaid status in ERP |

### Metrics Disaggregation

- Fund type: **SHIF**, **ECCIF**, **POMSF**

---

## AC2 — Summary Table

Below the metric cards, display a paginated table showing the count of all approved claims not yet paid, grouped by county. The county row should be collapsible to reveal specific providers in that county.

### Column Definitions

| Column | Description |
|---|---|
| County / Provider | County name. Expand to see healthcare providers associated with the claims |
| Total Claims | Count of all payment-ready claims |
| Claims | Count of payment-ready unpaid claims |
| % of Unpaid Claims | Percentage of unpaid claims |
| Total Value | Total value of payment-ready claims |
| Unpaid Claims Value | Total value of unpaid claims |
| % Value | Percentage value of unpaid claims |

### Table Behaviour

- All columns are sortable

### RBAC

Accessible to: **SHA**, **DHA**
