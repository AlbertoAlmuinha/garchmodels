
# garchmodels 0.1.2

- Fixed a small bug that prevented the use of formulas on variables in the fit, e.g.: __fit(y ~ date + month(date), data = df)__

# garchmodels 0.1.1

### Tuning Parameters Univariate Garch Models

- `arch_order()`, `garch_order()`, `ma_order()` and `ar_order()`.
- Modified garch_reg predict function to allow tuning.

# garchmodels 0.1.0

* Initial Release
