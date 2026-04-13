# eq5dsuite

A standardised suite of tools for EQ-5D analysis, implementing 
the analytical recommendations of Devlin et al. (2020) across 
three platforms. eq5dsuite supports the full EQ-5D analytical 
workflow — from value calculation to profile, EQ-5D value, and 
EQ-VAS analyses — ensuring consistent, reproducible outputs 
regardless of the analytical environment.

## Implementations

| Platform | Repository | Status |
|---|---|---|
| R | [eq5dsuite-r](https://github.com/MathsInHealth/eq5dsuite-r) | Available on CRAN |
| Stata | [eq5dsuite-stata](https://github.com/MathsInHealth/eq5dsuite-stata) | Available |
| Excel | [eq5dsuite-excel](https://github.com/MathsInHealth/eq5dsuite-excel) | Available |

## Features

All three implementations provide:

- EQ-5D value calculation for the EQ-5D-3L, EQ-5D-5L, and 
  EQ-5D-Y-3L instruments
- Crosswalk methods for mapping between instrument versions, 
  including the NICE-recommended UK mapping
- Profile analysis, EQ-5D value analysis, and EQ-VAS analysis 
  following the recommendations of Devlin et al. (2020)
- Support for published national value sets and user-defined 
  custom value sets

## Getting started

Choose the implementation that fits your analytical environment:

**R users:**
```r
install.packages("eq5dsuite")
```
Full documentation and worked examples are available in the 
[eq5dsuite-r](https://github.com/MathsInHealth/eq5dsuite-r) 
repository.

**Stata users:**
Download the ado files from the 
[eq5dsuite-stata](https://github.com/MathsInHealth/eq5dsuite-stata) 
repository and follow the installation instructions in the 
user guide.

**Excel users:**
Download the macro-enabled workbook from the 
[eq5dsuite-excel](https://github.com/MathsInHealth/eq5dsuite-excel) 
repository and follow the instructions on the Welcome sheet.

## About

eq5dsuite was developed to address the lack of standardised, 
reproducible tools for EQ-5D analysis in practice. Despite the 
availability of methodological guidance (Devlin et al., 2020), 
implementation has historically been fragmented across 
spreadsheets and bespoke scripts, leading to inconsistency and 
limited reproducibility. eq5dsuite bridges this gap by providing 
a harmonised analytical framework across the platforms most 
commonly used in health economics and outcomes research.

## Issues and feedback

This repository is the central issue tracker for cross-platform 
concerns — for example, inconsistencies between implementations, 
feature requests that apply to all platforms, or general 
questions about the eq5dsuite project.

For platform-specific issues, please use the issue tracker in 
the relevant repository:
- [R issues](https://github.com/MathsInHealth/eq5dsuite-r/issues)
- [Stata issues](https://github.com/MathsInHealth/eq5dsuite-stata/issues)
- [Excel issues](https://github.com/MathsInHealth/eq5dsuite-excel/issues)

For general enquiries, contact 
[info@mathsinhealth.com](mailto:info@mathsinhealth.com).

## Citation

If you use eq5dsuite in your research, please cite:

> Estévez-Carrillo A, Rivero-Arias O, Schlackow I, Rand K.
> eq5dsuite: An R Package for Describing and Analysing EQ-5D Data.
> *The R Journal* (forthcoming).

## License

MIT License. See [LICENSE](LICENSE) for details.
