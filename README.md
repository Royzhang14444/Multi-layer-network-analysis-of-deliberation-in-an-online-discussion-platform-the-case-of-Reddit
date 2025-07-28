# Multi-layer-network-analysis-of-deliberation-in-an-online-discussion-platform-the-case-of-Reddit
## Description

This repository contains the data and analysis scripts used in the study
"Multi-layer network analysis of deliberation in an online discussion platform: the case of Reddit".

The repository includes:
- Links to the original Reddit posts used in this study, so that the data can be downloaded directly from the source if needed.
- Processed datasets from two representative subreddits: **AITAH** and **Europe**, which were used in the analyses presented in the paper.  
  For privacy reasons, usernames have been normalised to numeric identifiers, and due to size limitations, the original comment text is not included.
- R scripts for data downloading (via the vosonSML package) and deliberation metrics analysis.

## Repository structure

```
├── data/
│   ├── links/         # Text files listing Reddit post links (raw data sources)
│   ├── processed/     # Processed datasets from AITAH and Europe subreddits
│
├── code/
│   ├── 01_download.R  # Download data using vosonSML
│   ├── 02_analysis.R  # Calculate deliberation metrics
│
└── README.md          # Project description and instructions
```

## Contact

For any questions about the data or analysis scripts, please contact:

Tianshu (Roy) Gao  
Email: roygao2016@gmail.com
