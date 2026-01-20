# IntelliProfiler 2.0  
<img src="images/logo.png" width="200">

### A research workflow for multi-animal behavioral analysis using R

This repository provides the source code for **IntelliProfiler 2.0**,  
a research-oriented workflow implemented in R for analyzing locomotor activity
and social proximity in group-housed mice.

IntelliProfiler processes positional data acquired from a commercially available
high-resolution RFID floor plate (eeeHive2D, Phenovance LLC, Japan; Lipp et al., 2024).
The workflow generates automated, quantitative behavioral metrics and visualizations
for downstream behavioral and statistical analyses.

> ⚠️ **Note**  
> IntelliProfiler is a *research workflow*. It does **not** include or control any RFID
> hardware components. Users must obtain compatible RFID tracking systems
> (e.g., eeeHive2D) separately.  
> This pipeline has not been validated with other hardware configurations.

---

## 📚References

Details of the IntelliProfiler workflow are described in the following article:  
**IntelliProfiler: a research workflow for analyzing multiple animals with a high-resolution home-cage RFID system** 
*(Ochi et al., 2026, Lab Animal)*  
https://www.nature.com/articles/s41684-025-01668-4

---

## 🧩Prerequisites

The following R packages are required to run the core scripts:

- `tidyverse`
- `openxlsx`
- `lubridate`

Additional packages may be required depending on the selected modules.

---
## ⚠️ Large files and Git LFS 
This repository contains large example data files that are managed using**Git Large File Storage (Git LFS)**.  
When cloning or pushing this repository—especially from a remote server—
please ensure that Git LFS is installed and properly configured in your environment.

---

## 🗂️Directory Structure
```text
IntelliProfiler2.0/
├─ scripts/
│ 
├─ data/
│ └─ example/ # example RFID log files
│ 
├─ results/
│ └─ example/ # example outputs (Excel, figures)
│ 
├─ images/     #figures used in README
├─ LICENSE
├─ .gitignore
└─ README.md
```

---
## 📝Citation
Once published, please cite: **Ochi S#, Azuma M#, Hara I, Inada H, Takabayashi K, Osumi N  IntelliProfiler 2.0: A high-throughput R-based pipeline for behavioral analysis of group-housed mice tracked with a high-resolution RFID system ''
 
---
## 📜License
This project is licensed under the MIT License see the LICENSE file for details
