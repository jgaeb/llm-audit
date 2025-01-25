Auditing large language models for race & gender disparities: Implications for
artificial intelligence–based hiring
---
Replication materials for Gaebler, Goel, Huq, and Tambe (2025) "Auditing large
language models for race & gender disparities: Implications for artificial
intelligence–based hiring."

To reproduce the analyses in our paper:
1. Make sure you have `R` version 4.4.0 and the `groundhog` package installed.
2. Unzip `data.db.gz`:
```bash
gunzip data.db.gz
```
3. Run the `R` script
```bash
Rsript analyze.R
```

The following scripts are also provided:
* `personas.py`: Generates the personae used in the study.
* `prompts.py`: Generates the prompts for different correspondence experiments
  performed in the study from the redacted and unredacted application
  materials.
* `chat.py`: Runs the experiments using generated prompts.
* `embed.py`: Generates word embeddings used to calculate the predictability of
  race and gender from application materials.

**NOTE:** The application materials (and raw model outputs, which contain
snippets of the application materials) are not included in the public data.
Researchers wishing to obtain these data should contact the authors directly.
