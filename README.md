# Patent data exploration

This repository contains the data and analysis workflow related to the study:

"Patent data-driven analysis of literature associations with changing innovation trends."   
Adrian Sven Geissler, Jan Gorodkin, and Stefan Ernst Seemann.  
Frontiers in Research Metrics and Analytics, 2024.   
https://www.frontiersin.org/journals/research-metrics-and-analytics/articles/10.3389/frma.2024.1432673/

## Material

The analysis relates to 2 datasets of patent-literature associations downlaoded from the 
Lens platform (https://www.lens.org/).
The 2 datasets relate to all patents that match the 
search terms "CRISPR" (downloaded July 6, 2023)
and "cyanobacteria" (downloaded July 3, 2023).

## Methods

The analysis matches all patents to IPC terms (international patent classification).
After identifying IPC terms with trend changes in the number of patents over time, an over-representation test identifyies which literature is associated with the respective patents (and thus might be of interest in explaining the change in innovation trend).
