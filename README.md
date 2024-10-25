# rare-plants-research-effort
Included in this repository are the data and scripts used to do the analysis for "QUANTIFYING RESEARCH EFFORT FOR CALIFORNIA’S FLORA:
EVIDENCE OF BIAS AGAINST RARE PLANTS IN LITERATURE AND SEQUENCE DATA", submitted to Madroño.

Below is a list of scripts, and what each script does.

1. Google Scrape 2.ipynb
    - Uses the searchlist.csv taken from Jepson eFlora to search GoogleScholar and log the number of search hits

2. genbank-scrape.Rmd
    - Uses the searchlist.csv taken from Jepson eFLora to search Genbank NCBI and log the number of saerch hits

3. query_from_species_list.ipynb
    - Uses AcceptedNames20210322.csv to request GBIF data for all accepted specis names from Jepson eFlora

4. occ_to_count.py 
    - Bins species into Uber's H3 hexagonal grids to calculate area

6. 02_statistical_analysis.R
    - Takes in completed dataframe containing area, papers and accessions and conducts all statistical analyses present in the paper

7. 03_summary_statistics.R
    - Takes the already loaded "lit_seq" object from script "6" gets calculates basic statistics

8. 04_figures.R
    - Code to recreate all figures present in the analysis 
