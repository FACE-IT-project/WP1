# FACE-IT

The code repository for Work Package 1 (WP1) of the FACE-IT project. Please visit the [GitHub pages](https://FACE-IT-project.github.io/WP1/) for the human friendly version of this repository. There one may find publicly accessible versions of the WP1 deliverables as they are created. This is a [workflowr](https://jdblischak.github.io/workflowr/) project, meaning it has built-in version tracking information.

## Files

The collection, amalgamation, and analyses performed on the data for the [FACE-IT](https://www.face-it-project.eu/) project are spread out over several scripts detailed below for convenience. Scripts are listed generally by their order of dependency on each other. The collected and amalgamated data are stored on a pCloud account where they are open to all FACE-IT consortium members. Please contact Robert Schlegel for access (robert.schlegel@imev-mer.fr). v1.0 of the data product has been published on [PANGAEA](https://doi.pangaea.de/10.1594/PANGAEA.953115), with a v2.0 planned by the end of WP1 in July 2024.

- __metadata/__: Metadata used throughout the project.
- __code/__: Scripts used to collect, amalgamate, and analyse data for FACE-IT.
    - __workflowr.R__: Small script that runs the __`workflowr`__ code used to compile the [WP1 website](https://face-it-project.github.io/WP1/index.html).
    - __functions.R__: The location of most of the functions used throughout the other scripts. This contains most of the intellectual effort of the work package. Lot's of fancy footwork.
    - __study_sites.R__: Metadata for the FACE-IT sites. This is where the figures for the sites used throughout the [WP1 website](https://face-it-project.github.io/WP1/index.html) are created.
    - __key_drivers.R__: All of the code required to accurately scrape the PANGAEA database for FACE-IT relevant data. This is saved in it's own script because it takes a while to load and is only used by `data_collection.R`.
    - __data_query.R__: Code used to query the PANGAEA database for available and relevant data.
    - __data_collection.R__: Primarily used to scrape PANGAEA for FACE-IT relevant data. Also used to subset and compile gridded ice cover data and MUR 1km SST data for the FACE-IT sites.
    - __data_processing.R__: Workspace for managing the large SOCAT and GLODAP data products. When FACE-IT members have datasets that need to be processed for uploading to PANGAEA, this is done here.
    - __data_product.R__: This script does a lot of work. First it combines all data collected for FACE-IT from all sources into a single data.frame per site with a project-wide standard format. Then a second layer of cleaning on the collected PANGAEA data is applied before combining them with the manually collected data. These per site files are then re-ordered into products based on driver groupings, as requested by PANGAEA, before publishing them.
    - __data_extraction.R__: When a specific subset of data is requested from the amalgamated FACE-IT dataset, the record of that request and the code written for it is stored here.
    - __data_analysis.R__: Where most summary analyses are run and summary figures are saved.
    - __site_analysis.R__: One-off analyses for specific sites. Also houses the comparisons of SST products against local _in situ_ data as well as the creation of ice cover figures from gridded data.
    - __gridded_analysis.R__: Used to familiarise oneself with Morten's model output structure. Very short script.
    - __ice_cover_and_AIS.R__: A brief analysis of ice cover and ship presence (AIS) data. Currently limited to Isfjorden.
    - __fjord_light.R__: Code for exploring PAR data in the FjordLight package.
    - __visualising.R__: Test bed for visualising unusual data formats. Not used much.
    - __review_paper.R__: Code for the analyses and figures of the WP1 review paper (D1.3) on [the drivers of change in EU Arctic fjords](https://www.cambridge.org/core/journals/cambridge-prisms-coastal-futures/article/drivers-of-change-in-arctic-fjord-socioecological-systems-examples-from-the-european-arctic/1129E92425F6012F9610D8C9C172B2F0).
    - __data_paper.R__: Code for the analyses and figures of [the manuscript](https://essd.copernicus.org/articles/15/3733/2023/essd-15-3733-2023.pdf) documenting v1.0 of the WP1 data product.
- __data/__: The outputs of the scripts in the `code/` folder are saved here. These are not pushed to GitHub. Contact Robert Schlegel (robert.schlegel@imev-mer.fr) for access.
- __figures/__: The output of most of the scripts in the `code/` folder.
- __analysis/__: Markdown and supporting files used to create the pages for the [project site](https://face-it-project.github.io/WP1/index.html)
- __docs/__: The output of the `analysis/` folder.
- __poster/__: Files used to create posters for FACE-IT WP1 related presentations. Also contains the .pdf files for the posters.
- __shiny/__: Shiny apps developed as part of WP1.
    - __dataAcess/__: User interface for exploring, filtering, and downloading data collected for FACE-IT.
    - __kongCTD/__: User interface for uploading CTD data collected in Kongsfjorden.
    - __demoMHW/__: An interactive demonstration of how the Hobday et al. (2016. 2018) marine heatwave definition works.
- __users/__: Folder where anyone in the FACE-IT consortium may create their own folder and manage their code.
- __output/__: Empty folder created by __`workflowr`__ project structure. I prefer to save output to the `data/` folder.

