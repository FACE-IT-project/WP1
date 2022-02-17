# FACE-IT

The code repository for Work Package 1 (WP1) of the FACE-IT project. Please visit the [GitHub pages](https://FACE-IT-project.github.io/WP1/) for the human friendly version of this repository. There one may find publicly accessible versions of the WP1 deliverables as they are created. This is a [workflowr](https://jdblischak.github.io/workflowr/) project, meaning it has built-in version tracking information.

## Files

The analyses performed on the data collected for FACE-IT are spread out over a selection of scripts which are detailed below for convenience. Note that nearly all of the data collected for FACE-IT are stores on a pCloud account. These data are open to all FACE-IT consortium members. Please contact Robert Schlegel for access (robert.schlegel@imev-mer.fr). Links to the origins of the data may also be found in the [meta-database](https://face-it-project.github.io/WP1/metadatabase.html).

- __analysis/__: Markdown and supporting files used to create the pages for the [project site](https://face-it-project.github.io/WP1/index.html)
- __code/__: Scripts used to perform a range of analyses on the data collected for FACE-IT
    - __data_analysis.R__: Where most summary analyses are run and figures are saved
    - __data_collection.R__: Primarily used to scrape PANGAEA for FACE-IT relevant data. Also used to subset and compile gridded ice cover data and MUR 1km SST data for the FACE-IT sites.
    - __data_processing.R__: Currently used to manage gridded GLODAP data.
    - __data_product.R__: Combines data collected from FACE-IT from all sources into a single data.frame per site with a project0wide standard format. Also provides a second layer of cleaning on the collected PANGAEA data before combining them. This script does a lot of work.
    - __FACE_IT_2021.R__: Code used in the analyses shown at the FACE-IT 2021 annual meeting.
    - __functions.R__: The location of most of the functions used throughout the other scripts. This contains most of the intellectual effort of the work package. Lot's of fancy footwork.
    - __gridded_analysis.R__: Short script used to familiarise oneself with Morten's model output structure. Very short script.
    - __ice_cover_and_AIS.R__: A brief analysis of ice cover and ship presence (AIS) data. Currently limited to Isfjorden.
    - __key_drivers.R__: All of the code required to accurately scrape the PANGAEA database for FACE-IT relevant data. This is saved in it's own script because it takes a while to load and is only used by data_collection.R.
    - __review_paper.R__: Code used for the analyses for the WP1 review paper (D1.3). Much of this was taken from other pre-existing scripts.
    - __site_analysis.R__: One-off analyses for specific sites. Also houses the comparisons of SST products against local _in situ_ data as well as the creation of ice cover figures from gridded data.
    - __SSC_2021.R__: Code used for analyses shown at the SSC conference in November 2021.
    - __study_sites.R__: Metadata for the FACE-IT sites. This is where the figures for the sites used throughout the [WP1 website](https://face-it-project.github.io/WP1/index.html) are created.
    - __visualising.R__: Test bed for visualising unusual data formats. Not used much.
    - __workflowr.R__: Small script used to run the __`workflowr`__ code to compile the website.
- __data/__: The outputs of the scripts in the `code/` folder are saved here. These are not pushed to GitHub. Contact Robert Schlegel (robert.schlegel@imev-mer.fr) for access.
- __docs/__: The output of the `analysis/` folder.
- __figures/__: The output of most of the scripts in the `code/` folder.
- __metadata/__: Metadata used throughout the project.
- __output/__: Empty.
- __poster/__: Files used to create posters for FACE-IT WP1 related presentations. Also contains the .pdf files for the posters.
- __shiny/__: Shiny apps developed as part of WP1.
    - __dataAcess/__: User interface for exploring, filtering, and downloading data collected for FACE-IT.
    - __kongCTD/__: User interface for uploading CTD data collected in Kongsfjorden.
- __users/__: Folder where anyone in the FACE-IT consortium may create their own folder and manage their code.

