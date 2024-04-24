# meme/memes.R
# Script used to create memes used in presentations


# Setup -------------------------------------------------------------------

library(extrafont)
library(showtext)
library(meme)


# Fonts -------------------------------------------------------------------
# https://cran.r-project.org/web/packages/extrafont/readme/README.html

# Import all fonts on computer to a usable format/location for R
# NB: Only run once
# font_import()

# Only necessary in session where you ran font_import()
# loadfonts()

# Vector of font family names
fonts()

# Show entire table
fonttable()

# Impact doesn't play nice
font_add("Impact", "~/Downloads/impact.ttf")
showtext_auto()


# Create ------------------------------------------------------------------
# https://cran.r-project.org/web/packages/meme/vignettes/meme.html

# MHW definition
MHW_def <- meme(img = "meme/Afraid-To-Ask-Andy.jpg", 
                upper = "I don't know the MHW definition", 
                lower = "and at this point I'm too afraid to ask", size = 7)
MHW_def
meme_save(MHW_def, file = "meme/MHW_def.png")

# MHW So hot right now
MHW_hot <- meme(img = "meme/so-hot-right-now.jpg", 
                upper = "Marine   heatwaves", 
                lower = "", size = 9, r = 0.9)
MHW_hot
meme_save(MHW_hot, file = "meme/MHW_hot.png")

