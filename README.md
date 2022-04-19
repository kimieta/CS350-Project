# Region Of Interest Identifier 
#### Kim Ta: CS350 Data science project _application prototype_
##
##
## Background 

As part of this project, we have been analysing protein distributions by looking at methods of finding the region of interest in fluorescent microscopic images of proteins. 

Seeded Growing Region (Rai, G.N., 2010) is a method we have implemented in this application to find the region of interest of a single structure in a digital image.  Users can upload any image to outline a specific structure in that image. 

##
## Extra Application Features
Users can:
- Reduce the resolution of the image for a quick outline,
- Choose the outline colour, and
- See an animation of the outlining process. 
##
## Installation

This application requires [R](https://cran.r-project.org/bin/windows/base/) version 4.1.1 to run.

##
## R Packages

We use the following open packages to create our application:

- [imager](https://cran.r-project.org/web/packages/imager/index.html) - image importing and processing
- [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html) - working with data frames
- [magick](https://cran.r-project.org/web/packages/magick/vignettes/intro.html) -  complex image processing
- [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) - creating graphics
- [animation](https://cran.r-project.org/web/packages/animation/index.html) - creating animations
- [shiny](https://cran.r-project.org/web/packages/shiny/index.html)  version 1.7.1 - building an interactive web application
- [shinycssloaders](https://cran.r-project.org/web/packages/shinycssloaders/index.html) - add animations into a Rshiny application 
- [bslib](https://cran.r-project.org/web/packages/bslib/index.html) - uses CSS to style Rshiny application 

##
> Note: To install R packages, copy and paste the content from the file "install_r_packages.txt" into the console. (Found on the GitHub page: https://github.com/kimieta/CS350-Project) 

##
## References 
Rai, G.N. 2010, “Gradient Based Seeded Region Grow method for CT Angiographic Image Segmentation”. https://arxiv.org/pdf/1001.3735.pdf. Accessed: 10/03/2022. 
