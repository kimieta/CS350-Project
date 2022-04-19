library(shiny)
library(imager)
library(animation)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(magick)
library(bslib)


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "lux"),
    # Application title
    titlePanel("Seeded Growing Region"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("myFile", "Choose a file", 
                      accept = c('image/png', 'image/jpeg')),
            sliderInput("sclrez",
                        "Resolution factor:",
                        min = 1,
                        max = 100,
                        value = 50),
            numericInput("x_seed_point",
                        "Seed (x co-ordinate):",
                        value = 30),
            numericInput("y_seed_point",
                        "Seed (y co-ordinate):",
                        value = 15),
            numericInput("thres",
                         "Threshold (0 - 1):",
                         value = 0.3),
            selectInput("col_choice",
                        "Colour:", choices = list("Red" = "#FF0000", "Blue" = "#0000FF",
                                                  "Yellow" = "#FFFF00", "White" = "#FFFFFF"), selected =  "#ff000"),
            
            submitButton()
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Information",h2("Information Page"),
                   h4("Purpose"),
                   p("Seeded Growing Region algorithm for image segmentation, in order to outline the region of interest in an image (for one structure only)."),
                   br(),
                   h4("Inputs"),
                   p("Choose a file: The file path to the image to perform this algorithm on."),
                   p("Resolution factor: The resolution you want the image to analysed, lower resolution images will take less time compared to higher resolutions."),
                   p("Seed (x co-ordinate) and Seed (y co-ordinate): The starting coordinate of the seed on the image to initialise the algorithm on. "),
                   p("Threshold (0 - 1): The threshold for differentiating what belongs in the seed group and what doesn't."),
                   p("Colour: The colour of the outline, purely as an extra feature."),
                   br(),
                   h4("Language"),
                   p("R version 4.1.1, application created using RShiny."),
                   br(),
                   h4("Github"),
                   a("https://github.com/kimieta/CS350-Project"),
                   br()),
          tabPanel("Image", plotOutput("plot1") %>% withSpinner(color="#0dc5c1")),
          tabPanel("Outline", plotOutput("plot2")  %>% withSpinner(color="#0dc5c1")), 
          tabPanel("SGR process",imageOutput("plot3") %>% withSpinner(color="#0dc5c1"))
        )
      )
    )
)

# functions ------

#' Resizes image by given scale factor
#'
#' @param img: Image 
#' @param factor: scale factor 
#'
#' @return reimage: resized image
#'
#' @example e.g. factor 2 --> 50% size of image

img_resizer <- function(img, factor){
  reimage <- resize(img, dim(img)[1]/factor, dim(img)[2]/factor)
  return(reimage)
}


#' Produces resized version of uploaded image
#'
#' @param f_input: original file input path 
#' @param res: resolution choice
#'
#' @return reimage: resized image

get_reszimage <- function(f_input, res){
  # getting the file name
  file_name <- (f_input)$name
  
  # creating path to copied image and putting it in the right format
  file_path <- paste("c:/temp/", file_name)
  file_path <- gsub("/", "\\\\", file_path)
  file_path <- gsub(" ","", file_path)
  
  # loading the image
  image <- load.image(file_path)
  
  # image resized
  reimage <- img_resizer(image, res)
  
  return(reimage)
  
}

#' Change the colour of a specific pixel in the image
#'
#' @param im_datafr: data frame - RBG dataframe version of image 
#' @param x_val: integer - x co-ordinate of pixel to change colour
#' @param y_val: integer - y co-ordinate of pixel to change colour
#' @param col: hex-digit colour value 
#'
#' @return im_datafr: data frame - RGB dataframe version of image with changed pixel

color_changer <- function(im_datafr, x_val, y_val, col){
  # setting the specific pixel to red
  im_datafr[im_datafr$x == x_val & im_datafr$y == y_val, ][6] <- col
  plot <- ggplot(im_datafr, aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()
  
  file_name <- paste(x_val, y_val, "_plot.jpg", sep = "")
  ggsave(filename = file_name, plot = plot)
  
  return(im_datafr)
}

#' Indicator for CORNER pointer translation
#'
#' @param quadrant: integer - indication of direction for pointer to move 
#' @param x_val: integer - x co-ordinate of pointer
#' @param y_val: integer - y co-ordinate of pointer
#'
#' @return pointers_list: list - new pointers created
#' only 4 possible corners, so each quadrant is like top, bottom: left or right
#' each CORNER pointer moves onto THREE new pointers, one corner, two edges
#' if the pointer goes left, we will move the pixel one unit left, so by -1 (on x-axis)
#' similar for right, up, and down (up and down is on the y-axis)
#' take in x and y values, return pointer (x,y) with new x and y values

corner_pointers <- function(quadrant, x_val, y_val){
  # top left, movement up and left
  if (quadrant == 1){
    CORNER_pointer <- c(x_val-1, y_val+1)
    EDGE_pointer1 <- c(x_val, y_val+1)
    EDGE_pointer2 <- c(x_val-1, y_val)
    # top right, movement up and right
  } else if (quadrant == 2){
    CORNER_pointer <- c(x_val+1, y_val+1) 
    EDGE_pointer1 <- c(x_val, y_val+1)
    EDGE_pointer2 <- c(x_val+1, y_val)
    # bottom right, movement down and right
  } else if (quadrant == 3){
    CORNER_pointer <- c(x_val+1, y_val-1)
    EDGE_pointer1 <- c(x_val, y_val-1)
    EDGE_pointer2 <- c(x_val+1, y_val)
    # bottom left, movement down and left 
  } else {
    CORNER_pointer <- c(x_val-1, y_val-1)
    EDGE_pointer1 <- c(x_val, y_val-1)
    EDGE_pointer2 <- c(x_val-1, y_val)
  }
  
  pointers_list <- list(CORNER_pointer, EDGE_pointer1, EDGE_pointer2)
  return(pointers_list)
}

#' Indicator for EDGE pointer translation
#'
#' @param direction: integer - indication of direction for pointer to move 
#' @param x_val: integer - x co-ordinate of pointer
#' @param y_val: integer - y co-ordinate of pointer
#'
#' @return pointer: vector - new pointer created
#' they can only go up, down, left or right

edge_pointers <- function(direction, x_val, y_val){
  if (direction == "up"){
    pointer <- c(x_val, y_val+1)
  } else if (direction == "down") {
    pointer <- c(x_val, y_val-1)
  } else if (direction == "right") {
    pointer <- c(x_val+1, y_val)
  } else if (direction == "left") {
    pointer <- c(x_val-1, y_val)
  }
  return(pointer)
}

#' Validation and error checking to make sure seed coord is valid
#'
#' @param x: integer - x co-ordinate of seed
#' @param y: integer - y co-ordinate of seed
#' @param dimension: integer - max number of pixels 
#' @param indicator: binary - indicating seed alone or seed and threshold (0/1) 
#'
#' @return customised error message

validator <- function(x, y, dimension, indicator){
  if ((x %in% 1:dimension)&(y %in% 1:dimension)) {
    return(paste("You have chosen coordinate (", x, ',', y,")"))
  } else if (indicator == 0){
    stop(
      paste("The x and y seed values need to be between 1 and ", dimension, ",
            Current seed values are ", x, " and ", y, ".")
    )
  } else {
    stop("An outline cannot be produced with the chosen threshold or seed value,please try again with different inputs.
    
         Choose seed coordinate within an object with a full structure and recommended to choose a threshold between 0.2 - 0.6.")
  }
  
}

server <- function(input, output, session) {
    
    # Saving image to local location so that it can be accessible 
    observeEvent(input$myFile, {
      inFile <- input$myFile
      if (is.null(inFile))
        return()
      file.copy(inFile$datapath, file.path("c:/temp", inFile$name) )
    })
  
    # Second tab (page) of the application, 
    # shows chosen image with resolution and seed point,
    # if chosen seed point is invalid, error message, 
    # giving the valid range is shown.
  output$plot1 <- renderPlot({
      
    # load the upload image and resize
    reimage <- get_reszimage(input$myFile, input$sclrez)
    
    grayim <- grayscale(reimage, method = "Luma", drop = TRUE)
    graydata <- as.data.frame(grayim)
    
    # choosing the seed
    xs <- input$x_seed_point
    ys <- input$y_seed_point
    
    selected_point <- filter(graydata, xs & ys)
    seed_greyval <- graydata[graydata$x == xs & graydata$y == ys, ][3]

    # error checking and validation
    # checking if the seeds are in the image 
    validate(
      need(validator(xs, ys, dim(reimage)[1], 0), "Current x and y values are invalid")
    )
    
    imgdata <- as.data.frame(reimage, wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
    imgdata <- color_changer(imgdata, xs, ys, input$col_choice)
    ggplot(imgdata, aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()
    
  })
  
  output$plot2 <- renderPlot({
    
    # load the upload image and resize
    reimage <- get_reszimage(input$myFile, input$sclrez)
    
    grayim <- grayscale(reimage, method = "Luma", drop = TRUE)
    graydata <- as.data.frame(grayim)
    
    # choosing the seed
    xs <- input$x_seed_point
    ys <- input$y_seed_point
    
    selected_point <- filter(graydata, x == xs & y == ys)
    seed_greyval <- graydata[graydata$x == xs & graydata$y == ys, ][3]
    # choosing the threshold
    threshold <- input$thres
    
    # getting the outline colour
    outline_col <- input$col_choice
    
    imgdata <- as.data.frame(reimage, wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
    imgdata <- color_changer(imgdata, xs, ys, outline_col)
    
    # recurrence relation to assign each pixel to a group (seed or not)
    # if it is, add it to the seed set
    # if it doesn't reach the threshold, stop the pointer, this is the edge
    # so then start to create the outline 
    
    # there are 8 starting pointes, each pointer of the seed will branch out
    # until the threshold is no longer met 
    # getting our first pointers 
    x_val <- selected_point$x
    y_val <- selected_point$y
    EDGE_up <- edge_pointers("up", x_val, y_val)
    EDGE_down <- edge_pointers("down", x_val, y_val)
    EDGE_right <- edge_pointers("right", x_val, y_val)
    EDGE_left <- edge_pointers("left", x_val, y_val)
    CORNER_q1 <- corner_pointers(1, x_val, y_val) # contains 3 pointers
    CORNER_q2 <- corner_pointers(2, x_val, y_val)
    CORNER_q3 <- corner_pointers(3, x_val, y_val)
    CORNER_q4 <- corner_pointers(4, x_val, y_val) # CORNER_q4[[1]] is the corner pointer
    

    # checking if the greyscale value is close by threshold value
    checker_edge <- function(pointer, direction, data, col){
      x_val <- pointer[1]
      y_val <- pointer[2]
      
      # error checking and validation
      # checking if an outline can be produced with the inputs
      validate(
        need(validator(x_val, y_val, dim(reimage)[1], 1), "Invalid inputs")
      )
      
      # getting the row from the dataframe with these coordinates
      row_data <- graydata[graydata$x == x_val & graydata$y == y_val, ]
      

      
      # getting the gayscale value of the pixel
      gray_val <- row_data[3]
      
      
      if (abs(seed_greyval - gray_val) < threshold){
        pointer <- edge_pointers(direction, x_val, y_val)
        data <- checker_edge(pointer, direction, data, col)
        
      } else {
        data <- color_changer(data, x_val, y_val, col)
        
      }
      
      return(data)
    }
    
    # with corners, we can move in three directions
    # the diagonal, and either L/R or Up/Down, dependant on its quadrant
    # note the corner will always move in the same quadrant
    # take in pointers coming out of the corner
    checker_corner <- function(pointer_list, direction, data, col){
      
      # case one, the edge pointers
      
      LR_pointer <- pointer_list[[2]]
      updown_pointer <- pointer_list[[3]]
      
      # determining the direction of the edge pointers from a corner pointer
      # based on the quadrant it is in
      if (direction == 1 | direction == 4){
        LR <- "left"
      } else {
        LR <- "right"
      }
      
      if (direction == 1 | direction == 2) {
        updown <- "up"
      } else {
        updown <- "down"
      }
      
      data <- checker_edge(LR_pointer, LR, data, col)
      data <- checker_edge(updown_pointer, updown, data, col)
      
      # case two, the corner specifically 
      corner_pointer <- pointer_list[[1]]
      x_val <- corner_pointer[1]
      y_val <- corner_pointer[2]
      
      # error checking and validation
      # checking if an outline can be produced with the inputs
      validate(
        need(validator(x_val, y_val, dim(reimage)[1], 1), "Invalid inputs")
      )
            
      # getting the row from the dataframe with these coordinates
      row_data <- graydata[graydata$x == x_val & graydata$y == y_val, ]

      
      # getting the gayscale value of the pixel
      gray_val <- row_data[3]
      
      if (abs(seed_greyval - gray_val) < threshold){
        corner_pointer <- corner_pointers(direction, x_val, y_val)
        data <- checker_corner(corner_pointer, direction, data, col)
        
      } else {
        data <- color_changer(data, x_val, y_val, col)
      }
      
      return(data)
      
    }
    
    # running the algorithm with the 8 initial pointers
    
    imgdata <- checker_corner(CORNER_q1, 1, imgdata, outline_col)
    imgdata <- checker_edge(EDGE_up, "up", imgdata, outline_col)
    imgdata <- checker_corner(CORNER_q2, 2, imgdata, outline_col)
    imgdata <- checker_edge(EDGE_right, "right", imgdata, outline_col)
    imgdata <- checker_corner(CORNER_q3, 3, imgdata, outline_col)
    imgdata <- checker_edge(EDGE_down, "down", imgdata, outline_col)
    imgdata <- checker_corner(CORNER_q4, 4, imgdata, outline_col)
    imgdata <- checker_edge(EDGE_left, "left", imgdata, outline_col)
    
    #making the animation and saving it to the file
    
    # getting the images that are saved in the file
    # plotting the images
    details = file.info(list.files(pattern="_plot"))
    details = details[with(details, order(as.POSIXct(mtime))), ]
    files = rownames(details)
    
    animation::saveGIF(
      expr = {
        for(i in 1:length(files)){
          im <- load.image(files[i])
          plot(im)
        }
      },
      movie.name = "outlining.gif"
    )
    
    # saving gif to file after speeding it up
    img <- magick::image_read("outlining.gif")
    img <- image_animate(img, fps = 10)
    
    image_write(image = img,
                path = "outlining.gif")
    
    
    # delete position images to refresh for running next time
    
    for (fn in files) {
      #Delete file if it exists
      file.remove(fn)
    }
   
    
    #####
    
    ggplot(imgdata, aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()
  })
  
  output$plot3 <- renderImage({

    list(src = "outlining.gif", contentType = "image/gif")
    
    
  }, deleteFile = FALSE)
  
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
