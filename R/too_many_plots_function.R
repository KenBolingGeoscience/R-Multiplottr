#' multiplotter
#'
#' This function is used to produce a quick series of charts based on correlation coefficents.  It
#' can be used to get a quick overveiw of a dataset and what variables display some co-variation.  It is a 
#' work in progress, and will eventually be able to do more advanced things than it can at the moment.
#' 
#' The initial idea for this project was to create a function which would simply print
#' a whole bunch of x,y plots at once.  However, upon further investigation this won't be that 
#' useful when you have more than 10 variables (which would print 45 individual plots at once).  
#' Consequently I decided that it would be nessicary to narrow down the results a bit so things don't get
#' out of hand.  I decided the best way would be to calculate the pearson coeffiecints 
#' and use that to figure out the best plots to use.
#'   
#' @param input_data a dataframe containing any number of variables
#' @param cutoff a positive number betwen 0 and 1 which is the lower limit of poitive or negative coefficents
#' @param dataset_name The name of the data set used.  This is important for keeping track of everything
#' @keywords stats
#' @export
#' @examples 
#' too_many_plots(input_data = mtcars, cutoff=0.75, dataset_name = "mtcars")


too_many_plots <- function(input_data, cutoff, dataset_name) {
  
  #First I'll try my best to sanitize the inputs, or at least put up warnings
  if(is.data.frame(input_data)!= TRUE)
    stop("Bro do you know what a dataframe is?") 
  
  if(is.character(dataset_name)!= TRUE)
    stop("Please change the name to a character, or try putting the name in quotes")
  
  if(cutoff > 1 | cutoff < 0)
    stop("please use a number between 0 and 1 for the cutoff")
  
  #Now to make the Pearson correlation coefficents using the cor() function:
  cor_cof_int <- as.data.frame(cor(input_data, method = c("pearson")))
  
  #The first data product is a heat map of the Pearson coefficients, there's more than one way to do this
  #and it looks like the tutorial here is probably better so I will probably replace this with that:
  #http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization  
  
  #for now I'm just using a basic one, it's not pretty but it's a quick way to visualize the dataset.  
  #with the more advanced version above I'll have more control over it.  Right now it's still a bit
  #rough, you
  
  #the heatmap function needs a matrix as an input
  
  cor_cof_int.matrix <- data.matrix(cor_cof_int)
  
  #This saves a .png of the heatmap  
  
  png(filename = paste(dataset_name, "Pearson.png"), 
      width = 6, height = 6, units = "in", res = 600)
  
 #this is the actual setup for the heat map
  
   heatmap(cor_cof_int.matrix, 
          Rowv=NA, 
          Colv="Rowv", 
          col = topo.colors(256), 
          revC=TRUE, 
          symm =TRUE, 
          scale="none", 
          margins=c(5,5), 
          main = dataset_name)
  
  dev.off()           #this is critically important for reasons I don't fully understand
  
  
  #print(cor_heatmap) #this will hopfully make it show up on the .md file in markdown, or not
  
  
  
  #this selects the coefficients that are above the cutoff value provided by the user
  #It also selects both positive and negative coefficients so that the user only has to input
  #one number: (cutoff=0.85 selects coefficients over .85 and under -.85)
  #It also limits the selection to the bottom half of the cor_cof_int data.frame by
  #selecting the values where the row number is greater than the column number 
  
  selection <- which( (cor_cof_int >= cutoff | cor_cof_int <= cutoff*-1) &
                        row(cor_cof_int) > col(cor_cof_int),arr.ind=TRUE)
  
  #This selects the row and column names that correspond with the index numbers from the previous step
  #This could probably be piped but I don't want to touch it since it's actually working!!
  ggplot_input <- data.frame(
    rownames(cor_cof_int)[selection[1:nrow(selection), 1]], 
    colnames(cor_cof_int)[selection[1:nrow(selection), 2]], 
    stringsAsFactors = FALSE)
  
  #^^ This is working for now don't touch!!
  
  #Next the ggplot_input goes into a for loop that runs from 1 to the number of rows in the 
  #selected values.   
  
  for (i in 1:nrow(selection)) {     
    
    #Declare a few objects which depend on "i".  These go into several parts of the loop 
    x_something <- as.character(ggplot_input[i,1])
    y_something <- as.character(ggplot_input[i,2])
    plot_number <- i
    
    #The aes_string argument is great for putting together stings/characters 
    output_plot <- ggplot() +
      geom_point(data = input_data, 
                 aes_string(x=x_something, 
                            y=y_something))+
      #adds a linear trend line
      geom_smooth( data = input_data, 
                   method = lm, 
                   se = FALSE, 
                   fullrange = TRUE, 
                   aes_string(x=x_something, 
                              y=y_something))+
      #the title is customized to display where the data came from and what is being compared
      ggtitle(paste("plot #", 
                    plot_number, 
                    paste(x_something, y_something, sep=" vs "), 
                    sep = " " ))+
      #label the x and y axes
      ylab(y_something)+
      xlab(x_something)+
      
      #finally the resulting plot is saved as a costomized .png file in the working directory, I would ideally like to change thing so that
      #it creates a new subdirectory with same name as the dataset but that will have to wait for the next release.
      ggsave(file= paste(dataset_name, 
                         "plot", 
                         plot_number, 
                         paste(x_something, y_something, sep="_vs_"),
                         ".png", sep = "_" ), 
             plot = last_plot(), 
             device = "png", 
             width=20, 
             height=16, 
             units = "cm")
    
    
    
  }
}
