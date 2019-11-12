library(ggplot2)
# install.packages("ggpubr")
library(ggpubr)
# install.packages("scales")
library(scales)
# install.packages("directlabels")
library(directlabels)

d_end <- t(table.3.3[1,])

quartz()
small.donations <- ggplot(data = table.3.3, aes(x = Year)) + 
  geom_line(aes(y = table.3.3$`Business Associations`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Public Sector Unions`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Industrial Unions`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Non-Profit Institutions`), colour = "black", size = 0.75) +
  geom_line(aes(y = table.3.3$Retired), colour = "blue", size = 0.75) +
  ylab("Number of Donations") +
  ggtitle("Retired") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11)) +
  scale_y_continuous(trans ='log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x)))
small.donations2 <- ggplot(data = table.3.3, aes(x = Year)) + 
  geom_line(aes(y = table.3.3$Retired), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Business Associations`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Public Sector Unions`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Industrial Unions`), colour = "black", size = 0.75) +
  geom_line(aes(y = table.3.3$`Non-Profit Institutions`), colour = "blue", size = 0.75) +
  ylab("Number of Donations") +
  ggtitle("Non-Profit Institutions") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11)) +
  scale_y_continuous(trans ='log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x)))
small.donations3 <- ggplot(data = table.3.3, aes(x = Year)) + 
  geom_line(aes(y = table.3.3$`Non-Profit Institutions`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$Retired), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Business Associations`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Public Sector Unions`), colour = "black", size = 0.75) +
  geom_line(aes(y = table.3.3$`Industrial Unions`), colour = "blue", size = 0.75) +
  ylab("Number of Donations") +
  ggtitle("Industrial Unions") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11)) +
  scale_y_continuous(trans ='log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x)))
small.donations4 <- ggplot(data = table.3.3, aes(x = Year)) + 
  geom_line(aes(y = table.3.3$`Industrial Unions`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Non-Profit Institutions`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$Retired), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Business Associations`), colour = "black", size = 0.75) +
  geom_line(aes(y = table.3.3$`Public Sector Unions`), colour = "blue", size = 0.75) +
  ylab("Number of Donations") +
  ggtitle("Public Sector Unions") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11)) +
  scale_y_continuous(trans ='log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x)))
small.donations5 <- ggplot(data = table.3.3, aes(x = Year)) + 
  geom_line(aes(y = table.3.3$`Public Sector Unions`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Industrial Unions`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$`Non-Profit Institutions`), colour = "black", size = 0.75) + 
  geom_line(aes(y = table.3.3$Retired), colour = "black", size = 0.75) +
  geom_line(aes(y = table.3.3$`Business Associations`), colour = "blue", size = 0.75) +
  ylab("Number of Donations") +
  ggtitle("Business Associations") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11)) +
  scale_y_continuous(trans ='log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x)))
quartz()
ggarrange(small.donations, small.donations2, small.donations3, small.donations4, small.donations5)

########
# An image based on a raster-format is comprised of a certain number of pixels. Depending on the size and quality
# of the image the number of pixels varies. The higher the density of pixels per inch, the higher the quality. Due 
# to this characteristic an image in a raster-based format has the advantage to include rich and complex color blend.
# This property is especially useful for photographs and its editing. On the other hand, the disadvantage in the application
# of a raster-based image lies in its limitation of scalability. When scaling a raster-based image, each pixel becomes larger
# or software tries to compensate the scaling effect by adding pixels. This leads to the result that the image becomes 
# blurry. Additionally, the file-size of images in the raster-based format are comparably large.
# An image in a vector-based format consists of paths which are generated by mathematical formulas. These paths are 
# defined in terms of two dimensional points which are connected by lines or curves. This property leads to the possibility
# of infinite scalability without quality loss. Thus, such images are very useful to create logos, drawings or illustrations.
# The construction method has as a result that it is very tedious to develop true-life images as e.g. a photography. While 
# an image in a vector-based format can be easily transformed to a raster-based format, it is difficult in reverse. 
# On the basis of this analysis, we would propose to store and share this figure in the vector-based format due to two
# reasons. First, the figure is an illustration of a graph and does not depict a photography or a true-life image respectively. 
# Thus, the advantage of being able to include rich and complex color blends is of no use in our case and results in a saving of
# storing capacity. Furthermore, we appreciate the opportunity to freely scale the figure without losing quality. This is 
# especially useful when sharing the figure or using it for different purposes, as e.g. in a research report, on a presentation slide
# or on a poster. 
