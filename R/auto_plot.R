library(magick)


p1 <- image_read("images/auto_plot/auto_plot.png")
p2 <- image_read("images/auto_plot/auto_plot2.png")
p3 <- image_read("images/auto_plot/auto_plot3.png")
p4 <- image_read("images/auto_plot/auto_plot4.png")
p5 <- image_read("images/auto_plot/auto_plot5.png")


all_images <- c(p1,p1,p2,p2,p3,p3,p4,p4,p5,p5)


auto_plot <- image_animate(all_images, fps = 1, dispose = "previous")


image_write(auto_plot, "images/auto_plot2.gif", quality = 100)

#system("convert -delay 80 *.png example_1.gif")

