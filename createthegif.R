# time at work gif

#crearing working directory
setwd("output/gif/") 

# Converting .png files in one .gif image using ImageMagick
system("/opt/local/bin/convert -delay 80 *.png timeatwork.gif")
# Remove .png files from working directory
file.remove(list.files(pattern=".png"))