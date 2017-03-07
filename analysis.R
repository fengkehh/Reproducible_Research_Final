if (!file.exists('StormData.csv.bz2')) {
    download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', 
                  destfile = 'StormData.csv.bz2')
}

if (!('data' %in% ls())) {
    data <- read.csv('StormData.csv.bz2')
}