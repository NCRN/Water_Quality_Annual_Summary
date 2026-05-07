# a script to generate the template csvs
# for congruency checks

# this is a script, not a function to be sourced into the main program

wqp <- read.csv('data/wqp.csv', nrow=5)
write.csv(wqp, 'templates/wqp.csv', row.names=F)

metadata <- read.csv('data/wqp_ncrnwater_metadata.csv', nrow=5)
write.csv(metadata, 'templates/wqp_ncrnwater_metadata.csv', row.names=F)
