# a script to generate the template csvs
# for congruency checks

# this is a script, not a function to be sourced into the main program

wqp <- read.csv('wqp.csv', nrow=5)
write.csv(wqp, 'templates/wqp_template.csv', row.names=F)

metadata <- read.csv('wqp_ncrnwater_metadata.csv', nrow=5)
write.csv(metadata, 'templates/wqp_ncrnwater_metadata_template.csv', row.names=F)
