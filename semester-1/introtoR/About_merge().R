
load(url("http://www.stats.gla.ac.uk/~levers/rp/children_classes.RData"))
head(children)
head(classes)

# Constructing smaller data frames to test what merge() is doing
children1 = children[1:5, ]
classes1  = classes[1:4, ]
children2 = children[c(1,100,1000,2000,2200),]
classes2  = classes[c(1, 50, 70, 100),]

dim(children1); dim(classes1); dim(children2); dim(classes2)
# note that children1, children2(5x4) and classes1, classe2(4x3) have different sizes

###########
## MERGE ##
###########
# Merge without any additional argument: it will merge both datasets 
# using "class" (the column both data frames have in common)
data.merge1 = merge(children1, classes1); data.merge1
data.merge2 = merge(children2, classes2); data.merge2
# merge() keeps all columns that have different names and uses "class" to 
# join both data frames. The way the later is done is by merging columns 
# where the value of "class" is the same (180 in both examples). So in data.merge1,
# we get children1 with additional columns classes$size and classes$combined for
# classes$class == 180. The output in data.merge2 is different because there is
# only one element in "class" that is the same in children2 and classes2 (again 180)
# so data.merge2 contains only one row, corresponding to children2 and classes2 merged
# for class == 180.

###################
## MERGE with BY ##
###################
# Imagine we have additional columns in children and classes that have the same name
# but are not related, so we don't want merge() to use that column to joint the data frames
children$samename = 1:2287
classes$samename = 1:133

# Constructing smaller data frames again
children1 = children[1:5, ]
classes1  = classes[1:4, ]
children2 = children[c(1,100,1000,2000,2200),]
classes2  = classes[c(1, 50, 70, 100),]
# We want to merge the data frames using the column "class" BUT NOT the column "samename."
# The resulting data frames should be the same as data.merge1 and data.merge1, with the 
# additional column "samename"
data.mergeby1 = merge(children1, classes1, by = "class"); data.mergeby1
data.mergeby2 = merge(children2, classes2, by = "class"); data.mergeby2

# Note what happens if we don't specify the column using "by":
data.mergenoby1 = merge(children1, classes1); data.mergenoby1
data.mergenoby2 = merge(children2, classes2); data.mergenoby2
# Here, R joined the data frames by merging columns where the value of "class" AND
# "samename" are the same. 

##############################
## MERGE with BY.X and BY.Y ##
##############################
# If we do
data.mergebyxy = merge(children1, classes1, by.x = 'SES', by.y = 'size'); data.mergebyxy
# R will assume that children1$SES and classes1$size are related (have the same data),
# so it will try to merge the data frames by merging columns where the value of 
# children1$SES AND classes1$size are the same. Since children1$SES and classes1$size 
# do not coincide in any entry (children1$SES = 23 10 15 23 10 while 
# classes1$size = 29 19 25 31, all different), the resulting data frame is null.

# Let's create additional rows for children1 and classes1 with the same value for
# children1$SES and classes1$size:
children1[6, ]   = children1[5, ]
classes1[5, ]    = classes1[1, ]
children1$SES[6] = classes1$size[5]

# Now, children1$SES and classes1$size share the value '29', so merge() will now
# merge the data frames by merging columns where the value of children1$SES AND classes1$size are the same
data.mergebyxy = merge(children1, classes1, by.x = 'SES', by.y = 'size'); data.mergebyxy







