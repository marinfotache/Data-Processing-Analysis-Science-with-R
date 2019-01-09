############################################################################
###                         Al.I. Cuza University of Iași                ###
###            Faculty of Economics and Business Administration          ###
###       Department of Accounting, Information Systems and Statistics   ###
############################################################################
###
############################################################################
###             Data Processing/Analysis/Science with R                  ###
############################################################################
###
############################################################################
###                      1c. Data Structures in R                        ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/01%20R_Introduction_Data%20Structures/01c_Data_Structures.pptx
############################################################################
## last update: 28.09.2018



#############################################################################
###             				vectors in R
#############################################################################

## Vector is a collection of elements, all of the same type
##  There are not rows and columns in a vector, but just elements 
x <- 1
x <-  c(1, 3, 5, 7, 25, -13, 47)
x
class(x)

y <- c("one", "two", "three", "eight")  
y

z <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
z

w <- c( 1, 'unu', FALSE)
class(w)
w

# Vectors can also be created with a sequence
ten_integers.1 <- 5:14
ten_integers.1
# or
(ten_integers.2 <- seq(from=5, to=14, by=1))
#ten_integers.2

(ten_integers.3 <- 5:14)


# descending vector
seq(from=5, to=-5, by=-1)



# combine sequences and c function
a_vector <- c( 2:4, 8:14) 
a_vector [5]

## generating a vector with dates between 29 september and 2 october
# as "pure" dates 
seq(as.Date("2014/09/29"), by = "day", length.out = 4)
# or
seq(as.Date("2014/09/29"), as.Date("2014/10/02"), "days")

# as timestamps
seq(c(ISOdate(2014,9,29)), by = "DSTday", length.out = 4) 
#
x <- as.POSIXct("2014-09-25 23:59:59", tz="Turkey")
format(seq(x, by="day", length.out=8), "%Y-%m-%d %Z")
#
d1 <- ISOdate(year=2014,month=9,day=25,tz="GMT")
seq(from=d1,by="day",length.out=8)

# there are many other ways of creating vectors
# example: creates a vector object named x containing 
# five random values drawn from a standard normal distribution
x <- rnorm(5)
x
x <- rnorm(5)
x

# another example: creates a vector object named x.rep using
#   function "rep" (repeat): sequence (5, 7, 11) repeated three times
x.rep <- rep(c(5, 7, 11), 3)
x.rep

# compare with another version which uses "each" clause
x.rep.2 <- rep(c(5, 7, 11), each=2, times=3)
x.rep.2


## some vectors are build-in (system defined)
letters
LETTERS
month.name
state.name
state.area


#################################################################
###                         factors in R (part 1)

## factors are nominal variables whose values have a number of levels

## factors are the main way to represent a nominal scale variable


# ex: a vector containing genre of students group
#  initially, both vectors are strings
names <- c( "Popescu I. Valeria", "Ionescu V. Viorel", 
     "Genete I. Aurelia", "Lazar T. Ionut", 
     "Sadovschi V. Iuliana", "Dominte I. Nicoleta")
genre <- c("Female", "Male", "Female", "Male", 
     "Female", "Female" )
class(names)
class(genre)
unclass(names)
unclass(genre)

# as genre can have only two values, Female and Male, it is advisable
#  to convert genre into a factor 
genre <- as.factor(genre)
class(genre)
unclass(genre)

# if a non existing value is added in vector "genre", it is 
#   automatically converted back into character  
genre <- c(genre, "Boy")
class(genre)
unclass(genre)

# re-create and re-convert into factor the vector genre
genre <- c("Female", "Male", "Female", "Male", 
     "Female", "Female" )
genre <- as.factor(genre)

# ... to be continued


###################################################################
###  Useful functions for vectors; how to access vector elements

# display data type and values of vector elements
class(ten_integers.1)
unclass(ten_integers.1)
typeof(ten_integers.1)

# internally, factor levels are stored as integers
class(genre)
unclass(genre)
typeof(genre)


# return number of elements in a vector
length(ten_integers.1)

length(letters)


## qualifying (addressing) vector elements

# first element in vector ten_integers.1
ten_integers.1 [1]

# last element in vector ten_integers.1
ten_integers.1 [length(ten_integers.1)]

# first three elements in vector ten_integers.1
ten_integers.1 [1:3]
# last three elements in vector ten_integers.1
ten_integers.1 [(length(ten_integers.1)-2) : length(ten_integers.1)]

ten_integers.1 [ length(ten_integers.1) : (length(ten_integers.1)-2)]

# first, third, fifth and sixth elements in vector ten_integers.1
ten_integers.1 [c(1, 3, 5, 6)]


# first three and last three elements in vector ten_integers.1
ten_integers.1 [c( 1:3, (length(ten_integers.1)-2) : length(ten_integers.1))]

## indices of elements can be qualified with other vectors 
# first, third, fifth and sixth elements in vector ten_integers.1
ind <- c(1, 3, 5, 6)
ind
ten_integers.1
ten_integers.1 [ind]


## excluding some elements from vectors

# excluding first element
ten_integers.1 [-1]

# excluding first three elements
ten_integers.1 [-(1:3)]

# excluding first, third, anf fourth elements
ten_integers.1 [-(c(1,3,4))]

# excluding first three elements plus elements 6 amnd 8
ten_integers.1 [-(c(1:3,6,8))]

# excluding first two and last two elements
ten_integers.1 [-c((1:2), (length(ten_integers.1)-1) : length(ten_integers.1))]


## filtering 

# filter vector elements - select only elements greater than 10
ten_integers.1 [ten_integers.1 > 10]


for (i in 1:length(ten_integers.1))
{
     if (ten_integers.1[i] > 10)
          print(ten_integers.1[i])
}     



# how many elementes are greater than 10 ?
length(ten_integers.1 [ten_integers.1 > 10])

# display INDICES of elements greater than 10
which (ten_integers.1 > 10)

# filter vector elements - select only elements greater than 10 - ver 2
ind <- which (ten_integers.1 > 10)
ten_integers.1 [ind]

# sorting a vector
names <- c( "Popescu I. Valeria", "Ionescu V. Viorel", 
     "Genete I. Aurelia", "Lazar T. Ionut", 
     "Sadovschi V. Iuliana", "Dominte I. Nicoleta")
names <- sort(names)
names

# sorting a vector in descending order
names.desc <- rev(sort(names))
names.desc


###################################################################
###            R is a vectorized language

# one of the nicest thing about vectors: operations are automatically applied 
#   on each element of the vector without looping among vector elements
num.vec.1 <-  c(1, 3, 5, 7, 25, -13, 47)
num.vec.2 <- num.vec.1 + 100
num.vec.2

date.vec.1 <- c ("2013-10-01", "2013-10-03", "2013-10-10")
# for the moment, vector date.vec.1 is displayed as a string vector
date.vec.1
class(date.vec.1)
# use as.Date() to convert all of the vector elements into dates
date.vec.1 <- as.Date(date.vec.1)
date.vec.1
class(date.vec.1)


## operations can be applied on two or more vectors
num.vec.3 <- num.vec.1 + num.vec.2
num.vec.3

## compare a vector with a value
x
x >= 0
x.1 <- x >= 0
x.1

## testing if at least one of the vector elements fulfils the predicate 
##   function "any"
x
any(x > 0)

## testing if all the vector elements fulfill the predicate - function "all" 
all(x > 0)
all(x > -25)

## for a string (character) vector, display the number of characters 
##   for each element
y 
nchar(y)

## it is possible to provide a name for each vector element
num_ro = c (one = "unu", two="doi", three="trei", four="patru")
num_ro
# the same result can be accomplished with:
num_ro = c ("unu", "doi", "trei", "patru")
num_ro
names(num_ro) = c ("one", "two", "three", "four")
num_ro

###
###   Descriptive statistics functions can be applied to vectors

# create a vector (age) containing the age of 10 people
age <- c(1,3,5,2,11,9,3,9,12,3)
# create another vector (weight) containing the weight of above people 
#   (Kabacoff, 2011)
weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)
weight
# if Kabacoff were used US metric system, we had convert the weight 
#    from lbs into kg
weight.lbs <- weight
weight.kg <- weight.lbs * 0.454

# compute the mean of people's weight
mean.weight = mean(weight)
mean.weight

# compute the standard deviation of people's weight
sd.weight = sd(weight)
sd.weight

# compute correlation between age and weight
cor.age_weight = cor(age,weight)
cor.age_weight

# display a scatterplot of weight (y) vs. age (x)
plot(age,weight)



#############################################################################
###                                matrices in R
#############################################################################

##  a matrice is a bidimensional vector
##  all matrice elements must be of the same type 

#  m1 is a 5 x 4 matrix
m.1 <- matrix(1:20, nrow=5, ncol=4)
m.1

# m.2 is a 2 x 2 matrix, filled by rows
cells <- c(1,26,24,68)
rownames <- c("Row1", "Row2")
colnames <- c("Col1", "Col2") 
m.2 <- matrix(cells, nrow=2, ncol=2, byrow=TRUE, 
	dimnames=list(rownames, colnames))
m.2

# m.3 is a 2 x 2 matrix, filled by columns
# list is a data structure presented after data frame
m.3 <- matrix(cells, nrow=2, ncol=2, byrow=FALSE, 
	dimnames=list(rownames, colnames))
m.3


# m.4 is a 4 x 3 matrix, filled by rows
m.4 <- matrix(1:12, nrow=4, ncol=3, byrow=TRUE)
m.4
# naming rows: row.1, row.2, ...
# naming columns: col.1, col.2, ...
dimnames(m.4)=list(paste("row.", 1:nrow(m.4), sep=""), paste("col.", 1:ncol(m.4), sep=""))
m.4

###
## accessing matrix elements
m.1

# display the 3rd row
m.1[3,]

# display the 3rd column
m.1[,3]

# display the element at the intersection of the 2nd row and the 3rd column
m.1 [2,3]

# display two elements from the same row: m.1 [2,3] and m.1[2,4]
m.1 [2, c(3,4)]

# display three elements from the same column: 
#	m.1 [1,2], m.1 [2,2] and m.1[3,2]
m.1 [c(1,2, 3), 2]

# display a "submatrix", from m1 [2,2] to m2[4.4]
m.1 [ c(2,3,4), c(2,3,4)]


## calculations on rows and columns of the matrix
m.4

# compute mean of all the cells in matrix m.4
mean(m.4)

# compute mean of all the cells on the third column
mean(m.4[,3])

# compute mean of all the cells on the third rows
mean(m.4[3,])

# compute sum of all the cells in matrix m.4
sum(m.4)

# compute sum of all the cells on the third column
sum(m.4[,3])

# compute sum of all the cells on the third row
sum(m.4[3,])

# compute sum of all the cells in matrix m.4
sum(m.4)


# rowSums calculated the sums of the cells for each row of a matrix
rowSums(m.4)

# colSums calculated the sums of the cells for each column of a matrix
colSums(m.4)

# rowMeans/colMeans calculate mean of the every row/column
rowMeans(m.4)
colMeans(m.4)

##
# adding total rows and columns to the matrix 
m.4

# add total column
m.4 <- cbind(m.4, rowSums(m.4))
# setting the name for the total column
column.names <- colnames(m.4)
column.names
column.names[length(column.names)] <- "col.total"
colnames(m.4) <- column.names
# check the operation
m.4

# add total row
m.4 <- rbind(m.4, colSums(m.4))
# setting the name for the total column
row.names <- rownames(m.4)
row.names
row.names[length(row.names)] <- "row.total"
rownames(m.4) <- row.names
# check the operation
m.4



#############################################################################
###                  			arrays in R
#############################################################################

dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
a1 <- array(1:24, c(2, 3, 4), dimnames=list(dim1, dim2, dim3))
a1
class(a1)
typeof(a1)
unclass(a1)

# display element [2,2,3]
a1 [2,2,3]

# display a matrix from elements of A and B for first row/column of C
a1 [,,1]

# display elements of A for the 3rd "row" of B and 2nd row/columns of C
a1 [,3,2]

# display a subarray containg all elements from 
#	first two rows/columns of A, B and C
a1 [c(1,2),c(1,2),c(1,2)]



############################################################################
###       data frames in R (the most important data structure in R       ###
############################################################################

## data frames look like tables (or Excel worksheets)
## each column of a data frame is a vector (with the same type of values)
## in statistical language, in a data frame:
##	- each row is an abservation
##	- each column is a variable


####            creating  data frames

## create an empty data frame
student_gi <- data.frame(studentID = numeric(), 
     name = character(), age = numeric(), 
     scholarship = character(), 
     lab_assessment = character(), 
     final_grade = numeric())
class(student_gi)
str(student_gi)

## create a data frame from vectors
studentID <- 1:5
name <- c("Popescu I. Vasile", "Ianos W. Adriana", 
     "Kovacz V. Iosef", "Babadag I. Maria", 
     "Pop P. Ion")
age <- c(23, 19, 21, 22, 31)
scholarship <- c("Social", "Studiu1", "Studiu2", 
     "Merit", "Studiu1")
lab_assessment <- c("Bine", "Foarte bine", 
     "Excelent", "Bine", "Slab")
final_grade <- c(9, 9.45, 9.75, 9, 6)


student_gi <- data.frame(studentID, name, age, 
     scholarship, lab_assessment, final_grade, stringsAsFactors = FALSE)

View(student_gi)
str(student_gi)


# compare with ....
library(tidyverse)
student_tibble <- tibble(studentID, name, age, 
     scholarship, lab_assessment, final_grade)
str(student_tibble)
glimpse(student_tibble)
student_tibble

# delete vectors which formed the data frame
rm(studentID, name, age, scholarship, lab_assessment, final_grade)


# display one column of the data frame as a vector
student_gi$name

# display one column of the data frame as a... column
student_gi["name"]

# confirm that student_gi is a data frame
class(student_gi)

# display structure of the data frame
str(student_gi)

# display type of invididual variables within data frame
class(student_gi$studentID)
class(student_gi$name)

names(student_gi)


# display first two columns (studentID and name )
student_gi [1:2]
# or
student_gi [ , 1:2]
#or
student_gi [c("studentID", "name")]
#or
student_gi [, c("studentID", "name")]
# or
cols <-  c("studentID", "name")
student_gi[cols]
# or
student_gi[, names(student_gi) %in% cols] 



# return "final_grade" variable (column) as a vector
student_gi$final_grade
# or
student_gi[ , 6]
# or
student_gi[ , "final_grade"]


# return "final_grade" variable (column) as a one-column data frame
student_gi[ , "final_grade", drop=FALSE]


# display first two observations (rows)
student_gi [1:2,]

# display observations 1, 2 and 5
student_gi [c(1:2, 5), ]


# cross-tabulate (sort of pivot table) lab_assessment by final_grade
table (student_gi$lab_assessment, student_gi$final_grade)

# summary statistics of final_grade
summary(student_gi$final_grade)

# plot 
plot(student_gi$age, student_gi$final_grade)


##
# attach() function adds the data frame to the R search path. 
search()
# When a variable name is encountered, data frames in the search path are checked 
#   in order to locate the variable.

# previous commands but with attach
attach(student_gi)
search()
final_grade
table (lab_assessment, final_grade)
summary(final_grade)
plot(lab_assessment, final_grade)
plot(age, final_grade)

# detach removes an objects from the search path
detach(student_gi)
search()


# it is advisable to use "with" instead of "attach"
with (student_gi, final_grade)
with (student_gi, table (lab_assessment, final_grade))
with (student_gi, plot(lab_assessment, final_grade) )


##
# case identifiers can be specified with a "rowname" option 
#	within the "data.frame" function
# new values for studentID (to avoid confusion with row numbers)
studentID <- c(1001, 1002, 1003, 1004, 1005)
name <- c("Popescu I. Vasile", "Ianos W. Adriana", "Kovacz V. Iosef", 
	"Babadag I. Maria", "Pop P. Ion")
age <- c(23, 19, 21, 22, 31)
scholarship <- c("Social", "Studiu1", "Studiu2", "Merit", "Studiu1")
lab_assessment <- c("Bine", "Foarte bine", "Excelent", "Bine", "Slab")
final_grade <- c(9, 9.45, 9.75, 9, 6)

#  (slightly) new version of the data frame
student_gi <- data.frame(studentID, name, age, scholarship, lab_assessment, 
	final_grade, row.names = studentID)
# studentID is the variable to use in labeling cases on various printouts and
# graphics produced by R.

# display the name of the rows (observations)
rownames(student_gi)

rm(studentID, name, age, scholarship, lab_assessment, final_grade)

student_gi
# notice the leftmost column

# display the observation (row) corresponding to student Ianos W. Adriana 
#  using her case identifier ("1002")
student_gi["1002",]

# display the observations corresponding to students Ianos W. Adriana 
#  and Pop P. Ion using their case identifier ("1002" and "1005")
student_gi[c("1002", "1005"),]


######################################################################
###   currently, the preferred version of data.frame is 
###   `tibble` (see the tidyverse - later on this course)


######################################################################
###                         factors in R (part 2)

# nominal variable
scholarship <- c("Social", "Studiu1", "Studiu2", "Merit", "Studiu1")

# factor function
scholarship_f <- factor(scholarship)
scholarship_f

# ordinal variable
lab_assessment <- c("Bine", "Foarte bine", "Excelent", "Bine", "Slab")
lab_assessment
lab_assessment <- factor(lab_assessment, order=TRUE, levels=c("Slab", 
	"Bine", "Foarte bine", "Excelent"))
lab_assessment

# re-create the data frame using factors
studentID <- c(1001, 1002, 1003, 1004, 1005)
name <- c("Popescu I. Vasile", "Ianos W. Adriana", "Kovacz V. Iosef", 
	"Babadag I. Maria", "Pop P. Ion")
age <- c(23, 19, 21, 22, 31)
scholarship <- c("Social", "Studiu1", "Studiu2", "Merit", "Studiu1")
scholarship <- factor(scholarship)

lab_assessment <- c("Bine", "Foarte bine", "Excelent", "Bine", "Slab")
lab_assessment <- factor(lab_assessment, order=TRUE, levels=c("Slab", 
	"Bine", "Foarte bine", "Excelent"))

final_grade <- c(9, 9.45, 9.75, 9, 6)

#  another version of the data frame
student_gi <- data.frame(name, age, scholarship, lab_assessment, 
	final_grade, row.names = studentID)

# structure of the data frame
str(student_gi)

# basic statistics about variables in data frame
summary(student_gi)



# factors and value labels for categorical variables
patientID <- c(1, 2, 3, 4) 
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
diabetes <- factor(diabetes)
status <- factor(status, order=TRUE)
gender <- c(1, 2, 2, 1)
patientdata <- data.frame(patientID, age, diabetes, status, gender)

# A variable named gender, which is coded 1 for male and 2 for female. 
# We create value labels with the code
patientdata$gender <- factor(patientdata$gender, 
                    levels = c(1,2),labels = c("male", "female"))
# levels indicate the actual values of the variable
# and labels refer to a character vector containing the desired labels.

patientdata

str(patientdata)



# factors can be of numerical type
some.numbers <- c(1, 2, 3, 2, 3, 1, 3, 2, 2, 2, 1, 1, 3)
class(some.numbers)
some.numbers <- some.numbers + 10
some.numbers

some.numbers <- as.factor(some.numbers)
some.numbers
some.numbers <- some.numbers + 10
# Warning message:
# In Ops.factor(some.numbers, 10) : ‘+’ not meaningful for factors
some.numbers



####################################################
###   data frames used in further scripts


## data frame for enrollment at internationalized master programmes at FEAA
program = c(rep("ADL",7), rep("FRM",5), rep("SAAS", 7))
academicyear = c (2009, 2010, 2011, 2012, rep(c(2010, 2011, 2012), 2), 
     2011, 2012, 2009, 2010, 2011, 2012, 2010, 2011, 2012)
studyyear = c(rep("Anul I",4), rep("Anul II",3),rep("Anul I",3), rep("Anul II", 2),
                rep("Anul I",4), rep("Anul II", 3) )
nofstuds = c(19, 28, 49, 48, 17, 27, 49, 15, 20, 26, 15, 19, 9, 13, 0, 0, 9, 12, 0)

# the data frame
mpi <- data.frame(program, academicyear, studyyear, nofstuds, stringsAsFactors=TRUE)
mpi
str(mpi)
head(mpi)
tail(mpi)


# remove the vectors (we'll work only with the data frame)
rm(program, academicyear, studyyear, nofstuds)



############################################################################
###                            lists in R                                ###
############################################################################

## the most complicated and also the most flexible data structure
## hold arbitrary objects of different types
## many functions/statistical models return the results in a list form


# first example: current timestamp
t = Sys.time()
# a POSIXlt object is actually a list
l.1 <- as.POSIXlt(t)
l.1
typeof(l.1)
names(l.1)
unclass(l.1)

# extract list components values
# seconds, minutes, hours, ...
# eqivalent to l.1$sec, l.1$min ... is:
l.1[[1]]
l.1[[2]]
l.1[[3]]
l.1[[4]]
l.1[[5]]
l.1[[6]]
l.1[[7]]
l.1[[8]]
l.1[[9]]

# see horizontally the components of the timestamp-list object
unlist(l.1)


# we already saw that for a matrix, dimension names (dimnames) is a list
rownames
colnames
m.3 <- matrix(cells, nrow=2, ncol=2, byrow=FALSE, 
     dimnames=list(rownames, colnames))
m.3
dimnames(m.3)
unlist(dimnames(m.3))


## creating other lists
list.1 = list ("unu", "doi", "trei")
list.2 = list( c("doi", "trei", "patru"))
list.1
list.2
list.3 = list (list.1, list.2, 3:7, patientdata)
list.3
# list.3 consists of four objects:
#  - first is a list with three string objects
#  - second id a vector of three strings
#  - third is a vector with five numbers
#  - fourth is a data frame

# display the structure (content) of a list
str(list.3)

# display the number of objects in a list
length(list.3)

## access the first object of the list
list.3[[1]]
class(list.3[[1]])
# ... the second
list.3[[2]]
class(list.3[[2]])

# ... and the fourth
list.3[[4]]
class(list.3[[4]])

# the first object of list.3 is a list without name:
names(list.3[[1]])

# ... but the fourth object is the data frame called "patientdata"
names(list.3[[4]])


## display the third object of the first (list) object in "list.3" 
list.3[[1]][[3]]

## display "age" variable of "patientdata" data frame
# the data frame is the fourth object of "list.3", 
#   and "age" is the second column of the data frame
list.3[[4]][, 2]
# or
list.3[[4]][, "age"]

## display "age" variable of "patientdata" data frame but this time as a column
list.3[[4]][, "age", drop=FALSE]

## display age of third patient 
list.3[[4]][, 2][3]
# or
list.3[[4]][, "age", drop=FALSE]$age[3]


############################################################################
#####                         Tables                                  ##### 
############################################################################
# Note: Tables are not treated as a full-fledged data structure, but as a 
#   kind of arrays

# Some functions (e.g. graphic functions, categorical data analysis
#   functions) take as argument a table

# Discussion about tables will be deepened in script 06c 


# There are two main types of tables:
#    - tables of frequencies counts number of occurences for each  
#       value of a (usually) categorical variable
#    - tables of proportions which divides number of occurences of each 
#       value to total number of occurences of a (usually) categorical 
#        variable


################################
###   Uni-dimensional tables

#
student_gi

# create a table with frequencies of "scholarship" in data frame "student_gi"
table.1 <- with(student_gi, table(scholarship))
table.1

# structure of table.1
str(table.1)
class(table.1)

# unidimensional tables are vectors with labeled elements (each element's 
#   label is a value of the attribute used in function "table")
names(table.1)

# tables.1 is not a data frame, so we cannot qualify the variable using $...
table.1$Merit
# ... but we can access with vector indices
table.1[1]
# ... or list indices
table.1[[1]]

# display both the name/label and the value of the 3rd element of the table table.1:
table.1[3]
# or
unlist(table.1)[3]

# display only the name/label of the 3rd element of the table table.1:
names(table.1) [3]

# display only the value of the 3rd element of the table table.1:
unlist(table.1)[[3]]


# display both the name/label and the value of the 3rd element of the table table.1,
#   knowing the names of the element:
table.1["Studiu1"]

# display both the names/labels and the values of two elements of the table table.1,
#   knowing their names:
table.1[c("Merit", "Studiu1")]



################################
###   Bi-dimensional tables

student_gi

# create a contingency (pivot) table with frequencies of 
#  "scholarship by" "lab_assessment"
table.2 <- with(student_gi, table(scholarship, lab_assessment))
table.2

# structure of table.2
str(table.2)
class(table.2)

# any cell can be accessed using indices of row and column...
table.2[1, 2]
# ... or the names/labels
table.2["Merit", "Bine"]

## display the second column (associated with value "Bine" of "lab_assessment")
##     as a vector
# one can use the index of the column (2)...
table.2[, 2]
# ... or the label/name of the column ("Bine")
table.2[, "Bine"]

## the same can be done with rows

##  one can access particular rows and columns in a table
table.2[c("Merit", "Studiu1"), c("Slab", "Excelent")]



################################
###   three-dimensional tables
student_gi

# create a three-dimensional table with frequencies of 
#  "scholarship" by" "lab_assessment" by "final_grade"
table.3 <- with(student_gi, table(scholarship, lab_assessment, final_grade))
table.3

# ftable improves the display of three-dimensional tables
ftable(table.3)


# any cell can be accessed using indices of the three axes...
table.3[3, 3, 3]
# ... or the names/labels
table.3["Studiu2", "Excelent", "9.75"]


## display, as an one-dimensional table, the values of the lab_assessment
##  which corespond to value "Studiu2" (4th) of "scholarship" and
##    value "9.75" (4th) of "final_grade" 

# one can use the indexes ...
table.3[4, , 4]
# ... or the labels/names
table.3[ "Studiu2", , "9.75" ]


## display, as a bi-dimensional table, the values of the first ("scholarship")
##  and the third ("final_grade") axes associated with the 4th value ("Excelent")
##  of the second axis ("lab_assessment")   
# one can use the index...
table.3[, 4, ]
# ... or the label/name ("Excelent")
table.3[, "Excelent", ]


##  one can access particular ranges on each axis
table.3[c("Merit", "Studiu1"), c("Slab", "Excelent"), c("9.45", "9.75") ]




############################################################
###            Data structures conversion

## convert a vector into a data frame
a_vector
v_to_df.1 <- as.data.frame(a_vector)
v_to_df.1
str(v_to_df.1)


## convert a matrix into a data frame
m.4
m_to_df.1 <- as.data.frame(m.4)
m_to_df.1
str(m_to_df.1)
names(m_to_df.1)
rownames(m_to_df.1)


# convert a table into a data frame
table_to_dataframe = data.frame(unlist(HairEyeColor))
head(table_to_dataframe, 5)

# convert a list into a data frame
df <- data.frame(matrix(unlist(list.1), nrow=132, byrow=T))
head(df,5)
# Only some simple lists can be converted into tables 
