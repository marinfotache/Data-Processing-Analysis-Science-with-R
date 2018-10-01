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
###                         1b. R Main Data Types                        ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/01%20R_Introduction_Data%20Structures/01b_Main%20Data%20Types.pptx
############################################################################

## last update: 28.09.2018



############################################################################
###                                Characters (strings)                  ###
a_char_var = "Just an example of a character variable"
a_char_var <- "Just an example of a character variable"
a_char_var
print(a_char_var)
class(a_char_var)

another_one <- c('one', 'five', 'twenty')
another_one [2]
x = "ana are mere"
"gigi" -> y

another_char_var <- "Another example of a character variable"
class(another_char_var)

###
##             same basic functions for dealing with strings

# function which return the number of characters in a string
nchar(a_char_var)  # be careful, it is NOT length, as in other programming languages
nchar(another_one)

# extract part of a string
substr(a_char_var, 9, 15)
# or
substr(a_char_var, start=9, stop=15)

cnp <- '1821004390802'

# get the gender digit
substr(cnp, 1, 1)


# string concatenation
paste("a_char_var:",  a_char_var, "~", "another_char_var:",
     another_char_var,  sep=" " )



############################################################################
###                               Real Numbers                           ###

a_num_var <- 75.53
class(a_num_var)

# even when assigning integers, the variable type is actually numeric 
another_num_var <- 75
class(another_num_var)

class(65/4)

## sume basic functions for dealing with numbers

# extract the integer from a numeric (real) value 
floor(a_num_var)  
# or
as.integer(a_num_var)  
# or
trunc(a_num_var,0)  

# rounding
round(a_num_var,2)  
round(a_num_var,1)  
round(a_num_var,0) 
round(a_num_var,-1)  

# division
19 / 17

# modulus
19 %% 17

# integer division
19 %/% 17

# extract the square root
sqrt(144)



############################################################################
###                                  Integers                            ###

# declaring integers requires letter L
an_int_var <- 14575L
class(an_int_var)


## some basic functions for dealing with integers

# convert a numeric (real) value into integer 
as.integer(12345.6789)

# "rounding" integers 
round(an_int_var,-1)  
round(an_int_var,-2)  
round(an_int_var,-3)  


############################################################################
###                             Complex numbers                         ###

# declaring complex numbers requires letter i
a_complex_var <- 825 + 3i
class(a_complex_var)

# well, that's all for the complex numbers :)



############################################################################
###                                    Dates                             ###

# internally, dates are represented as integers elapsed since 
#  "origin" (default origin is 1st of January 1970)
as.Date(16401, origin = "1970-01-01")

# the default format for declaring dates is yyyy-mm-dd. 
a_date_var <- as.Date("2014-10-01")
a_date_var
class(a_date_var)


# the default format can be replaced with an user-defined one
# for now, the type of variable "another_date_var" will be character
another_date_var <- "12/25/2014"
another_date_var
class(another_date_var)
# convert character to date using a more rigorous format
another_date_var <- as.Date(another_date_var, "%m/%d/%Y")
another_date_var
class(another_date_var)

##      Format parameters  
# %a Abbreviated weekday name in the current locale.
# %A Full weekday name in the current locale.
# %b Abbreviated month name in the current locale.
# %B Full month name in the current locale.
# %d Day of the month as decimal number (01–31).
# %H Hours as decimal number (00–23). Times such as 24:00:00 are accepted
# %I Hours as decimal number (01–12).
# %j Day of year as decimal number (001–366).
# %m Month as decimal number (01–12).
# %M Minute as decimal number (00–59).
# %p AM/PM indicator in the locale. Used in conjunction with %I and not with %H.
# %S Second as decimal number (00–61), allowing for up to two leap-seconds 
#  (but POSIX-compliant implementations will ignore leap seconds).
# %U Week of the year as decimal number (00–53) using Sunday as the first 
#   day 1 of the week (and typically with the first Sunday of the year 
#   as day 1 of week 1). The US convention.
# %w Weekday as decimal number (0–6, Sunday is 0).
# %W Week of the year as decimal number (00–53) using Monday as the first 
#   day of week (and typically with the first Monday of the year as 
#    day 1 of week 1). The UK convention.
# %y Year without century (00–99). On input, values 00 to 68 are prefixed 
#    by 20 and 69 to 99 by 19
# %Y Year with century. Note that whereas there was no zero in the original 
#  Gregorian calendar, ISO 8601:2004 defines it to be valid (interpreted as 1BC)
# get the current date with Sys.Date()
Sys.Date()

# get the current timestamp with date()
date()

# change to date format (for display; this does not affect internal storage)
today <- Sys.Date()
format(today, format="%d %B %Y")

# display day of the week
format(today, format="%A")


# Internally dates are integer representation
#   number of days since 01-01-1970
unclass(a_date_var)
# this is the same as 
a_date_var - as.Date("1970-01-01")


# calculate the difference between two dates
today <- Sys.Date()
dob <- as.Date("1956-10-12")
age <- today - dob
# the difference is displayed in days
age
# variable age is of class "difftime"
class(age)

# Unfortunately, difftime works only for "secs", "mins", 
#   "hours", "days", and "weeks"
difftime(today, dob, units="weeks")

# So, the next command does NOT work
difftime(today, dob, units="years")



############################################################################
###                                  Timestamps                          ###

#  Timestamps are internally stored as the number of seconds 
#  since 1970-01-01, with negative values for earlier moments

# get the current timestamp with date()
date()

# convert character to timestamp using function as.POSIXct and format
a_timestamp_var <- as.POSIXct("2014-09-08 17:15:47", "%Y-%m-%d %H:%M:%S")
a_timestamp_var

typeof(a_timestamp_var)
class(a_timestamp_var)

# see the internal integer representation
unclass(a_timestamp_var)


# function strftime
ts <- Sys.time()
ts
class(ts)
timeStamp <-  strftime(t,"%d-%m-%Y %H:%M:%S")
timeStamp
typeof(timeStamp)


#  specify the time zone:
timestamp_var.1 <- as.POSIXct("2014-10-01 18:42:03", tz = "GMT")
timestamp_var.1

# even if there is no error displayed, there is no defined a timezone for Romania...
timestamp_var.2 <- as.POSIXct("2014-10-01 18:42:03", tz = "Romania")
timestamp_var.2

# ... but we can use a neighbour's timezone:
timestamp_var.3 <- as.POSIXct("2014-10-01 18:42:03", tz = "Turkey")
timestamp_var.3


# POSIXlt
# This class enables easy extraction of specific components of a time. 
# (“ct” stand for calender time and “lt” stands for local time. 
#   “lt” also helps one remember that POXIXlt objects are lists.)

# create a timestamp/POSIXlt variable
timestamp_var.4 <- as.POSIXlt("2014-10-24 18:05:26")
timestamp_var.4
class(timestamp_var.4)
unclass(timestamp_var.4)

## extract components of a timestamp object:
# year (since 1900)
timestamp_var.4$year
# month (after the first of the year, current_month - 1)
timestamp_var.4$mon
# day of the month
timestamp_var.4$mday
# day of the year (0–365)
timestamp_var.4$yday
# day of the week, starting on Sunday (0–6)
timestamp_var.4$wday
# hour
timestamp_var.4$hour
# minute
timestamp_var.4$min
# second
timestamp_var.4$sec

## truncate or round off the time:
timestamp_var.4

# truncate or round at minutes level:
trunc(timestamp_var.4, "mins")

# truncate or round at hours level:
trunc(timestamp_var.4, "hours")

# truncate or round at days level:
trunc(timestamp_var.4, "days")


############################################################################
###  Recommended package for handling dates and timestamps: `lubridate`  ###

# install.packages("lubridate")
library(lubridate)

# for documentation about "lubridate" package, see:
# http://vita.had.co.nz/papers/lubridate.pdf
# http://cran.r-project.org/web/packages/lubridate/lubridate.pdf

# convert strings to dates with function "dmy" 
dob <- as.Date("1956-10-12")
dob
dob = dmy("25-12-1966")
dob
class(dob)

# extract year, month and day from a date without using format
year(dob)
month(dob)
day(dob)
# extract the name of the day of the week from a date
wday(dob, label=T)
# extract the number of the day of the week from a date
wday(dob)


## convert strings to timestamps with different functions such as: 
ts1.lub <- ymd_hms("2014-10-24 23:57:58")
ts1.lub
class(ts1.lub)
ts2.lub <- mdy_hm("10/24/2014 23:57")
ts2.lub
ts3.lub <- ydm_hm("2014-24-10 11:57pm")
ts3.lub
ts4.lub <- dmy("24102014")
ts4.lub


# Extract/reassign timestamp components:
year(ts1.lub)
week(ts1.lub)
wday(ts1.lub, label = TRUE)
hour(ts1.lub)
tz(ts1.lub)
second(ts1.lub) <- 10
ts1.lub

##
# Lubridate distinguishes between four types of objects: 
# * instants - specific moments in time
# * intervals - record time spans
# * durations
# * periods. 

# Dates and times parsed in lubridate are instants:
is.instant(ts1.lub)

# round an instant
round_date(ts1.lub, "minute")
round_date(ts1.lub, "day")

# get the current time or date as an instant:
now()
today()

# lubridate uses UTC time zones as default.
#   see an instant in a different time zone:
ts1.lub
tz(ts1.lub)
with_tz(ts1.lub, "Turkey")
tz(ts1.lub)
# change the time zone of an instant (keeping the same clock time):
ts1.lub <- force_tz(ts1.lub, "Turkey")
ts1.lub
tz(ts1.lub)


# Some calculations with instants. Note that the units are seconds:
ts2.lub <- mdy_hm("11/24/2014 9:12")
ts2.lub <- force_tz(ts2.lub, "Turkey")
ts2.lub - ts1.lub
ts2.lub > ts1.lub
ts1.lub + 30
ts1.lub <- force_tz(ts1.lub, "UTC")
ts2.lub <- force_tz(ts2.lub, "UTC")



# Intervals in lubridate
# An interval is the span of time that occurs between two specified instants.\
ts1.lub <- ymd_hms("2014-10-24 23:57:58")
ts2.lub <- mdy_hm("11/24/2014 9:12")
ts2.lub - ts1.lub
ts2.lub > ts1.lub
ts1.lub + 30
ts3.lub <- ydm_hm("2014-25-10 11:57pm")
ts3.lub
ts4.lub <- dmy("24102014")
ts4.lub

interval.1 <- as.interval(ts1.lub, ts2.lub)
interval.1

# Check whether a certain instant occured with a specified interval:
ts3.lub
ts3.lub %within% interval.1
ts4.lub
ts4.lub %within% interval.1

# Determine whether two intervals overlap:
interval.2 <- as.interval(ymd_hm("2014-09-25 10:03"), 
     ymd_hm("2014-10-30 19:19"))
interval.2
int_overlaps(interval.1, interval.2)



##
# A duration is a time span not anchored to specific start and end times. 
# It has an exact, fixed length, and is stored internally in seconds.

# Create some duration variables:
ten.minutes <- dminutes(10)
ten.minutes
five.days <- ddays(5)
five.days
one.year <- dyears(1)
one.year
as.duration(in.bed)

# Operations with durations:
ts1.lub - ten.minutes
ts1.lub - five.days
five.days + dhours(12)
ten.minutes/as.duration(in.bed)


#
#  A period is a time span not anchored to specific start and end times, 
#  and measured in units larger than seconds with inexact lengths. 

# create some periods:
three.weeks <- weeks(3)
three.weeks
four.hours <- hours(4)
four.hours

# operations  with periods:
ts1.lub + three.weeks
sabbatical <- months(6) + days(12)
sabbatical
three.weeks/sabbatical


##

# Compute age of a person
today <- now()
today
dob
class(today)

# difference between today and dob can be expressed in days
today - dob

# next command does not work
months(today - dob)
class(today)
class(dob)


interval(dob, today)

as.duration(interval(dob, today))

as.period(interval(dob, today), units = "day")
as.period(interval(dob, today), units = "month")
as.period(interval(dob, today), units = "year")


# http://www.cyclismo.org/tutorial/R/time.html

###
### see also packages: 
### chron
### fCalendar


############################################################################
###                  	     Special values 					   ###										

###
### Infinity (-Inf and Inf) -a value that is infinitely large. 

13 / 0
#[1] Inf

-13/0
#[1] -Inf


###
### Not a Number (NaN)
0 / 0
# [1] NaN


###
### Not available (NA). 
# NA indicates that the value that is “supposed” to be stored here is missing.
# In R NA is the equivalent of NULL in relational databases

x <- 23
y <- NaN
z <- NA

x + y

x + z

0 / x

0 / y

0 / z


###
### No value (NULL). 
# The NULL value basically asserts that the variable genuinely has no value whatsoever. 


# For NaN we actually know what the value is, because it’s something "insane" like 0/0. 
# For NA, we believe that there is supposed to be a value “out there”, but currently 
#  we do not know it.
# For NULL we strongly believe that there is no value at all.


