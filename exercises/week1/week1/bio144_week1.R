########################################################################

# BIO144

########################################################################
# WEEK 1 BC
########################################################################

rm(list=ls()) # clean session

####### Check if imported correctly #######

# table ()
# read.csv()
# summary()
# str()

####### Deal with rows and columns #######

# Save the part you want in a new excel file.

####### Options of read.csv() #######

?read.csv()

########################################################################
# WEEK 1 IC
########################################################################

####### Nice functions #######

# skim() to have a look at a dataset
# glimpse() to have a look at a dataset
# %>% um funktionen zu verknüpfen zb:

# class_RTs %>%
#  group_by(Gender) %>%
#  summarise(number = n())

# filter() for filtering some values out zb:

# lass_RTs_filtered <- class_RTs %>%
#  filter(Pref_Reaction_time > 50,
#         Pref_Reaction_time < 500)
# use | for or and & for and


