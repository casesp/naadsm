## Makefile.am -- Process this file with automake to produce Makefile.in

# Create an Expect script from the XML description of tests.

# The Expect script for DejaGnu goes into the distribution file, so that a make
# check can be run after installation.  But the XML file from which the Expect
# script was generated is not needed in the distribution.

dist_noinst_DATA = all.exp

nodist_all_exp_SOURCES = ../tests_start_tag.xml all.xml ../tests_end_tag.xml

all.exp: $(nodist_all_exp_SOURCES)
	cat $^ | python ../xml2expect.py > $@  

# Do include the test scenario parameter files in the distribution file.

EXTRA_DIST = proportion_0.xml proportion_25.xml proportion_50.xml \
  proportion_75.xml proportion_100.xml \
  proportion_75_w_vars.xml \
  proportion_50_0.xml proportion_0_50.xml proportion_50_50.xml \
  days_between_vacc_1.xml days_between_vacc_3.xml \
  days_between_vacc_5.xml days_between_vacc_7.xml
