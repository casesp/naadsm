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

EXTRA_DIST = detected_1.xml detected_1_w_vars.xml undetected_1.xml \
  detected_2.xml days_awareness_0.xml days_awareness_low1.xml \
  days_awareness_1daypast.xml days_awareness_2dayspast.xml \
  days_awareness_3dayspast.xml days_awareness_2dayspast_2.xml \
  days_awareness_2dayspast_3.xml fixed_detect_0.xml fixed_detect_4.xml \
  fixed_detect_5.xml
