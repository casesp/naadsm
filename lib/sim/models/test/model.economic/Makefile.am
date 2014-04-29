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

EXTRA_DIST = vaccinated_cost_1.xml vaccinated_cost_2.xml vaccinated_cost_3.xml \
  destroyed_cost_1.xml destroyed_cost_2.xml \
  surveillance_cost_1.xml surveillance_cost_2.xml \
  destroyed_before_dead.xml detected_dead_costs_unspecified.xml \
  detected_dead_no_costs.xml detected_dead_all_costs.xml \
  detected_dead_some_costs.xml \
  detected_dead_while_queued_costs_if_queued.xml \
  detected_dead_not_queued_costs_if_queued.xml
