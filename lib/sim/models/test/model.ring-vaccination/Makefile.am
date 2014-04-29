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

EXTRA_DIST = radius_0.xml radius_0.5.xml radius_1.xml radius_2.5.xml \
 radius_1_delay_1.xml radius_1_delay_2.xml spread.xml spread_radius_1.xml \
 spread_radius_2.5.xml spread_radius_10.xml 2species_same_params.xml \
 2species_diff_params.xml revaccinate_0.xml revaccinate_1.xml \
 none_trigger_none_vacc.xml cattle_trigger_cattle_vacc.xml \
 cattle_trigger_pigs_vacc.xml cattle_trigger_both_vacc.xml \
 pigs_trigger_cattle_vacc.xml pigs_trigger_pigs_vacc.xml \
 pigs_trigger_both_vacc.xml both_trigger_cattle_vacc.xml \
 both_trigger_pigs_vacc.xml both_trigger_both_vacc.xml \
 trigger_small_ring.xml trigger_large_ring.xml \
 quarantine_waiting_0.xml quarantine_waiting_1.xml quarantine_waiting_2.xml \
 early_exit_1.xml early_exit_1_w_vars.xml \
 early_exit_2.xml early_exit_2_w_vars.xml \
 early_exit_3.xml early_exit_3_w_vars.xml \
 early_exit_4.xml early_exit_4_w_vars.xml early_exit_4_w_vars2.xml \
 early_exit_5.xml early_exit_5_w_vars.xml early_exit_5_w_vars2.xml \
 early_exit_6.xml early_exit_6_w_vars.xml early_exit_6_w_vars2.xml \
 destroyed_while_waiting_1.xml destroyed_while_waiting_1_w_vars.xml \
 destroyed_while_waiting_2.xml destroyed_while_waiting_2_w_vars.xml \
 destroyed_while_waiting_3.xml destroyed_while_waiting_3_w_vars.xml \
 lat_and_rtree.xml spread_radius_10_w_vars.xml
