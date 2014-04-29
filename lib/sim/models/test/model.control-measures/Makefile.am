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

EXTRA_DIST = delay_0.xml delay_1.xml delay_3.xml \
 delay_0_w_vars.xml delay_1_w_vars.xml delay_3_w_vars.xml \
 delay_0_limit_1.xml delay_0_limit_2.xml delay_0_limit_1-4.xml delay_3_limit_1-4.xml \
 delay_0_limit_0.xml trace_out_0.xml trace_out_1.xml \
 trace_out_success.xml trace_out_success_w_vars.xml \
 trace_out_failure.xml trace_out_failure_w_vars.xml \
 trace_in_1.xml trace_out_2.xml \
 trace_out_delay_2_window_3.xml trace_out_delay_3_window_3.xml \
 trace_out_delay_4_window_3.xml priority_detect_nearby_trace.xml \
 trace_no_destroy_1.xml trace_no_destroy_2.xml trace_no_destroy_3.xml \
 trace_no_destroy_4.xml trace_no_destroy_5.xml \
 priority_nearby_detect_trace.xml priority_trace_detect_nearby.xml \
 destruction_list_3_units_3_reasons.xml \
 priority_prodtype_time_1.xml priority_prodtype_time_2.xml \
 priority_time_prodtype.xml \
 priority_prodtype_time_reason.xml priority_prodtype_reason_time.xml \
 priority_prodtype-1_time_reason.xml priority_reason_time_prodtype-1.xml \
 vacc_priority_prodtype_time_1.xml vacc_priority_prodtype_time_2.xml \
 vacc_priority_time_prodtype.xml vacc_dest_same_day.xml \
 vacc_dest_same_day_2.xml vacc_dest_same_day_3.xml vacc_dest_same_day_4.xml \
 vacc_threshold_0.xml vacc_threshold_1.xml vacc_threshold_2.xml \
 vacc_threshold_3.xml vacc_threshold_3_backtrack.xml \
 vacc_threshold_4_backtrack.xml
