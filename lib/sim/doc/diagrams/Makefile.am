## Makefile.am -- Process this file with automake to produce Makefile.in

%.gnuplot: $(top_srcdir)/models/test/%.xml
	python herd2gnuplot.py < $< > $@

.dia.eps:
	dia -e $@ $<

.gnuplot.eps:
	(echo set terminal postscript eps color 24 && echo set output \'$@\' && cat $<) | gnuplot

.eps.png:
	convert -density 72x72 $< $@
	convert -geometry "640x1024>" $@ $@

.dot.png:
	dot -T png $< > $@

dist_noinst_DATA = state-transition.png state-transition.eps \
  uniform.png uniform.eps \
  triangular.png triangular.eps \
  piecewise.png piecewise.eps \
  piecewise_flat.png piecewise_flat.eps \
  gaussian.png gaussian.eps \
  beta.png beta.eps \
  betapert.png betapert.eps \
  gamma.png gamma.eps \
  weibull.png weibull.eps \
  exponential.png exponential.eps \
  logistic.png logistic.eps \
  loglogistic.png loglogistic.eps \
  lognormal.png lognormal.eps \
  pearson.png pearson.eps \
  poisson.png poisson.eps \
  rel_chart.png rel_chart.eps \
  airborne.png airborne.eps \
  directions.png directions.eps \
  building_blocks.png building_blocks.eps \
  events.png events.eps \
  events_flowchart.png events_flowchart.eps \
  priority_queues.png priority_queues.eps \
  back_linking.png back_linking.eps \
  no_back_linking.png no_back_linking.eps \
  1herd.eps 1herd.png \
  1herd_start_infectious.eps 1herd_start_infectious.png \
  1herd_start_infectious_1dayleft.eps 1herd_start_infectious_1dayleft.png \
  1herd_start_subclinical.eps 1herd_start_subclinical.png \
  1herd_1dayleft.eps 1herd_1dayleft.png \
  1herd_3daysleft.eps 1herd_3daysleft.png \
  2herds.eps 2herds.png \
  2herds_pigs_cattle_pigs_infected.eps 2herds_pigs_cattle_pigs_infected.png \
  2herds_pigs_cattle_all_infected.eps 2herds_pigs_cattle_all_infected.png \
  2targets_same_dist_same_size.eps 2targets_same_dist_same_size.png \
  2targets_same_dist_diff_size.eps 2targets_same_dist_diff_size.png \
  2herds_1_start_destroyed.eps 2herds_1_start_destroyed.png \
  2herds_1_start_nimmune.eps 2herds_1_start_nimmune.png \
  2herds_1_start_nimmune_2daysleft.eps 2herds_1_start_nimmune_2daysleft.png \
  2herds_1_start_nimmune_3daysleft.eps 2herds_1_start_nimmune_3daysleft.png \
  2herds_1_start_vimmune.eps 2herds_1_start_vimmune.png \
  2herds_1_start_vimmune_1dayleft.eps 2herds_1_start_vimmune_1dayleft.png \
  2herds_1_start_vimmune_2daysleft.eps 2herds_1_start_vimmune_2daysleft.png \
  2herds_ascii.png 2herds_iso88591.png 2herds_utf8.png 2herds_utf8_no_bom.png \
  2herds_utf16_big_endian.png 2herds_utf16_big_endian_no_bom.png \
  2herds_utf16_little_endian.png 2herds_utf16_little_endian_no_bom.png \
  3herds.eps 3herds.png \
  3herds_2_at_same_loc.eps 3herds_2_at_same_loc.png \
  3herds_pigs_2cattle_pigs_infected.eps 3herds_pigs_2cattle_pigs_infected.png \
  3herds_pigs_cattle_sheep_sheep_infected.eps 3herds_pigs_cattle_sheep_sheep_infected.png \
  3herds_pigs_cattle_sheep_pigs_sheep_infected.eps 3herds_pigs_cattle_sheep_pigs_sheep_infected.png \
  3herds_pigs_cattle_sheep_all_infected.eps 3herds_pigs_cattle_sheep_all_infected.png \
  3targets_same_dist_same_size.eps 3targets_same_dist_same_size.png \
  4herds_line.eps 4herds_line.png \
  4herds_all_infected.eps 4herds_all_infected.png \
  4herds_line_2.eps 4herds_line_2.png \
  4herds_line_3.eps 4herds_line_3.png \
  4herds_line_4.eps 4herds_line_4.png \
  4herds_2.eps 4herds_2.png \
  5herds.eps 5herds.png \
  5herds_very_close.eps 5herds_very_close.png \
  6herds.eps 6herds.png \
  6herds_2.eps 6herds_2.png \
  7herds.eps 7herds.png \
  8herds_all_infected.eps 8herds_all_infected.png \
  8herds_2rows.eps 8herds_2rows.png \
  8herds_2rows_2.eps 8herds_2rows_2.png \
  herds_circle.eps herds_circle.png \
  herds_circle2.eps herds_circle2.png \
  16herds_square.eps 16herds_square.png \
  no_rtree_lat0.eps no_rtree_lat0.png \
  rtree_lat0.eps rtree_lat0.png \
  no_rtree_lat45.eps no_rtree_lat45.png \
  rtree_lat45.eps rtree_lat45.png \
  no_rtree_contact_spread.eps no_rtree_contact_spread.png \
  rtree_contact_spread.eps rtree_contact_spread.png \
  1000herds_line.eps 1000herds_line.png \
  complexity.eps complexity.png \
  speedup.eps speedup.png \
  uncontrolled_example.png \
  rtree_benefit.eps rtree_benefit.png \
  contact_problem.eps contact_problem.png \
  contact_proof.eps contact_proof.png \
  extend_array_1.eps extend_array_1.png \
  extend_array_2.eps extend_array_2.png \
  surv_levels.eps surv_levels.png \
  basic_circle.eps basic_circle.png \
  ordered_circles.eps ordered_circles.png \
  adjacent_zones.eps adjacent_zones.png \
  enclosure.eps enclosure.png \
  enclosure2.eps enclosure2.png \
  split_zones.eps split_zones.png \
  movement_inside.eps movement_inside.png \
  lower_to_higher.eps lower_to_higher.png \
  detour.eps detour.png \
  1_zone_1_focus.eps 1_zone_1_focus.png \
  2_zones_1_focus.eps 2_zones_1_focus.png \
  1_zone_2_foci.eps 1_zone_2_foci.png \
  1_zone_2_foci_merge.eps 1_zone_2_foci_merge.png \
  2_zones_2_foci.eps 2_zones_2_foci.png \
  2_zones_2_foci_merge1.eps 2_zones_2_foci_merge1.png \
  2_zones_2_foci_merge2.eps 2_zones_2_foci_merge2.png \
  3_foci_merge.eps 3_foci_merge.png \
  1_zone_13_foci.eps 1_zone_13_foci.png \
  1_zone_13_foci_merge.eps 1_zone_13_foci_merge.png \
  zones_trace_cattle.eps zones_trace_cattle.png \
  zones_trace_pigs.eps zones_trace_pigs.png \
  zones_trace_both.eps zones_trace_both.png \
  zones_trace_both_merge.eps zones_trace_both_merge.png \
  2foci_separate.eps 2foci_separate.png \
  2foci_joined.eps 2foci_joined.png \
  3foci.eps 3foci.png \
  donut_0.eps donut_0.png \
  donut_1.eps donut_1.png \
  2_zone_levels.eps 2_zone_levels.png \
  2targets_1_in_zone.eps 2targets_1_in_zone.png \
  prevalence_1.eps prevalence_1.png \
  prevalence_2.eps prevalence_2.png \
  air_prevalence.eps air_prevalence.png \
  air_prevalence_incr.eps air_prevalence_incr.png \
  prevalence_day2.eps prevalence_day2.png \
  prevalence_day4.eps prevalence_day4.png \
  latitude_and_area.eps latitude_and_area.png \
  16herds_1_zone.eps 16herds_1_zone.png \
  16herds_2_zones.eps 16herds_2_zones.png \
  filters_flowchart.png \
  7herds_w_ring.png

state_transition_png_SOURCES = state-transition.dia
state_transition_eps_SOURCES = state-transition.dia

uniform_png_SOURCES = uniform.gnuplot
uniform_eps_SOURCES = uniform.gnuplot

triangular_png_SOURCES = triangular.gnuplot
triangular_eps_SOURCES = triangular.gnuplot

piecewise_png_SOURCES = piecewise.gnuplot
piecewise_eps_SOURCES = piecewise.gnuplot

piecewise_flat_png_SOURCES = piecewise_flat.gnuplot
piecewise_flat_eps_SOURCES = piecewise_flat.gnuplot

gaussian_png_SOURCES = gaussian.gnuplot
gaussian_eps_SOURCES = gaussian.gnuplot

beta_png_SOURCES = beta.gnuplot
beta_eps_SOURCES = beta.gnuplot

betapert_png_SOURCES = betapert.gnuplot
betapert_eps_SOURCES = betapert.gnuplot

gamma_png_SOURCES = gamma.gnuplot
gamma_eps_SOURCES = gamma.gnuplot

weibull_png_SOURCES = weibull.gnuplot
weibull_eps_SOURCES = weibull.gnuplot

exponential_png_SOURCES = exponential.gnuplot
exponential_eps_SOURCES = exponential.gnuplot

logistic_png_SOURCES = logistic.gnuplot
logistic_eps_SOURCES = logistic.gnuplot

loglogistic_png_SOURCES = loglogistic.gnuplot
loglogistic_eps_SOURCES = loglogistic.gnuplot

lognormal_png_SOURCES = lognormal.gnuplot
lognormal_eps_SOURCES = lognormal.gnuplot

pearson_png_SOURCES = pearson.gnuplot
pearson_eps_SOURCES = pearson.gnuplot

poisson_png_SOURCES = poisson.gnuplot
poisson_eps_SOURCES = poisson.gnuplot

rel_chart_png_SOURCES = rel_chart.gnuplot
rel_chart_eps_SOURCES = rel_chart.gnuplot

airborne_eps_SOURCES = airborne.dia
airborne_png_SOURCES = airborne.dia

directions_eps_SOURCES = directions.dia
directions_png_SOURCES = directions.dia

building_blocks_eps_SOURCES = building_blocks.dia bluetongue.jpg stormy_sky.jpg vaccine.jpg guillotine.jpg cow.jpg sherlock_items.jpg farm_truck.jpg
building_blocks_png_SOURCES = building_blocks.dia bluetongue.jpg stormy_sky.jpg vaccine.jpg guillotine.jpg cow.jpg sherlock_items.jpg farm_truck.jpg

events_eps_SOURCES = events.dia ear.gif siren.gif cow.png pig.png
events_png_SOURCES = events.dia ear.gif siren.gif cow.png pig.png

events_flowchart_eps_SOURCES = events_flowchart.dia
events_flowchart_png_SOURCES = events_flowchart.dia

priority_queues_eps_SOURCES = priority_queues.dia
priority_queues_png_SOURCES = priority_queues.dia

back_linking_eps_SOURCES = back_linking.dia
back_linking_png_SOURCES = back_linking.dia

no_back_linking_eps_SOURCES = no_back_linking.dia
no_back_linking_png_SOURCES = no_back_linking.dia

1herd_eps_SOURCES = $(top_srcdir)/models/test/1herd.xml
1herd_png_SOURCES = $(top_srcdir)/models/test/1herd.xml

1herd_start_infectious_eps_SOURCES = $(top_srcdir)/models/test/1herd_start_infectious.xml
1herd_start_infectious_png_SOURCES = $(top_srcdir)/models/test/1herd_start_infectious.xml

1herd_start_infectious_1dayleft_eps_SOURCES = $(top_srcdir)/models/test/1herd_start_infectious_1dayleft.xml
1herd_start_infectious_1dayleft_png_SOURCES = $(top_srcdir)/models/test/1herd_start_infectious_1dayleft.xml

1herd_start_subclinical_eps_SOURCES = $(top_srcdir)/models/test/1herd_start_subclinical.xml
1herd_start_subclinical_png_SOURCES = $(top_srcdir)/models/test/1herd_start_subclinical.xml

1herd_1dayleft_eps_SOURCES = $(top_srcdir)/models/test/1herd_1dayleft.xml
1herd_1dayleft_png_SOURCES = $(top_srcdir)/models/test/1herd_1dayleft.xml

1herd_3daysleft_eps_SOURCES = $(top_srcdir)/models/test/1herd_3daysleft.xml
1herd_3daysleft_png_SOURCES = $(top_srcdir)/models/test/1herd_3daysleft.xml

2herds_eps_SOURCES = $(top_srcdir)/models/test/2herds.xml
2herds_png_SOURCES = $(top_srcdir)/models/test/2herds.xml

2herds_pigs_cattle_pigs_infected_eps_SOURCES = $(top_srcdir)/models/test/2herds_pigs_cattle_pigs_infected.xml
2herds_pigs_cattle_pigs_infected_png_SOURCES = $(top_srcdir)/models/test/2herds_pigs_cattle_pigs_infected.xml

2herds_pigs_cattle_all_infected_eps_SOURCES = $(top_srcdir)/models/test/2herds_pigs_cattle_all_infected.xml
2herds_pigs_cattle_all_infected_png_SOURCES = $(top_srcdir)/models/test/2herds_pigs_cattle_all_infected.xml

2targets_same_dist_same_size_eps_SOURCES = $(top_srcdir)/models/test/2targets_same_dist_same_size.xml
2targets_same_dist_same_size_png_SOURCES = $(top_srcdir)/models/test/2targets_same_dist_same_size.xml

2targets_same_dist_diff_size_eps_SOURCES = $(top_srcdir)/models/test/2targets_same_dist_diff_size.xml
2targets_same_dist_diff_size_png_SOURCES = $(top_srcdir)/models/test/2targets_same_dist_diff_size.xml

2herds_1_start_destroyed_eps_SOURCES = $(top_srcdir)/models/test/2herds_1_start_destroyed.xml
2herds_1_start_destroyed_png_SOURCES = $(top_srcdir)/models/test/2herds_1_start_destroyed.xml

2herds_1_start_nimmune_eps_SOURCES = $(top_srcdir)/models/test/2herds_1_start_nimmune.xml
2herds_1_start_nimmune_png_SOURCES = $(top_srcdir)/models/test/2herds_1_start_nimmune.xml

2herds_1_start_nimmune_2daysleft_eps_SOURCES = $(top_srcdir)/models/test/2herds_1_start_2daysleft_nimmune.xml
2herds_1_start_nimmune_2daysleft_png_SOURCES = $(top_srcdir)/models/test/2herds_1_start_2daysleft_nimmune.xml

2herds_1_start_nimmune_3daysleft_eps_SOURCES = $(top_srcdir)/models/test/2herds_1_start_3daysleft_nimmune.xml
2herds_1_start_nimmune_3daysleft_png_SOURCES = $(top_srcdir)/models/test/2herds_1_start_3daysleft_nimmune.xml

2herds_1_start_vimmune_eps_SOURCES = $(top_srcdir)/models/test/2herds_1_start_vimmune.xml
2herds_1_start_vimmune_png_SOURCES = $(top_srcdir)/models/test/2herds_1_start_vimmune.xml

2herds_1_start_vimmune_1dayleft_eps_SOURCES = $(top_srcdir)/models/test/2herds_1_start_1dayleft_vimmune.xml
2herds_1_start_vimmune_1dayleft_png_SOURCES = $(top_srcdir)/models/test/2herds_1_start_1dayleft_vimmune.xml

2herds_1_start_vimmune_2daysleft_eps_SOURCES = $(top_srcdir)/models/test/2herds_1_start_2daysleft_vimmune.xml
2herds_1_start_vimmune_2daysleft_png_SOURCES = $(top_srcdir)/models/test/2herds_1_start_2daysleft_vimmune.xml

2herds_ascii_eps_SOURCES = $(top_srcdir)/models/test/2herds_ascii.xml
2herds_ascii_png_SOURCES = $(top_srcdir)/models/test/2herds_ascii.xml

2herds_iso88591_eps_SOURCES = $(top_srcdir)/models/test/2herds_iso88591.xml
2herds_iso88591_png_SOURCES = $(top_srcdir)/models/test/2herds_iso88591.xml

2herds_utf8_eps_SOURCES = $(top_srcdir)/models/test/2herds_utf8.xml
2herds_utf8_png_SOURCES = $(top_srcdir)/models/test/2herds_utf8.xml

2herds_utf8_no_bom_eps_SOURCES = $(top_srcdir)/models/test/2herds_utf8_no_bom.xml
2herds_utf8_no_bom_png_SOURCES = $(top_srcdir)/models/test/2herds_utf8_no_bom.xml

2herds_utf16_big_endian_eps_SOURCES = $(top_srcdir)/models/test/2herds_utf16_big_endian.xml
2herds_utf16_big_endian_png_SOURCES = $(top_srcdir)/models/test/2herds_utf16_big_endian.xml

2herds_utf16_big_endian_no_bom_eps_SOURCES = $(top_srcdir)/models/test/2herds_utf16_big_endian_no_bom.xml
2herds_utf16_big_endian_no_bom_png_SOURCES = $(top_srcdir)/models/test/2herds_utf16_big_endian_no_bom.xml

2herds_utf16_little_endian_eps_SOURCES = $(top_srcdir)/models/test/2herds_utf16_little_endian.xml
2herds_utf16_little_endian_png_SOURCES = $(top_srcdir)/models/test/2herds_utf16_little_endian.xml

2herds_utf16_little_endian_no_bom_eps_SOURCES = $(top_srcdir)/models/test/2herds_utf16_little_endian_no_bom.xml
2herds_utf16_little_endian_no_bom_png_SOURCES = $(top_srcdir)/models/test/2herds_utf16_little_endian_no_bom.xml

3herds_eps_SOURCES = $(top_srcdir)/models/test/3herds.xml
3herds_png_SOURCES = $(top_srcdir)/models/test/3herds.xml

3herds_2_at_same_loc_eps_SOURCES = $(top_srcdir)/models/test/3herds_2_at_same_loc.xml
3herds_2_at_same_loc_png_SOURCES = $(top_srcdir)/models/test/3herds_2_at_same_loc.xml

3herds_pigs_2cattle_pigs_infected_eps_SOURCES = $(top_srcdir)/models/test/3herds_pigs_2cattle_pigs_infected.xml
3herds_pigs_2cattle_pigs_infected_png_SOURCES = $(top_srcdir)/models/test/3herds_pigs_2cattle_pigs_infected.xml

3herds_pigs_cattle_sheep_pigs_sheep_infected_eps_SOURCES = $(top_srcdir)/models/test/3herds_pigs_cattle_sheep_pigs_sheep_infected.xml
3herds_pigs_cattle_sheep_pigs_sheep_infected_png_SOURCES = $(top_srcdir)/models/test/3herds_pigs_cattle_sheep_pigs_sheep_infected.xml

3targets_same_dist_same_size_eps_SOURCES = $(top_srcdir)/models/test/3targets_same_dist_same_size.xml
3targets_same_dist_same_size_png_SOURCES = $(top_srcdir)/models/test/3targets_same_dist_same_size.xml

4herds_line_eps_SOURCES = $(top_srcdir)/models/test/4herds_line.xml
4herds_line_png_SOURCES = $(top_srcdir)/models/test/4herds_line.xml

4herds_all_infected_eps_SOURCES = $(top_srcdir)/models/test/4herds_all_infected.xml
4herds_all_infected_png_SOURCES = $(top_srcdir)/models/test/4herds_all_infected.xml

5herds_eps_SOURCES = $(top_srcdir)/models/test/5herds.xml
5herds_png_SOURCES = $(top_srcdir)/models/test/5herds.xml

5herds_very_close_eps_SOURCES = $(top_srcdir)/models/test/5herds_very_close.xml
5herds_very_close_png_SOURCES = $(top_srcdir)/models/test/5herds_very_close.xml

6herds_eps_SOURCES = $(top_srcdir)/models/test/6herds.xml
6herds_png_SOURCES = $(top_srcdir)/models/test/6herds.xml

6herds_2_eps_SOURCES = $(top_srcdir)/models/test/6herds_2.xml
6herds_2_png_SOURCES = $(top_srcdir)/models/test/6herds_2.xml

7herds_eps_SOURCES = $(top_srcdir)/models/test/7herds.xml
7herds_png_SOURCES = $(top_srcdir)/models/test/7herds.xml

8herds_all_infected_eps_SOURCES = $(top_srcdir)/models/test/8herds_all_infected.xml
8herds_all_infected_png_SOURCES = $(top_srcdir)/models/test/8herds_all_infected.xml

8herds_2rows_eps_SOURCES = $(top_srcdir)/models/test/8herds_2rows.xml
8herds_2rows_png_SOURCES = $(top_srcdir)/models/test/8herds_2rows.xml

8herds_2rows_2_eps_SOURCES = $(top_srcdir)/models/test/8herds_2rows_2.xml
8herds_2rows_2_png_SOURCES = $(top_srcdir)/models/test/8herds_2rows_2.xml

herds_circle_eps_SOURCES = $(top_srcdir)/models/test/herds_circle.xml
herds_circle_png_SOURCES = $(top_srcdir)/models/test/herds_circle.xml

herds_circle2_eps_SOURCES = $(top_srcdir)/models/test/herds_circle2.xml
herds_circle2_png_SOURCES = $(top_srcdir)/models/test/herds_circle2.xml

16herds_square_eps_SOURCES = $(top_srcdir)/models/test/16herds_square.xml
16herds_square_png_SOURCES = $(top_srcdir)/models/test/16herds_square.xml

no_rtree_lat0_eps_SOURCES = $(top_srcdir)/models/test/no_rtree_lat0.xml
no_rtree_lat0_png_SOURCES = $(top_srcdir)/models/test/no_rtree_lat0.xml

rtree_lat0_eps_SOURCES = $(top_srcdir)/models/test/rtree_lat0.xml
rtree_lat0_png_SOURCES = $(top_srcdir)/models/test/rtree_lat0.xml

no_rtree_lat45_eps_SOURCES = $(top_srcdir)/models/test/no_rtree_lat45.xml
no_rtree_lat45_png_SOURCES = $(top_srcdir)/models/test/no_rtree_lat45.xml

rtree_lat45_eps_SOURCES = $(top_srcdir)/models/test/rtree_lat45.xml
rtree_lat45_png_SOURCES = $(top_srcdir)/models/test/rtree_lat45.xml

no_rtree_contact_spread_eps_SOURCES = $(top_srcdir)/models/test/no_rtree_contact_spread.xml
no_rtree_contact_spread_png_SOURCES = $(top_srcdir)/models/test/no_rtree_contact_spread.xml

rtree_contact_spread_eps_SOURCES = $(top_srcdir)/models/test/rtree_contact_spread.xml
rtree_contact_spread_png_SOURCES = $(top_srcdir)/models/test/rtree_contact_spread.xml

1000herds_line_eps_SOURCES = $(top_srcdir)/models/test/1000herds_line.xml
1000herds_line_png_SOURCES = $(top_srcdir)/models/test/1000herds_line.xml

complexity_eps_SOURCES = complexity.gnuplot
complexity_png_SOURCES = complexity.gnuplot

speedup_eps_SOURCES = speedup.gnuplot
speedup_png_SOURCES = speedup.gnuplot

uncontrolled_example_png_SOURCES = uncontrolled_example.eps

rtree_benefit_eps_SOURCES = rtree_benefit.gnuplot
rtree_benefit_png_SOURCES = rtree_benefit.gnuplot

contact_problem_eps_SOURCES = contact_problem.dia
contact_problem_png_SOURCES = contact_problem.dia

contact_proof_eps_SOURCES = contact_proof.dia
contact_proof_png_SOURCES = contact_proof.dia

extend_array_1_eps_SOURCES = extend_array_1.dia
extend_array_1_png_SOURCES = extend_array_1.dia

extend_array_2_eps_SOURCES = extend_array_2.dia
extend_array_2_png_SOURCES = extend_array_2.dia

surv_levels_eps_SOURCES = surv_levels.dia
surv_levels_png_SOURCES = surv_levels.dia

basic_circle_eps_SOURCES = basic_circle.dia
basic_circle_png_SOURCES = basic_circle.dia

ordered_circles_eps_SOURCES = ordered_circles.dia
ordered_circles_png_SOURCES = ordered_circles.dia

adjacent_zones_eps_SOURCES = adjacent_zones.dia
adjacent_zones_png_SOURCES = adjacent_zones.dia

enclosure_eps_SOURCES = enclosure.dia
enclosure_png_SOURCES = enclosure.dia

enclosure2_eps_SOURCES = enclosure2.dia
enclosure2_png_SOURCES = enclosure2.dia

split_zones_eps_SOURCES = split_zones.dia
split_zones_png_SOURCES = split_zones.dia

movement_inside_eps_SOURCES = movement_inside.dia
movement_inside_png_SOURCES = movement_inside.dia

lower_to_higher_eps_SOURCES = lower_to_higher.dia
lower_to_higher_png_SOURCES = lower_to_higher.dia

detour_eps_SOURCES = detour.dia
detour_png_SOURCES = detour.dia

1_zone_1_focus_eps_SOURCES = 1_zone_1_focus.gnuplot
1_zone_1_focus_png_SOURCES = 1_zone_1_focus.gnuplot

2_zones_1_focus_eps_SOURCES = 2_zones_1_focus.gnuplot
2_zones_1_focus_png_SOURCES = 2_zones_1_focus.gnuplot

1_zone_2_foci_eps_SOURCES = 1_zone_2_foci.gnuplot
1_zone_2_foci_png_SOURCES = 1_zone_2_foci.gnuplot

1_zone_2_foci_merge_eps_SOURCES = 1_zone_2_foci_merge.gnuplot
1_zone_2_foci_merge_png_SOURCES = 1_zone_2_foci_merge.gnuplot

2_zones_2_foci_eps_SOURCES = 2_zones_2_foci.gnuplot
2_zones_2_foci_png_SOURCES = 2_zones_2_foci.gnuplot

2_zones_2_foci_merge1_eps_SOURCES = 2_zones_2_foci_merge1.gnuplot
2_zones_2_foci_merge1_png_SOURCES = 2_zones_2_foci_merge1.gnuplot

2_zones_2_foci_merge2_eps_SOURCES = 2_zones_2_foci_merge2.gnuplot
2_zones_2_foci_merge2_png_SOURCES = 2_zones_2_foci_merge2.gnuplot

3_foci_merge_eps_SOURCES = 3_foci_merge.gnuplot
3_foci_merge_png_SOURCES = 3_foci_merge.gnuplot

1_zone_13_foci_eps_SOURCES = 1_zone_13_foci.gnuplot
1_zone_13_foci_png_SOURCES = 1_zone_13_foci.gnuplot

1_zone_13_foci_merge_eps_SOURCES = 1_zone_13_foci_merge.gnuplot
1_zone_13_foci_merge_png_SOURCES = 1_zone_13_foci_merge.gnuplot

zones_trace_cattle_eps_SOURCES = zones_trace_cattle.gnuplot
zones_trace_cattle_png_SOURCES = zones_trace_cattle.gnuplot

zones_trace_pigs_eps_SOURCES = zones_trace_pigs.gnuplot
zones_trace_pigs_png_SOURCES = zones_trace_pigs.gnuplot

zones_trace_both_eps_SOURCES = zones_trace_both.gnuplot
zones_trace_both_png_SOURCES = zones_trace_both.gnuplot

zones_trace_both_merge_eps_SOURCES = zones_trace_both_merge.gnuplot
zones_trace_both_merge_png_SOURCES = zones_trace_both_merge.gnuplot

2foci_separate_eps_SOURCES = 2foci_separate.gnuplot
2foci_separate_png_SOURCES = 2foci_separate.gnuplot

2foci_joined_eps_SOURCES = 2foci_joined.gnuplot
2foci_joined_png_SOURCES = 2foci_joined.gnuplot

3foci_eps_SOURCES = 3foci.gnuplot
3foci_png_SOURCES = 3foci.gnuplot

donut_0_eps_SOURCES = donut_0.gnuplot
donut_0_png_SOURCES = donut_0.gnuplot

donut_1_eps_SOURCES = donut_1.gnuplot
donut_1_png_SOURCES = donut_1.gnuplot

2_zone_levels_eps_SOURCES = 2_zone_levels.gnuplot
2_zone_levels_png_SOURCES = 2_zone_levels.gnuplot

2targets_1_in_zone_eps_SOURCES = 2targets_1_in_zone.gnuplot
2targets_1_in_zone_png_SOURCES = 2targets_1_in_zone.gnuplot

prevalence_1_eps_SOURCES = prevalence_1.gnuplot
prevalence_1_png_SOURCES = prevalence_1.gnuplot

prevalence_2_eps_SOURCES = prevalence_2.gnuplot
prevalence_2_png_SOURCES = prevalence_2.gnuplot

air_prevalence_eps_SOURCES = air_prevalence.gnuplot
air_prevalence_png_SOURCES = air_prevalence.gnuplot

air_prevalence_incr_eps_SOURCES = air_prevalence_incr.gnuplot
air_prevalence_incr_png_SOURCES = air_prevalence_incr.gnuplot

prevalence_day2_eps_SOURCES = prevalence_day2.gnuplot
prevalence_day2_png_SOURCES = prevalence_day2.gnuplot

prevalence_day4_eps_SOURCES = prevalence_day4.gnuplot
prevalence_day4_png_SOURCES = prevalence_day4.gnuplot

latitude_and_area_eps_SOURCES = latitude_and_area.dia
latitude_and_area_png_SOURCES = latitude_and_area.dia

16herds_1_zone_eps_SOURCES = 16herds_1_zone.gnuplot
16herds_1_zone_png_SOURCES = 16herds_1_zone.gnuplot

16herds_2_zones_eps_SOURCES = 16herds_2_zones.gnuplot
16herds_2_zones_png_SOURCES = 16herds_2_zones.gnuplot

filters_flowchart_png_SOURCES = filters_flowchart.dot

7herds_w_ring_png_SOURCES = 7herds_w_ring.gnuplot
