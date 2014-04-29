/** @file prophylactic-vaccination-model.c
 * Module that simulates a policy of vaccinating units before disease appears.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 *
 * Copyright &copy; University of Guelph, 2009
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* To avoid name clashes when multiple modules have the same interface. */
#define is_singleton prophylactic_vaccination_model_is_singleton
#define new prophylactic_vaccination_model_new
#define run prophylactic_vaccination_model_run
#define reset prophylactic_vaccination_model_reset
#define events_listened_for prophylactic_vaccination_model_events_listened_for
#define is_listening_for prophylactic_vaccination_model_is_listening_for
#define has_pending_actions prophylactic_vaccination_model_has_pending_actions
#define to_string prophylactic_vaccination_model_to_string
#define local_printf prophylactic_vaccination_model_printf
#define local_fprintf prophylactic_vaccination_model_fprintf
#define local_free prophylactic_vaccination_model_free
#define handle_before_any_simulations_event prophylactic_vaccination_model_handle_before_any_simulations_event
#define handle_before_each_simulation_event prophylactic_vaccination_model_handle_before_each_simulation_event

#include "model.h"
#include "model_util.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#include "prophylactic-vaccination-model.h"

#if !HAVE_ROUND && HAVE_RINT
#  define round rint
#endif

/* Temporary fix -- "round" and "rint" are in the math library on Red Hat 7.3,
 * but they're #defined so AC_CHECK_FUNCS doesn't find them. */
double round (double x);

/** This must match an element name in the DTD. */
#define MODEL_NAME "prophylactic-vaccination-model"



#define NEVENTS_LISTENED_FOR 2
EVT_event_type_t events_listened_for[] = { EVT_BeforeAnySimulations, EVT_BeforeEachSimulation };



const char prophylactic_vaccination_model_vaccination_reason[] = "Prph";



/** Specialized information for this model. */
typedef struct
{
  char *production_type_name;
  PDF_dist_t *proportion_of_units_vaccinated;
  int days_between_vaccinations;
  GArray *units; /**< A list of units of the production type we are interested
    in.  Will be used before each simulation to pick units for vaccination.
    Each item is a HRD_herd_t * pointer. */
  gboolean set_fixed_pattern;
}
local_data_t;



/**
 * Before any simulations, this module declares all the reasons for which it
 * may request a vaccination.
 *
 * @param queue for any new events the model creates.
 */
void
handle_before_any_simulations_event (EVT_event_queue_t * queue)
{
  GPtrArray *reasons;

#if DEBUG
  g_debug ("----- ENTER handle_before_any_simulations_event (%s)", MODEL_NAME);
#endif

  reasons = g_ptr_array_sized_new (1);
  g_ptr_array_add (reasons, (gpointer) prophylactic_vaccination_model_vaccination_reason);
  EVT_event_enqueue (queue, EVT_new_declaration_of_vaccination_reasons_event (reasons));

  /* Note that we don't clean up the GPtrArray.  It will be freed along with
   * the declaration event after all interested sub-models have processed the
   * event. */

#if DEBUG
  g_debug ("----- EXIT handle_before_any_simulations_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Before each simulation, this module vaccinates some of the units.
 *
 * @param self this module.
 * @param herds a list of herds.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
handle_before_each_simulation_event (struct naadsm_model_t_ *self,
                                     HRD_herd_list_t * herds,
                                     RAN_gen_t *rng,
                                     EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  guint num_units;
  double p;
  guint i, num_chosen, lo, hi;
  HRD_herd_t *unit;
  EVT_event_t *event;

#if DEBUG
  g_debug ("----- ENTER handle_before_each_simulation_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  num_units = local_data->units->len;
  p = PDF_random_non_neg (local_data->proportion_of_units_vaccinated, rng);
  /* The algorithm for randomly picking units is below, but first we handle the
   * special case where the random number generator is "fixed" at a particular
   * value, as it is for our deterministic tests. */
  if (rng->fixed)
    {
      num_chosen = (int) round (p * num_units);
      /* We only need to do this once, and then it will be the same pattern
       * used in every simulation. */
      if (!local_data->set_fixed_pattern)
        {
          /* Distribute the proper proportion of vaccinated units evenly through
           * the population. */
          HRD_herd_t **a, **b;
          for (i = 1; i < num_chosen; i++)
            {
              a = &g_array_index (local_data->units, HRD_herd_t *, i);
              b = &g_array_index (local_data->units, HRD_herd_t *, (guint) floor (i/p));
              *a = *b;
            }
          local_data->set_fixed_pattern = TRUE;
        }
      lo = 0;
      hi = num_chosen;
    } /* end of case where the random number generator is "fixed" */
  else
    {
      /* If the proportion of units to vaccinate is small, pick the units to
       * vaccinate; if the proportion is large, pick the units NOT to
       * vaccinate.  Use the Knuth-Fisher-Yates shuffle to sample without
       * replacement from the units. */
      num_chosen = (int) round (MIN(p, 1.0 - p) * num_units);
      for (i = 0; i < num_chosen; i++)
        {
          guint k;
          HRD_herd_t *tmp;
          HRD_herd_t **a, **b;
          /* Knuth-Fisher-Yates shuffle */
          k = (guint) floor (RAN_num(rng) * (num_units - i));
          a = &g_array_index (local_data->units, HRD_herd_t *, k);
          b = &g_array_index (local_data->units, HRD_herd_t *, num_units - i - 1);
          tmp = *a;
          *a = *b;
          *b = tmp;
        }
      /* Now the top num_chosen items in the array are random picks. */
      if (p < 0.5)
        {
          /* We picked the units to vaccinate. */
          lo = num_units - num_chosen;
          hi = num_units;
        }
      else
        {
          /* We picked the units NOT to vaccinate. */
          lo = 0;
          hi = num_units - num_chosen;
        }
    }
  /* Do the vaccinations. */
  for (i = lo; i < hi; i++)
    {
      int days_since_vaccination;
      unit = g_array_index (local_data->units, HRD_herd_t *, i);
      /* How long ago was this unit vaccinated?  Pick a number between 1 day
       * ago (the day before the simulation begins) and the number given in the
       * parameter "days_between_vaccinations". */
      days_since_vaccination = (int) floor (RAN_num(rng) * local_data->days_between_vaccinations) + 1;
      event =
        EVT_new_vaccination_event (unit,
                                   /* day = */ 0,
                                   prophylactic_vaccination_model_vaccination_reason,
                                   /* day commitment made = */ 0);
      event->u.vaccination.fast_forward = days_since_vaccination - 1;
#if DEBUG
      g_debug ("vaccinated unit \"%s\" %i days before simulation starts",
               unit->official_id, days_since_vaccination);
#endif
      EVT_event_enqueue (queue, event);
    }

#if DEBUG
  g_debug ("----- EXIT handle_before_each_simulation_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Runs this module.
 *
 * @param self this module.
 * @param herds a herd list.
 * @param zones a zone list.
 * @param event the event that caused the model to run.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
run (struct naadsm_model_t_ *self, HRD_herd_list_t * herds, ZON_zone_list_t * zones,
     EVT_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
#if DEBUG
  g_debug ("----- ENTER run (%s)", MODEL_NAME);
#endif

  switch (event->type)
    {
    case EVT_BeforeAnySimulations:
      handle_before_any_simulations_event (queue);
      break;
    case EVT_BeforeEachSimulation:
      handle_before_each_simulation_event (self, herds, rng, queue);
      break;
    default:
      g_error
        ("%s has received a %s event, which it does not listen for.  This should never happen.  Please contact the developer.",
         MODEL_NAME, EVT_event_type_name[event->type]);
    }

#if DEBUG
  g_debug ("----- EXIT run (%s)", MODEL_NAME);
#endif
}



/**
 * Resets this module after a simulation run.
 *
 * @param self this module.
 */
void
reset (struct naadsm_model_t_ *self)
{
#if DEBUG
  g_debug ("----- ENTER reset (%s)", MODEL_NAME);
#endif

  /* Nothing to do. */

#if DEBUG
  g_debug ("----- EXIT reset (%s)", MODEL_NAME);
#endif
}



/**
 * Reports whether this module is listening for a given event type.
 *
 * @param self this module.
 * @param event_type an event type.
 * @return TRUE if this module is listening for the event type.
 */
gboolean
is_listening_for (struct naadsm_model_t_ *self, EVT_event_type_t event_type)
{
  int i;

  for (i = 0; i < self->nevents_listened_for; i++)
    if (self->events_listened_for[i] == event_type)
      return TRUE;
  return FALSE;
}



/**
 * Reports whether this module has any pending actions to carry out.
 *
 * @param self this module.
 * @return TRUE if this module has pending actions.
 */
gboolean
has_pending_actions (struct naadsm_model_t_ * self)
{
  return FALSE;
}



/**
 * Returns a text representation of this module.
 *
 * @param self this module.
 * @return a string.
 */
char *
to_string (struct naadsm_model_t_ *self)
{
  local_data_t *local_data;
  GString *s;
  char *substring, *chararray;

  local_data = (local_data_t *) (self->model_data);
  s = g_string_new (NULL);
  g_string_sprintf (s, "<%s for %s", MODEL_NAME, local_data->production_type_name);

  substring = PDF_dist_to_string (local_data->proportion_of_units_vaccinated);
  g_string_sprintfa (s, "\n  proportion-of-units-vaccinated=%s", substring);
  g_free (substring);

  g_string_sprintfa (s, "\n  time-between-vaccinations=%i>", local_data->days_between_vaccinations);

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Prints this module to a stream.
 *
 * @param stream a stream to write to.
 * @param self this module.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
local_fprintf (FILE * stream, struct naadsm_model_t_ *self)
{
  char *s;
  int nchars_written;

  s = to_string (self);
  nchars_written = fprintf (stream, "%s", s);
  free (s);
  return nchars_written;
}



/**
 * Prints this module.
 *
 * @param self this module.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
local_printf (struct naadsm_model_t_ *self)
{
  return local_fprintf (stdout, self);
}



/**
 * Frees this module.  Does not free the production type name.
 *
 * @param self this module.
 */
void
local_free (struct naadsm_model_t_ *self)
{
  local_data_t *local_data;

#if DEBUG
  g_debug ("----- ENTER free (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  PDF_free_dist (local_data->proportion_of_units_vaccinated);
  g_array_free (local_data->units, TRUE);
  g_free (local_data);
  g_ptr_array_free (self->outputs, TRUE);
  g_free (self);

#if DEBUG
  g_debug ("----- EXIT free (%s)", MODEL_NAME);
#endif
}



/**
 * Returns whether this module is a singleton or not.
 */
gboolean
is_singleton (void)
{
  return FALSE;
}



/**
 * Returns a new prophylactic vaccination model.
 */
naadsm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, projPJ projection,
     ZON_zone_list_t * zones)
{
  naadsm_model_t *self;
  local_data_t *local_data;
  gboolean *production_types;
  guint nprod_types;
  HRD_production_type_t production_type;
  guint nherds, i, count;
  scew_element const *e;
  gboolean success;
  HRD_herd_t *herd;

#if DEBUG
  g_debug ("----- ENTER new (%s)", MODEL_NAME);
#endif
  production_type = 0; /* This value does nothing except avoid a compiler warning on Windows */

  self = g_new (naadsm_model_t, 1);
  local_data = g_new (local_data_t, 1);

  self->name = MODEL_NAME;
  self->events_listened_for = events_listened_for;
  self->nevents_listened_for = NEVENTS_LISTENED_FOR;
  self->outputs = g_ptr_array_new ();
  self->model_data = local_data;
  self->run = run;
  self->reset = reset;
  self->is_listening_for = is_listening_for;
  self->has_pending_actions = has_pending_actions;
  self->to_string = to_string;
  self->printf = local_printf;
  self->fprintf = local_fprintf;
  self->free = local_free;

  /* Make sure the right XML subtree was sent. */
  g_assert (strcmp (scew_element_name (params), MODEL_NAME) == 0);

#if DEBUG
  g_debug ("setting production type");
#endif
  production_types = naadsm_read_prodtype_attribute (params, "production-type", herds->production_type_names);
  /* This module should apply to only one production type */
  nprod_types = herds->production_type_names->len;
  count = 0;
  for (i = 0; i < nprod_types; i++)
    if (production_types[i] == TRUE)
      {
        count++;
        production_type = i;
        local_data->production_type_name = (char *) g_ptr_array_index (herds->production_type_names, i);
      }
  if (count > 1)
    g_error ("%s: parameters must be given separately for each production type", MODEL_NAME);  

  e = scew_element_by_name (params, "proportion-of-units-vaccinated");
  if (e != NULL)
    local_data->proportion_of_units_vaccinated = PAR_get_PDF (e);
  else
    local_data->proportion_of_units_vaccinated = PDF_new_point_dist (0);

  e = scew_element_by_name (params, "time-between-vaccinations");
  if (e != NULL)
    {
      local_data->days_between_vaccinations = (int) round (PAR_get_time (e, &success));
      if (success == FALSE)
        {
          g_warning ("%s: setting time between vaccinations to 1 year", MODEL_NAME);
          local_data->days_between_vaccinations = 365;
        }
      else if (local_data->days_between_vaccinations < 1)
        {
          g_warning ("%s: time between vaccinations cannot be less than 1 day, setting time to 1 day", MODEL_NAME);
          local_data->days_between_vaccinations = 1;
        }
    }
  else
    {
      g_warning ("%s: time between vaccinations missing, setting 1 to year", MODEL_NAME);
      local_data->days_between_vaccinations = 365;
    }

  /* Find all units of the production type we're interested in. */
  local_data->units = g_array_new (FALSE, FALSE, sizeof(HRD_herd_t *));
  nherds = HRD_herd_list_length (herds);
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      if (herd->production_type == production_type)
        g_array_append_val (local_data->units, herd);
    }
  local_data->set_fixed_pattern = FALSE;

#if DEBUG
  g_debug ("----- EXIT new (%s)", MODEL_NAME);
#endif

  return self;
}

/* end of file prophylactic-vaccination-model.c */
