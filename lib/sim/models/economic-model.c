/** @file economic-model.c
 * Module that tallies costs of an outbreak.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.2
 * @date June 2003
 *
 * Copyright &copy; University of Guelph, 2003-2006
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * @todo Add a baseline vaccination capacity and increase in cost past the
 *   baseline.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* To avoid name clashes when dlpreopening multiple modules that have the same
 * global symbols (interface).  See sec. 18.4 of "GNU Autoconf, Automake, and
 * Libtool". */
#define new economic_model_LTX_new
#define run economic_model_LTX_run
#define reset economic_model_LTX_reset
#define events_listened_for economic_model_LTX_events_listened_for
#define is_listening_for economic_model_LTX_is_listening_for
#define has_pending_actions economic_model_LTX_has_pending_actions
#define has_pending_infections economic_model_LTX_has_pending_infections
#define to_string economic_model_LTX_to_string
#define local_printf economic_model_LTX_printf
#define local_fprintf economic_model_LTX_fprintf
#define local_free economic_model_LTX_free
#define handle_vaccination_event economic_model_LTX_handle_vaccination_event
#define handle_destruction_event economic_model_LTX_handle_destruction_event
#define events_created economic_model_LTX_events_created

#include "model.h"
#include "model_util.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#include "guilib.h"

#include "economic-model.h"

extern const char *RPT_frequency_name[];

/** This must match an element name in the DTD. */
#define MODEL_NAME "economic-model"

#define MODEL_DESCRIPTION "\
A module to tally costs of an outbreak.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 June 2003\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 0
EVT_event_type_t events_created[] = { 0 };

#define NEVENTS_LISTENED_FOR 2
EVT_event_type_t events_listened_for[] = { EVT_Vaccination, EVT_Destruction };



/* Specialized information for this model. */
typedef struct
{
  gboolean *production_type;
  GPtrArray *production_types;
  double appraisal;
  double euthanasia;
  double indemnification;
  double carcass_disposal;
  double cleaning_disinfecting;
  REL_chart_t *visiting;
  REL_chart_t *testing;
  double vaccination_fixed;
  double vaccination;
  unsigned int baseline_capacity, capacity_used;
  double extra_vaccination;
  RPT_reporting_t *total_cost;
  RPT_reporting_t *appraisal_cost;
  RPT_reporting_t *euthanasia_cost;
  RPT_reporting_t *indemnification_cost;
  RPT_reporting_t *carcass_disposal_cost;
  RPT_reporting_t *cleaning_disinfecting_cost;
  RPT_reporting_t *vaccination_cost;
  RPT_reporting_t *cumul_total_cost;
  RPT_reporting_t *cumul_appraisal_cost;
  RPT_reporting_t *cumul_euthanasia_cost;
  RPT_reporting_t *cumul_indemnification_cost;
  RPT_reporting_t *cumul_carcass_disposal_cost;
  RPT_reporting_t *cumul_cleaning_disinfecting_cost;
  RPT_reporting_t *cumul_vaccination_cost;
}
local_data_t;


/**
 * Responds to a vaccination event by computing its cost.
 *
 * @param self the model.
 * @param event a vaccination event.
 */
void
handle_vaccination_event (struct ergadm_model_t_ *self, EVT_vaccination_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;
  double cost = 0;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_vaccination_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  herd = event->herd;

  if (local_data->production_type[herd->production_type] == TRUE)
    {
      /* Fixed cost for the herd. */
      cost += local_data->vaccination_fixed;

      /* Per-animal cost. */
      cost += local_data->vaccination * herd->size;

      if (local_data->capacity_used > local_data->baseline_capacity)
        {
          cost += local_data->extra_vaccination * herd->size;
        }
      else
        {
          local_data->capacity_used += herd->size;
          if (local_data->capacity_used > local_data->baseline_capacity)
            cost += local_data->extra_vaccination *
              (local_data->capacity_used - local_data->baseline_capacity);
        }

      RPT_reporting_add_real (local_data->vaccination_cost, cost, NULL);
      RPT_reporting_add_real (local_data->total_cost, cost, NULL);
      RPT_reporting_add_real (local_data->cumul_vaccination_cost, cost, NULL);
      RPT_reporting_add_real (local_data->cumul_total_cost, cost, NULL);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_vaccination_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a destruction event by computing its cost.
 *
 * @param self the model.
 * @param event a destruction event.
 */
void
handle_destruction_event (struct ergadm_model_t_ *self, EVT_destruction_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;
  unsigned int size;
  double cost, sum = 0;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_destruction_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  herd = event->herd;
  size = herd->size;

  if (local_data->production_type[herd->production_type] == TRUE)
    {
      cost = local_data->appraisal;
      RPT_reporting_add_real (local_data->appraisal_cost, cost, NULL);
      RPT_reporting_add_real (local_data->cumul_appraisal_cost, cost, NULL);
      sum += cost;

      cost = size * local_data->euthanasia;
      RPT_reporting_add_real (local_data->euthanasia_cost, cost, NULL);
      RPT_reporting_add_real (local_data->cumul_euthanasia_cost, cost, NULL);
      sum += cost;

      cost = size * local_data->indemnification;
      RPT_reporting_add_real (local_data->indemnification_cost, cost, NULL);
      RPT_reporting_add_real (local_data->cumul_indemnification_cost, cost, NULL);
      sum += cost;

      cost = size * local_data->carcass_disposal;
      RPT_reporting_add_real (local_data->carcass_disposal_cost, cost, NULL);
      RPT_reporting_add_real (local_data->cumul_carcass_disposal_cost, cost, NULL);
      sum += cost;

      cost += local_data->cleaning_disinfecting;
      RPT_reporting_add_real (local_data->cleaning_disinfecting_cost, cost, NULL);
      RPT_reporting_add_real (local_data->cumul_cleaning_disinfecting_cost, cost, NULL);
      sum += cost;

      RPT_reporting_add_real (local_data->total_cost, sum, NULL);
      RPT_reporting_add_real (local_data->cumul_total_cost, sum, NULL);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_destruction_event (%s)", MODEL_NAME);
#endif
}



/**
 * Runs this model.
 *
 * @param self the model.
 * @param herds a herd list.
 * @param zones a zone list.
 * @param event the event that caused the model to run.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
run (struct ergadm_model_t_ *self, HRD_herd_list_t * herds, ZON_zone_list_t * zones,
     EVT_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER run (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "ENTER run %s", MODEL_NAME); 
    //guilib_printf( guilog );
  }

  switch (event->type)
    {
    case EVT_Vaccination:
      handle_vaccination_event (self, &(event->u.vaccination));
      break;
    case EVT_Destruction:
      handle_destruction_event (self, &(event->u.destruction));
      break;
    default:
      g_error
        ("%s has received a %s event, which it does not listen for.  This should never happen.  Please contact the developer.",
         MODEL_NAME, EVT_event_type_name[event->type]);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT run (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "EXIT run %s", MODEL_NAME); 
    //guilib_printf( guilog );
  }
}



/**
 * Resets this model after a simulation run.
 *
 * @param self the model.
 */
void
reset (struct ergadm_model_t_ *self)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  RPT_reporting_reset (local_data->total_cost);
  RPT_reporting_reset (local_data->appraisal_cost);
  RPT_reporting_reset (local_data->euthanasia_cost);
  RPT_reporting_reset (local_data->indemnification_cost);
  RPT_reporting_reset (local_data->carcass_disposal_cost);
  RPT_reporting_reset (local_data->cleaning_disinfecting_cost);
  RPT_reporting_reset (local_data->vaccination_cost);
  RPT_reporting_reset (local_data->cumul_total_cost);
  RPT_reporting_reset (local_data->cumul_appraisal_cost);
  RPT_reporting_reset (local_data->cumul_euthanasia_cost);
  RPT_reporting_reset (local_data->cumul_indemnification_cost);
  RPT_reporting_reset (local_data->cumul_carcass_disposal_cost);
  RPT_reporting_reset (local_data->cumul_cleaning_disinfecting_cost);
  RPT_reporting_reset (local_data->cumul_vaccination_cost);

  local_data->capacity_used = 0;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT reset (%s)", MODEL_NAME);
#endif
}



/**
 * Reports whether this model is listening for a given event type.
 *
 * @param self the model.
 * @param event_type an event type.
 * @return TRUE if the model is listening for the event type.
 */
gboolean
is_listening_for (struct ergadm_model_t_ *self, EVT_event_type_t event_type)
{
  int i;

  for (i = 0; i < self->nevents_listened_for; i++)
    if (self->events_listened_for[i] == event_type)
      return TRUE;
  return FALSE;
}



/**
 * Reports whether this model has any pending actions to carry out.
 *
 * @param self the model.
 * @return TRUE if the model has pending actions.
 */
gboolean
has_pending_actions (struct ergadm_model_t_ * self)
{
  return FALSE;
}



/**
 * Reports whether this model has any pending infections to cause.
 *
 * @param self the model.
 * @return TRUE if the model has pending infections.
 */
gboolean
has_pending_infections (struct ergadm_model_t_ * self)
{
  return FALSE;
}



/**
 * Returns a text representation of this model.
 *
 * @param self the model.
 * @return a string.
 */
char *
to_string (struct ergadm_model_t_ *self)
{
  GString *s;
  gboolean already_names;
  unsigned int i;
  char *substring, *chararray;
  local_data_t *local_data;

  local_data = (local_data_t *) (self->model_data);
  s = g_string_new (NULL);
  g_string_sprintf (s, "<%s for ", MODEL_NAME);
  already_names = FALSE;
  for (i = 0; i < local_data->production_types->len; i++)
    if (local_data->production_type[i] == TRUE)
      {
        if (already_names)
          g_string_append_printf (s, ",%s",
                                  (char *) g_ptr_array_index (local_data->production_types, i));
        else
          {
            g_string_append_printf (s, "%s",
                                    (char *) g_ptr_array_index (local_data->production_types, i));
            already_names = TRUE;
          }
      }

  g_string_sprintfa (s, "\n  appraisal (per unit)=%g\n", local_data->appraisal);
  g_string_sprintfa (s, "  euthanasia (per animal)=%g\n", local_data->euthanasia);
  g_string_sprintfa (s, "  indemnification (per animal)=%g\n", local_data->indemnification);
  g_string_sprintfa (s, "  carcass-disposal (per animal)=%g\n", local_data->carcass_disposal);
  g_string_sprintfa (s, "  cleaning-disinfecting (per unit)=%g\n",
                     local_data->cleaning_disinfecting);

  substring = REL_chart_to_string (local_data->visiting);
  g_string_sprintfa (s, "  visiting=%s\n", substring);
  free (substring);

  substring = REL_chart_to_string (local_data->testing);
  g_string_sprintfa (s, "  testing=%s\n", substring);
  free (substring);

  g_string_sprintfa (s, "  vaccination-fixed (per unit)=%g\n", local_data->vaccination_fixed);
  g_string_sprintfa (s, "  vaccination (per animal)=%g\n", local_data->vaccination);
  g_string_sprintfa (s, "  baseline-capacity=%u\n", local_data->baseline_capacity);
  g_string_sprintfa (s, "  additional-vaccination (per animal)=%g>", local_data->extra_vaccination);

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Prints this model to a stream.
 *
 * @param stream a stream to write to.
 * @param self the model.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
local_fprintf (FILE * stream, struct ergadm_model_t_ *self)
{
  char *s;
  int nchars_written;

  s = to_string (self);
  nchars_written = fprintf (stream, "%s", s);
  free (s);
  return nchars_written;
}



/**
 * Prints this model.
 *
 * @param self the model.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
local_printf (struct ergadm_model_t_ *self)
{
  return local_fprintf (stdout, self);
}



/**
 * Frees this model.  Does not free the production type name.
 *
 * @param self the model.
 */
void
local_free (struct ergadm_model_t_ *self)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER free (%s)", MODEL_NAME);
#endif

  /* Free the dynamically-allocated parts. */
  local_data = (local_data_t *) (self->model_data);
  g_free (local_data->production_type);
  REL_free_chart (local_data->visiting);
  REL_free_chart (local_data->testing);

  RPT_free_reporting (local_data->total_cost, TRUE);
  RPT_free_reporting (local_data->appraisal_cost, TRUE);
  RPT_free_reporting (local_data->euthanasia_cost, TRUE);
  RPT_free_reporting (local_data->indemnification_cost, TRUE);
  RPT_free_reporting (local_data->carcass_disposal_cost, TRUE);
  RPT_free_reporting (local_data->cleaning_disinfecting_cost, TRUE);
  RPT_free_reporting (local_data->vaccination_cost, TRUE);
  RPT_free_reporting (local_data->cumul_total_cost, TRUE);
  RPT_free_reporting (local_data->cumul_appraisal_cost, TRUE);
  RPT_free_reporting (local_data->cumul_euthanasia_cost, TRUE);
  RPT_free_reporting (local_data->cumul_indemnification_cost, TRUE);
  RPT_free_reporting (local_data->cumul_carcass_disposal_cost, TRUE);
  RPT_free_reporting (local_data->cumul_cleaning_disinfecting_cost, TRUE);
  RPT_free_reporting (local_data->cumul_vaccination_cost, TRUE);

  g_free (local_data);
  g_ptr_array_free (self->outputs, TRUE);
  g_free (self);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT free (%s)", MODEL_NAME);
#endif
}



/**
 * Returns the version of the interface this model conforms to.
 */
char *
interface_version (void)
{
  return MODEL_INTERFACE_VERSION;
}



/**
 * Returns a new economic model.
 */
ergadm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  ergadm_model_t *m;
  local_data_t *local_data;
  scew_element *e, **ee;
  gboolean success;
  unsigned int noutputs;
  RPT_reporting_t *output;
  const XML_Char *variable_name;
  int i, j;                     /* loop counters */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER new (%s)", MODEL_NAME);
#endif

  m = g_new (ergadm_model_t, 1);
  local_data = g_new (local_data_t, 1);

  m->name = MODEL_NAME;
  m->description = MODEL_DESCRIPTION;
  m->events_created = events_created;
  m->nevents_created = NEVENTS_CREATED;
  m->events_listened_for = events_listened_for;
  m->nevents_listened_for = NEVENTS_LISTENED_FOR;
  m->outputs = g_ptr_array_sized_new (14);
  m->model_data = local_data;
  m->run = run;
  m->reset = reset;
  m->is_listening_for = is_listening_for;
  m->has_pending_actions = has_pending_actions;
  m->has_pending_infections = has_pending_infections;
  m->to_string = to_string;
  m->printf = local_printf;
  m->fprintf = local_fprintf;
  m->free = local_free;

  /* Make sure the right XML subtree was sent. */
  g_assert (strcmp (scew_element_name (params), MODEL_NAME) == 0);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "setting production types");
#endif
  local_data->production_types = herds->production_type_names;
  local_data->production_type =
    ergadm_read_prodtype_attribute (params, "production-type", herds->production_type_names);

  e = scew_element_by_name (params, "appraisal");
  if (e != NULL)
    {
      local_data->appraisal = PAR_get_money (e, &success);
      if (success == FALSE)
        {
          g_warning ("%s: setting per-unit appraisal cost to 0", MODEL_NAME);
          local_data->appraisal = 0;
        }
    }
  else
    {
      g_warning ("%s: per-unit appraisal cost missing, setting to 0", MODEL_NAME);
      local_data->appraisal = 0;
    }

  e = scew_element_by_name (params, "euthanasia");
  if (e != NULL)
    {
      local_data->euthanasia = PAR_get_money (e, &success);
      if (success == FALSE)
        {
          g_warning ("%s: setting per-animal euthanasia cost to 0", MODEL_NAME);
          local_data->euthanasia = 0;
        }
    }
  else
    {
      g_warning ("%s: per-animal euthanasia cost missing, setting to 0", MODEL_NAME);
      local_data->euthanasia = 0;
    }

  e = scew_element_by_name (params, "indemnification");
  if (e != NULL)
    {
      local_data->indemnification = PAR_get_money (e, &success);
      if (success == FALSE)
        {
          g_warning ("%s: setting per-animal indemnification cost to 0", MODEL_NAME);
          local_data->indemnification = 0;
        }
    }
  else
    {
      g_warning ("%s: per-animal indemnification cost missing, setting to 0", MODEL_NAME);
      local_data->indemnification = 0;
    }

  e = scew_element_by_name (params, "carcass-disposal");
  if (e != NULL)
    {
      local_data->carcass_disposal = PAR_get_money (e, &success);
      if (success == FALSE)
        {
          g_warning ("%s: setting per-animal carcass disposal cost to 0", MODEL_NAME);
          local_data->carcass_disposal = 0;
        }
    }
  else
    {
      g_warning ("%s: per-animal carcass disposal cost missing, setting to 0", MODEL_NAME);
      local_data->carcass_disposal = 0;
    }

  e = scew_element_by_name (params, "cleaning-disinfecting");
  if (e != NULL)
    {
      local_data->cleaning_disinfecting = PAR_get_money (e, &success);
      if (success == FALSE)
        {
          g_warning ("%s: setting per-unit cleaning and disinfecting cost to 0", MODEL_NAME);
          local_data->cleaning_disinfecting = 0;
        }
    }
  else
    {
      g_warning ("%s: per-unit cleaning and disinfecting cost missing, setting to 0", MODEL_NAME);
      local_data->cleaning_disinfecting = 0;
    }

  e = scew_element_by_name (params, "visiting");
  if (e != NULL)
    {
      local_data->visiting = PAR_get_relationship_chart (e);
    }
  else
    {
      g_warning ("%s: visiting cost missing, setting to 0", MODEL_NAME);
      local_data->visiting = REL_new_point_chart (0);
    }

  e = scew_element_by_name (params, "testing");
  if (e != NULL)
    {
      local_data->testing = PAR_get_relationship_chart (e);
    }
  else
    {
      g_warning ("%s: testing cost missing, setting to 0", MODEL_NAME);
      local_data->testing = REL_new_point_chart (0);
    }

  e = scew_element_by_name (params, "vaccination-fixed");
  if (e != NULL)
    {
      local_data->vaccination_fixed = PAR_get_money (e, &success);
      if (success == FALSE)
        {
          g_warning ("%s: setting per-unit vaccination cost to 0", MODEL_NAME);
          local_data->vaccination_fixed = 0;
        }
    }
  else
    {
      g_warning ("%s: per-unit vaccination cost missing, setting to 0", MODEL_NAME);
      local_data->vaccination_fixed = 0;
    }

  e = scew_element_by_name (params, "vaccination");
  if (e != NULL)
    {
      local_data->vaccination = PAR_get_money (e, &success);
      if (success == FALSE)
        {
          g_warning ("%s: setting per-animal vaccination cost to 0", MODEL_NAME);
          local_data->vaccination = 0;
        }
    }
  else
    {
      g_warning ("%s: per-animal vaccination cost missing, setting to 0", MODEL_NAME);
      local_data->vaccination = 0;
    }

  e = scew_element_by_name (params, "baseline-vaccination-capacity");
  if (e != NULL)
    {
      local_data->baseline_capacity = (unsigned int) PAR_get_unitless (e, &success);
      if (success == FALSE)
        {
          g_warning ("%s: setting baseline vaccination capacity to 1,000,000", MODEL_NAME);
          local_data->baseline_capacity = 1000000;
        }
    }
  else
    {
      g_warning ("%s: baseline vaccination capacity missing, setting to 1,000,000", MODEL_NAME);
      local_data->baseline_capacity = 1000000;
    }

  e = scew_element_by_name (params, "additional-vaccination");
  if (e != NULL)
    {
      local_data->extra_vaccination = PAR_get_money (e, &success);
      if (success == FALSE)
        {
          g_warning ("%s: setting additional per-animal vaccination cost to 0", MODEL_NAME);
          local_data->extra_vaccination = 0;
        }
    }
  else
    {
      g_warning ("%s: additional per-animal vaccination cost missing, setting to 0", MODEL_NAME);
      local_data->extra_vaccination = 0;
    }

  local_data->total_cost = RPT_new_reporting ("total-cost", NULL, RPT_real, RPT_never, FALSE);
  local_data->appraisal_cost =
    RPT_new_reporting ("appraisal-cost", NULL, RPT_real, RPT_never, FALSE);
  local_data->euthanasia_cost =
    RPT_new_reporting ("euthanasia-cost", NULL, RPT_real, RPT_never, FALSE);
  local_data->indemnification_cost =
    RPT_new_reporting ("indemnification-cost", NULL, RPT_real, RPT_never, FALSE);
  local_data->carcass_disposal_cost =
    RPT_new_reporting ("carcass-disposal-cost", NULL, RPT_real, RPT_never, FALSE);
  local_data->cleaning_disinfecting_cost =
    RPT_new_reporting ("cleaning-and-disinfecting-cost", NULL, RPT_real, RPT_never, FALSE);
  local_data->vaccination_cost =
    RPT_new_reporting ("vaccination-cost", NULL, RPT_real, RPT_never, FALSE);
  local_data->cumul_total_cost =
    RPT_new_reporting ("cumulative-total-cost", NULL, RPT_real, RPT_never, TRUE);
  local_data->cumul_appraisal_cost =
    RPT_new_reporting ("cumulative-appraisal-cost", NULL, RPT_real, RPT_never, TRUE);
  local_data->cumul_euthanasia_cost =
    RPT_new_reporting ("cumulative-euthanasia-cost", NULL, RPT_real, RPT_never, TRUE);
  local_data->cumul_indemnification_cost =
    RPT_new_reporting ("cumulative-indemnification-cost", NULL, RPT_real, RPT_never, TRUE);
  local_data->cumul_carcass_disposal_cost =
    RPT_new_reporting ("cumulative-carcass-disposal-cost", NULL, RPT_real, RPT_never, TRUE);
  local_data->cumul_cleaning_disinfecting_cost =
    RPT_new_reporting ("cumulative-cleaning-and-disinfecting-cost", NULL, RPT_real, RPT_never,
                       TRUE);
  local_data->cumul_vaccination_cost =
    RPT_new_reporting ("cumulative-vaccination-cost", NULL, RPT_real, RPT_never, TRUE);
  g_ptr_array_add (m->outputs, local_data->total_cost);
  g_ptr_array_add (m->outputs, local_data->appraisal_cost);
  g_ptr_array_add (m->outputs, local_data->euthanasia_cost);
  g_ptr_array_add (m->outputs, local_data->indemnification_cost);
  g_ptr_array_add (m->outputs, local_data->carcass_disposal_cost);
  g_ptr_array_add (m->outputs, local_data->cleaning_disinfecting_cost);
  g_ptr_array_add (m->outputs, local_data->vaccination_cost);
  g_ptr_array_add (m->outputs, local_data->cumul_total_cost);
  g_ptr_array_add (m->outputs, local_data->cumul_appraisal_cost);
  g_ptr_array_add (m->outputs, local_data->cumul_euthanasia_cost);
  g_ptr_array_add (m->outputs, local_data->cumul_indemnification_cost);
  g_ptr_array_add (m->outputs, local_data->cumul_carcass_disposal_cost);
  g_ptr_array_add (m->outputs, local_data->cumul_cleaning_disinfecting_cost);
  g_ptr_array_add (m->outputs, local_data->cumul_vaccination_cost);

  /* Set the reporting frequency for the output variables. */
  ee = scew_element_list (params, "output", &noutputs);
#if INFO
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "%i output variables", noutputs);
#endif
  for (i = 0; i < noutputs; i++)
    {
      e = ee[i];
      variable_name = scew_element_contents (scew_element_by_name (e, "variable-name"));
      /* Do the outputs include a variable with this name? */
      for (j = 0; j < m->outputs->len; j++)
        {
          output = (RPT_reporting_t *) g_ptr_array_index (m->outputs, j);
          if (strcmp (output->name, variable_name) == 0)
            break;
        }
      if (j == m->outputs->len)
        g_warning ("no output variable named \"%s\", ignoring", variable_name);
      else
        {
          RPT_reporting_set_frequency (output,
                                       RPT_string_to_frequency (scew_element_contents
                                                                (scew_element_by_name
                                                                 (e, "frequency"))));
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "report \"%s\" %s", variable_name,
                 RPT_frequency_name[output->frequency]);
#endif
        }
    }
  free (ee);

  /* No vaccinations have been performed yet. */
  local_data->capacity_used = 0;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}

char *
economic_model_interface_version (void)
{
  return interface_version ();
}


ergadm_model_t *
economic_model_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file economic-model.c */
