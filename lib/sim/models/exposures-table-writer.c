/** @file exposures-table-writer.c
 * Writes out a table of exposures and infections in comma-separated value
 * (csv) format.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 *
 * Copyright &copy; University of Guelph, 2009-2010
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * @todo Add the ".csv" extension to filename if it's missing.
 * @todo check errno after opening the file for writing.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* To avoid name clashes when multiple modules have the same interface. */
#define is_singleton exposures_table_writer_is_singleton
#define new exposures_table_writer_new
#define run exposures_table_writer_run
#define reset exposures_table_writer_reset
#define events_listened_for exposures_table_writer_events_listened_for
#define is_listening_for exposures_table_writer_is_listening_for
#define has_pending_actions exposures_table_writer_has_pending_actions
#define to_string exposures_table_writer_to_string
#define local_printf exposures_table_writer_printf
#define local_fprintf exposures_table_writer_fprintf
#define local_free exposures_table_writer_free
#define handle_before_any_simulations_event exposures_table_writer_handle_before_any_simulations_event
#define handle_before_each_simulation_event exposures_table_writer_handle_before_each_simulation_event
#define handle_exposure_event exposures_table_writer_handle_exposure_event
#define handle_infection_event exposures_table_writer_handle_infection_event

#include "model.h"
#include "model_util.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#include "exposures-table-writer.h"

/** This must match an element name in the DTD. */
#define MODEL_NAME "exposures-table-writer"



#define NEVENTS_LISTENED_FOR 4
EVT_event_type_t events_listened_for[] = { EVT_BeforeAnySimulations,
  EVT_BeforeEachSimulation, EVT_Exposure, EVT_Infection };



/* Specialized information for this model. */
typedef struct
{
  char *filename; /* with the .csv extension */
  FILE *stream; /* The open file. */
  gboolean stream_is_stdout;
  int run_number;
}
local_data_t;



/**
 * Before any simulations, this module sets the run number to zero, opens its
 * output file and writes the table header.
 *
 * @param self the model.
 */
void
handle_before_any_simulations_event (struct naadsm_model_t_ * self)
{
  local_data_t *local_data;

#if DEBUG
  g_debug ("----- ENTER handle_before_any_simulations_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  if (!local_data->stream_is_stdout)
    {
      errno = 0;
      local_data->stream = fopen (local_data->filename, "w");
      if (errno != 0)
        {
          g_error ("%s: %s error when attempting to open file \"%s\"",
                   MODEL_NAME, strerror(errno), local_data->filename);
        }
    }
  fprintf (local_data->stream, "Run,Day,Type,Reason,Source ID,Production type,Lat,Lon,Recipient ID,Production type,Lat,Lon\n");
  
  /* This count will be incremented for each new simulation. */
  local_data->run_number = 0;

#if DEBUG
  g_debug ("----- EXIT handle_before_any_simulations_event (%s)", MODEL_NAME);
#endif
}



/**
 * Before each simulation, this module increments its "run number".
 *
 * @param self the model.
 */
void
handle_before_each_simulation_event (struct naadsm_model_t_ * self)
{
  local_data_t *local_data;

#if DEBUG
  g_debug ("----- ENTER handle_before_each_simulation_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  local_data->run_number++;

#if DEBUG
  g_debug ("----- EXIT handle_before_each_simulation_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to an exposure event by writing a table row.
 *
 * @param self the model.
 * @param event an exposure event.
 */
void
handle_exposure_event (struct naadsm_model_t_ *self,
                       EVT_exposure_event_t * event)
{
  local_data_t *local_data;

#if DEBUG
  g_debug ("----- ENTER handle_exposure_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Show all traceable exposures, but show untraceable exposures only if they
   * are adequate. */

  if (event->traceable || event->adequate)
    {
      /* The data fields are: run, day, type, reason, source ID, source production
       * type, source latitude, source longitude, recipient ID, recipient
       * production type, recipient latitude, recipient longitude. */
      if (event->exposing_herd == NULL)
        fprintf (local_data->stream,
                 "%i,%hu,Exposure,%s,,,,,%s,%s,%g,%g\n",
                 local_data->run_number,
                 event->day, event->cause,
                 event->exposed_herd->official_id,
                 event->exposed_herd->production_type_name,
                 event->exposed_herd->latitude,
                 event->exposed_herd->longitude);
      else
        fprintf (local_data->stream,
                 "%i,%hu,Exposure,%s,%s,%s,%g,%g,%s,%s,%g,%g\n",
                 local_data->run_number,
                 event->day, event->cause,
                 event->exposing_herd->official_id,
                 event->exposing_herd->production_type_name,
                 event->exposing_herd->latitude,
                 event->exposing_herd->longitude,
                 event->exposed_herd->official_id,
                 event->exposed_herd->production_type_name,
                 event->exposed_herd->latitude,
                 event->exposed_herd->longitude);
    }

#if DEBUG
  g_debug ("----- EXIT handle_exposure_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Responds to an infection event by writing a table row.
 *
 * @param self the model.
 * @param event an infection event.
 */
void
handle_infection_event (struct naadsm_model_t_ *self,
                        EVT_infection_event_t * event)
{
  local_data_t *local_data;

#if DEBUG
  g_debug ("----- ENTER handle_infection_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* The data fields are: run, day, type, reason, source ID, source production
   * type, source latitude, source longitude, recipient ID, recipient
   * production type, recipient latitude, recipient longitude. */
  fprintf (local_data->stream,
           "%i,%hu,Infection,,,,,,%s,%s,%g,%g\n",
           local_data->run_number,
           event->day,
           event->herd->official_id,
           event->herd->production_type_name,
           event->herd->latitude,
           event->herd->longitude);

#if DEBUG
  g_debug ("----- EXIT handle_infection_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Runs this model.
 *
 * @param self the model.
 * @param herds a list of herds.
 * @param zones a list of zones.
 * @param event the event that caused the model to run.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
run (struct naadsm_model_t_ *self, HRD_herd_list_t * herds,
     ZON_zone_list_t * zones, EVT_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
#if DEBUG
  g_debug ("----- ENTER run (%s)", MODEL_NAME);
#endif

  switch (event->type)
    {
    case EVT_BeforeAnySimulations:
      handle_before_any_simulations_event (self);
      break;
    case EVT_BeforeEachSimulation:
      handle_before_each_simulation_event (self);
      break;
    case EVT_Exposure:
      handle_exposure_event (self, &(event->u.exposure));
      break;
    case EVT_Infection:
      handle_infection_event (self, &(event->u.infection));
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
 * Resets this model after a simulation run.
 *
 * @param self the model.
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
  return;
}



/**
 * Reports whether this model is listening for a given event type.
 *
 * @param self the model.
 * @param event_type an event type.
 * @return TRUE if the model is listening for the event type.
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
 * Reports whether this model has any pending actions to carry out.
 *
 * @param self the model.
 * @return TRUE if the model has pending actions.
 */
gboolean
has_pending_actions (struct naadsm_model_t_ * self)
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
to_string (struct naadsm_model_t_ *self)
{
  local_data_t *local_data;
  GString *s;
  char *chararray;

  local_data = (local_data_t *) (self->model_data);
  s = g_string_new (NULL);
  g_string_sprintf (s, "<%s filename=\"%s\">", MODEL_NAME, local_data->filename);

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
 * Prints this model.
 *
 * @param self the model.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
local_printf (struct naadsm_model_t_ *self)
{
  return local_fprintf (stdout, self);
}



/**
 * Frees this model.
 *
 * @param self the model.
 */
void
local_free (struct naadsm_model_t_ *self)
{
  local_data_t *local_data;

#if DEBUG
  g_debug ("----- ENTER free (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Flush and close the file. */
  if (local_data->stream_is_stdout)
    fflush (local_data->stream);
  else
    fclose (local_data->stream);

  /* Free the dynamically-allocated parts. */
  g_free (local_data->filename);
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
  return TRUE;
}



/**
 * Returns a new exposures table writer.
 */
naadsm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, projPJ projection,
     ZON_zone_list_t * zones)
{
  naadsm_model_t *self;
  local_data_t *local_data;
  scew_element const *e;

#if DEBUG
  g_debug ("----- ENTER new (%s)", MODEL_NAME);
#endif

  self = g_new (naadsm_model_t, 1);
  local_data = g_new (local_data_t, 1);

  self->name = MODEL_NAME;
  self->events_listened_for = events_listened_for;
  self->nevents_listened_for = NEVENTS_LISTENED_FOR;
  self->outputs = g_ptr_array_sized_new (0);
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

  /* Get the filename for the table.  If the filename is omitted, blank, '-',
   * or 'stdout' (case insensitive), then the table is written to standard
   * output. */
  e = scew_element_by_name (params, "filename");
  if (e == NULL)
    {
      local_data->filename = g_strdup ("stdout"); /* just so we have something
        to display, and to free later */
      local_data->stream = stdout;
      local_data->stream_is_stdout = TRUE;    
    }
  else
    {
      local_data->filename = PAR_get_text (e);
      if (local_data->filename == NULL
          || g_ascii_strcasecmp (local_data->filename, "") == 0
          || g_ascii_strcasecmp (local_data->filename, "-") == 0
          || g_ascii_strcasecmp (local_data->filename, "stdout") == 0)
        {
          local_data->stream = stdout;
          local_data->stream_is_stdout = TRUE;
        }
      else
        {
          local_data->filename = naadsm_insert_node_number_into_filename (local_data->filename);
          local_data->stream_is_stdout = FALSE;
        }
    }

#if DEBUG
  g_debug ("----- EXIT new (%s)", MODEL_NAME);
#endif

  return self;
}

/* end of file exposures-table-writer.c */
