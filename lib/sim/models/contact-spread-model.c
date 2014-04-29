/** @file contact-spread-model.c
 * Module for contact spread.
 *
 * On each day, this module follows these steps:
 * <ol>
 *   <li>
 *     Look up a multiplier for the rate of movement of animals based on the
 *     number of days since a public announcement of an outbreak.  Use this
 *     multiplier to scale the movement rate specified in the parameters.
 *   <li>
 *     For each herd <i>A</i>,
 *     <ol>
 *       <li>
 *         Check whether <i>A</i> can be the source of an infection.  That is,
 *         is it Latent (applicable only for direct contact) or Infectious,
 *         and not quarantined?
 *       <li>
 *         If <i>A</i> cannot be a source, go on to the next herd.
 *       <li>
 *         Sample a number <i>N</i> from a Poisson distribution whose mean is
 *         the movement rate.
 *       <li>
 *         Create <i>N</i> exposures with <i>A</i> as the source.
 *     </ol>
 *   <li>
 *     For each exposure <i>E</i>,
 *     <ol>
 *       <li>
 *         Sample a number <i>TargetDistance</i> from the distance distribution
 *         specified in the parameters.
 *       <li>
 *         From all herds that can be the target of an infection (that is,
 *         those that are not Destroyed), choose the herd
 *         <i>B</i> whose distance from the source is closest to
 *         <i>TargetDistance</i>.
 *       <li>
 *         Record the exposure of the target herd.  This exposure can be
 *         discovered by trace-back investigations.
 *       <li>
 *         Generate a random number <i>r</i> in [0,1].
 *       <li>
 *         If doing direct contact and <i>r</i> < the prevalence in the source
 *         unit, or if doing indirect contact and <i>r</i> < the probability of
 *         infection given exposure,
 *         <ol>
 *           <li>
 *             Infect the target herd.
 *         </ol>
 *     </ol>
 * </ol>
 *
 * These calculations come from the document <i>SpreadModel Version 3.0
 * Diagrams and pseudocoding</i>, by Mark A. Schoenbaum of the
 * <a href="http://www.aphis.usda.gov/">USDA</a> and Francisco Zagmutt-Vergara
 * of <a href="http://www.colostate.edu/">Colorado State University</a>.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date November 2003
 *
 * Copyright &copy; University of Guelph, 2003-2007
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* To avoid name clashes when dlpreopening multiple modules that have the same
 * global symbols (interface).  See sec. 18.4 of "GNU Autoconf, Automake, and
 * Libtool". */
#define interface_version contact_spread_model_LTX_interface_version
#define is_singleton contact_spread_model_LTX_is_singleton
#define new contact_spread_model_LTX_new
#define set_params contact_spread_model_LTX_set_params
#define run contact_spread_model_LTX_run
#define reset contact_spread_model_LTX_reset
#define events_listened_for contact_spread_model_LTX_events_listened_for
#define is_listening_for contact_spread_model_LTX_is_listening_for
#define has_pending_actions contact_spread_model_LTX_has_pending_actions
#define has_pending_infections contact_spread_model_LTX_has_pending_infections
#define to_string contact_spread_model_LTX_to_string
#define local_printf contact_spread_model_LTX_printf
#define local_fprintf contact_spread_model_LTX_fprintf
#define local_free contact_spread_model_LTX_free
#define handle_request_for_exposure_causes_event contact_spread_model_LTX_handle_request_for_exposure_causes_event
#define handle_request_for_infection_causes_event contact_spread_model_LTX_handle_request_for_infection_causes_event
#define handle_new_day_event contact_spread_model_LTX_handle_new_day_event
#define handle_public_announcement_event contact_spread_model_LTX_handle_public_announcement_event
#define callback contact_spread_model_LTX_callback
#define check_and_choose contact_spread_model_LTX_check_and_choose
#define events_created contact_spread_model_LTX_events_created

#include "model.h"
#include "model_util.h"
#include "gis.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#include "guilib.h"

#include "contact-spread-model.h"

#if !HAVE_ROUND && HAVE_RINT
#  define round rint
#endif

double round (double x);

extern const char *HRD_status_name[];
extern const char *EVT_event_type_name[];
extern const char *EVT_contact_type_name[];

/** This must match an element name in the DTD. */
#define MODEL_NAME "contact-spread-model"

#define MODEL_DESCRIPTION "\
A module to simulate spread of a disease by direct contact (movement of animals\n\
between herds) and indirect contact (movement of people/equipment between\n\
herds).\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 November 2003\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 4
EVT_event_type_t events_created[] =
  { EVT_DeclarationOfExposureCauses, EVT_DeclarationOfInfectionCauses,
  EVT_Exposure, EVT_AttemptToInfect
};

#define NEVENTS_LISTENED_FOR 4
EVT_event_type_t events_listened_for[] =
  { EVT_RequestForExposureCauses, EVT_RequestForInfectionCauses, EVT_NewDay,
  EVT_PublicAnnouncement
};



#define EPSILON 0.01



/* Specialized information for this model. */
typedef struct
{
  double movement_rate;
  double fixed_movement_rate;
  REL_chart_t *movement_control; /**< Movement control charts by source and
    recipient production type, used when the source unit is not inside a zone
    focus.  When the source unit is inside a zone focus, use the charts by
    zone and production type, in local_data_t below. */
  PDF_dist_t *distance_dist;
  PDF_dist_t *shipping_delay;
  gboolean latent_units_can_infect;
  gboolean subclinical_units_can_infect;
  double prob_infect;
}
param_block_t;



/* Specialized information for this model. */
typedef struct
{
  GPtrArray *production_types; /**< Each item in the list is a char *. */
  ZON_zone_list_t *zones;
  param_block_t ***param_block[EVT_NCONTACT_TYPES]; /**< Blocks of parameters.
    Use an expression of the form
    param_block[contact_type][source_production_type][recipient_production_type]
    to get a pointer to a particular param_block. */
  REL_chart_t ***movement_control[EVT_NCONTACT_TYPES]; /**< Movement control
    charts for units inside zones.  Use an expression of the form
    movement_control[contact_type][zone->level-1][source_production_type]
    to get a pointer to a particular chart. */
  GPtrArray *pending_infections; /**< An array to store delayed contacts.  Each
    item in the array is a GQueue of Infection and Exposure events.  (Actually
    a singly-linked list would suffice, but the GQueue syntax is much more
    readable than the GSList syntax.)  An index "rotates" through the array, so
    an event that is to happen in 1 day is placed in the GQueue that is 1 place
    ahead of the rotating index, an event that is to happen in 2 days is placed
    in the GQueue that is 2 places ahead of the rotating index, etc. */
  unsigned int npending_infections;
  unsigned int rotating_index; /**< To go with pending_infections. */
  double use_rtree_index;
  gboolean outbreak_known;
  unsigned short int public_announcement_day;
}
local_data_t;


/**
 * Responds to a request for exposure causes by declaring all the causes which
 * this model may state for an exposure.
 *
 * @param self the model.
 * @param queue for any new events the model creates.
 */
void
handle_request_for_exposure_causes_event (struct ergadm_model_t_ *self, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  unsigned int nprod_types, i, j;
  gboolean causes_direct, causes_indirect;
  GPtrArray *causes;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_request_for_exposure_causes_event (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "ENTER handle_request_for_exposure_causes_event %s", MODEL_NAME); 
    guilib_printf( guilog );
  }
  
  local_data = (local_data_t *) (self->model_data);

  /* Find out if any parameters have been defined for direct contact. */
  nprod_types = local_data->production_types->len;
  causes_direct = FALSE;
  for (i = 0; i < nprod_types && causes_direct == FALSE; i++)
    if (local_data->param_block[DirectContact][i] != NULL)
      for (j = 0; j < nprod_types && causes_direct == FALSE; j++)
        if (local_data->param_block[DirectContact][i][j] != NULL)
          causes_direct = TRUE;

  /* Find out if any parameters have been defined for indirect contact. */
  causes_indirect = FALSE;
  for (i = 0; i < nprod_types && causes_indirect == FALSE; i++)
    if (local_data->param_block[IndirectContact][i] != NULL)
      for (j = 0; j < nprod_types && causes_indirect == FALSE; j++)
        if (local_data->param_block[IndirectContact][i][j] != NULL)
          causes_indirect = TRUE;

  causes = g_ptr_array_new ();
  if (causes_direct)
    g_ptr_array_add (causes, (char *) EVT_contact_type_name[DirectContact]);
  if (causes_indirect)
    g_ptr_array_add (causes, (char *) EVT_contact_type_name[IndirectContact]);
  EVT_event_enqueue (queue, EVT_new_declaration_of_exposure_causes_event (causes));

  /* Note that we don't clean up the GPtrArray.  It will be freed along with
   * the declaration event after all interested sub-models have processed the
   * event. */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_request_for_exposure_causes_event (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "EXIT handle_request_for_exposure_causes_event %s", MODEL_NAME); 
    guilib_printf( guilog );
  }
  
  return;
}



/**
 * Responds to a request for infection causes by declaring all the causes which
 * this model may state for an infection.
 *
 * @param self the model.
 * @param queue for any new events the model creates.
 */
void
handle_request_for_infection_causes_event (struct ergadm_model_t_ *self, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  unsigned int nprod_types, i, j;
  gboolean causes_direct, causes_indirect;
  GPtrArray *causes;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_request_for_infection_causes_event (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "ENTER handle_request_for_infection_causes_event %s", MODEL_NAME); 
    guilib_printf( guilog );
  }

  local_data = (local_data_t *) (self->model_data);

  /* Find out if any parameters have been defined for direct contact. */
  nprod_types = local_data->production_types->len;
  causes_direct = FALSE;
  for (i = 0; i < nprod_types && causes_direct == FALSE; i++)
    if (local_data->param_block[DirectContact][i] != NULL)
      for (j = 0; j < nprod_types && causes_direct == FALSE; j++)
        if (local_data->param_block[DirectContact][i][j] != NULL)
          causes_direct = TRUE;

  /* Find out if any parameters have been defined for indirect contact. */
  causes_indirect = FALSE;
  for (i = 0; i < nprod_types && causes_indirect == FALSE; i++)
    if (local_data->param_block[IndirectContact][i] != NULL)
      for (j = 0; j < nprod_types && causes_indirect == FALSE; j++)
        if (local_data->param_block[IndirectContact][i][j] != NULL)
          causes_indirect = TRUE;

  causes = g_ptr_array_new ();
  if (causes_direct)
    g_ptr_array_add (causes, (char *) EVT_contact_type_name[DirectContact]);
  if (causes_indirect)
    g_ptr_array_add (causes, (char *) EVT_contact_type_name[IndirectContact]);
  EVT_event_enqueue (queue, EVT_new_declaration_of_infection_causes_event (causes));

  /* Note that we don't clean up the GPtrArray.  It will be freed along with
   * the declaration event after all interested sub-models have processed the
   * event. */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_request_for_infection_causes_event (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "EXIT handle_request_for_infection_causes_event %s", MODEL_NAME); 
    guilib_printf( guilog );
  }
  
  return;
}



/**
 * Special structure for use with the callback function below.
 */
typedef struct
{
  struct ergadm_model_t_ *self;
  HRD_herd_list_t *herds;
  ZON_zone_list_t *zones;
  RAN_gen_t *rng;
  HRD_herd_t *herd1;
  ZON_zone_fragment_t *herd1_fragment;
  EVT_contact_type_t contact_type;
  unsigned int recipient_production_type;
  double movement_distance;
  HRD_herd_t *best_herd; /**< The current pick for best recipient. */
  double best_herd_distance; /**< The distance from the source unit to
    best_herd. */
  double best_herd_difference; /**< The difference between the desired contact
    distance and best_herd_distance. */
  double min_difference; /**< The smallest value of best_herd_difference so
    far.  See the file drift.xml in the direct contact tests for an explanation
    of why this is needed. */
  double cumul_size;
} callback_t;



/**
 * Check whether herd 1 can infect herd 2 and if so, whether it beats the
 * best (so far) matching potential target herd.
 */
void
check_and_choose (callback_t * callback_data, HRD_herd_t * herd2)
{
  HRD_herd_t *herd1;
  local_data_t *local_data;
  gboolean herd2_can_be_target;
  double distance;
  double difference;
  ZON_zone_fragment_t *herd1_fragment, *herd2_fragment;
  gboolean contact_forbidden;
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER check_and_choose (%s)", MODEL_NAME);
#endif
  /*
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "ENTER check_and_choose %s", MODEL_NAME); 
    guilib_printf( guilog );
  }
  */
  
  /* Are herd 1 and herd 2 the same? */
  herd1 = callback_data->herd1;
  if (herd1 == herd2)
    goto end;

  local_data = (local_data_t *) (callback_data->self->model_data);

  /* Can herd 2 be the target of an exposure? */
#if DEBUG
  s = g_string_new (NULL);
  g_string_sprintf (s, "unit \"%s\" is %s, state is %s: ",
                    herd2->official_id, herd2->production_type_name,
                    HRD_status_name[herd2->status]);
#endif

#ifdef LARAMIE
  /* In the experimental version LARAMIE, destroyed units can still receive
   * indirect contacts. */
  if( callback_data->contact_type == IndirectContact )
    {
      herd2_can_be_target =
        herd2->production_type == callback_data->recipient_production_type;
    }
  else
    {
      herd2_can_be_target =
        herd2->production_type == callback_data->recipient_production_type
        && herd2->status != Destroyed;
    } 
#else
  herd2_can_be_target =
    herd2->production_type == callback_data->recipient_production_type
    && herd2->status != Destroyed;
#endif
    
#if DEBUG
  g_string_sprintfa (s, "%s be target", herd2_can_be_target ? "can" : "cannot");
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif
  if (!herd2_can_be_target)
    goto end;

  /* Is herd 2 closer to herd 1 than the best match so far? */
  distance = GIS_local_distance (herd1->lat, herd1->lon, herd2->lat, herd2->lon);
  difference = fabs (callback_data->movement_distance - distance);
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "distance to unit \"%s\" = %g km (within %g km)",
         herd2->official_id, distance, difference);
#endif

#ifdef CHEYENNE
  /* In the experimental version CHEYENNE, quarantined units can still receive
   * direct contacts. */
  contact_forbidden = FALSE;
#endif
  if (callback_data->best_herd == NULL)
    {
#ifndef CHEYENNE
      /* If herd 2 is quarantined, it cannot be the recipient of a direct
       * contact. */
      contact_forbidden = (herd2->quarantined && callback_data->contact_type == DirectContact);
#endif
      if (!contact_forbidden)
        {
          callback_data->best_herd = herd2;
          callback_data->best_herd_distance = distance;
          callback_data->best_herd_difference = difference;
          callback_data->min_difference = difference;
          callback_data->cumul_size = herd2->size;
        }
    }
  else if (fabs (difference - callback_data->min_difference) <= EPSILON)
    {
      /* When we find a second potential recipient unit the same distance from
       * the source as the current closest potential recipient, we first check
       * if contact with the new find is forbidden by quarantine or zone rules.
       * If so, we can ignore it.  Otherwise, the new find has a 1 in 2 chance
       * of replacing the older find, if they are the same size.  (If the new
       * find is half the size of the old one, it has a 1 in 3 chance; if it is
       * twice the size of the old one, it has a 2 in 3 chance.)  If we find a
       * third potential recipient unit the same distance from the source, its
       * chance of replacing the older choice is its size divided by the sum of
       * its size and the sizes of all of the older choices.  This gives the
       * same result as recording all the potential recipient herds and
       * choosing among them afterwards. */
      herd1_fragment = callback_data->herd1_fragment;
      herd2_fragment = callback_data->zones->membership[herd2->index];

#ifndef CHEYENNE
      contact_forbidden = ((herd2->quarantined && callback_data->contact_type == DirectContact)
                           || (ZON_level (herd2_fragment) > ZON_level (herd1_fragment))
                           || ((ZON_level (herd2_fragment) == ZON_level (herd1_fragment))
                               && !ZON_same_fragment (herd2_fragment, herd1_fragment)));
#else
  #error "How should Cheyenne handle zone rules?"
#endif
      if (!contact_forbidden)
        {
          callback_data->cumul_size += herd2->size;

          if (RAN_num (callback_data->rng) < herd2->size / callback_data->cumul_size)
            {
              callback_data->best_herd = herd2;
              callback_data->best_herd_distance = distance;
              callback_data->best_herd_difference = difference;
              if (difference < callback_data->min_difference)
                callback_data->min_difference = difference;
            }
        }
    }
  else if (difference < callback_data->min_difference)
    {
#ifndef CHEYENNE
      contact_forbidden = (herd2->quarantined && callback_data->contact_type == DirectContact);
#endif
      if (!contact_forbidden)
        {
          callback_data->best_herd = herd2;
          callback_data->best_herd_distance = distance;
          callback_data->best_herd_difference = difference;
          callback_data->min_difference = difference;
          callback_data->cumul_size = herd2->size;
        }
    }

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT check_and_choose (%s)", MODEL_NAME);
#endif
  /*
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "EXIT check_and_choose %s", MODEL_NAME); 
    guilib_printf( guilog );
  }
  */
  return;
}



int
callback (int id, void *arg)
{
  callback_t *callback_data;
  HRD_herd_list_t *herds;
  HRD_herd_t *herd2;

  callback_data = (callback_t *) arg;
  herds = callback_data->herds;
  /* Because herd indices start at 0, and R-Tree rectangle IDs start at 1. */
  herd2 = HRD_herd_list_get (herds, id - 1);
  check_and_choose (callback_data, herd2);

  /* A return value of 0 would mean that the spatial search should stop early
   * (before all herds in the search rectangle were visited).  We don't want
   * that, so return 1. */
  return 1;
}



/**
 * Responds to a new day event by releasing any pending contacts and
 * stochastically generating exposures and infections.
 *
 * @param self the model.
 * @param herds a list of herds.
 * @param zones the zone list.
 * @param event a new day event.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
handle_new_day_event (struct ergadm_model_t_ *self, HRD_herd_list_t * herds,
                      ZON_zone_list_t * zones, EVT_new_day_event_t * event,
                      RAN_gen_t * rng, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  EVT_event_t *pending_event;
  double disease_control_factors;
  double rate;
  PDF_dist_t *poisson;
  unsigned int nherds;          /* number of herds */
  EVT_contact_type_t contact_type;
  param_block_t **contact_type_block;
  HRD_herd_t *herd1, *herd2;
  unsigned int nprod_types, i, j;
  param_block_t *param_block;
  unsigned int herd1_index, herd2_index;
  unsigned int zone_index;
  REL_chart_t *control_chart;
  int nexposures;
  EVT_event_t *exposure, *infection;
  struct Rect search_rect;
  double mult;                  /* to account for latitude */
  callback_t callback_data;
  double distance;              /* between two herds being considered */
  double r, P;
  ZON_zone_fragment_t *background_zone, *herd1_fragment, *herd2_fragment;
  int shipping_delay;
  int delay_index;
  GQueue *q;
  HRD_expose_t exposure_update;
  char guilog[1024];

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "ENTER handle_new_day_event %s", MODEL_NAME); 
    guilib_printf( guilog );
  }

  local_data = (local_data_t *) (self->model_data);

  /* Release any pending (due to shipping delays) events. */
  local_data->rotating_index =
    (local_data->rotating_index + 1) % local_data->pending_infections->len;
  q = (GQueue *) g_ptr_array_index (local_data->pending_infections, local_data->rotating_index);
  while (!g_queue_is_empty (q))
    {
      /* Remove the event from this model's internal queue and place it in the
       * simulation's event queue. */
      pending_event = (EVT_event_t *) g_queue_pop_head (q);
      EVT_event_enqueue (queue, pending_event);
      local_data->npending_infections--;
    }

  /* Allocate a distribution that will be used to pick the number of exposures
   * from a source. */
  poisson = PDF_new_poisson_dist (1.0);

  callback_data.self = self;
  callback_data.herds = herds;
  callback_data.zones = zones;
  callback_data.rng = rng;

  background_zone = ZON_zone_list_get_background (zones);
  nprod_types = local_data->production_types->len;
  nherds = HRD_herd_list_length (herds);
  for (herd1_index = 0; herd1_index < nherds; herd1_index++)
    {
      herd1 = HRD_herd_list_get (herds, herd1_index);

#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "unit \"%s\" is %s, state is %s",
             herd1->official_id, herd1->production_type_name, HRD_status_name[herd1->status]);
#endif

      /* If the herd is not Latent, Infectious Subclinical, or Infectious
       * Clinical, it cannot generate exposures. */
      if (herd1->status < Latent || herd1->status > InfectiousClinical)
        continue;

      callback_data.herd1 = herd1;
      if (NULL != guilib_record_exposure)
        {
          exposure_update.src_index = herd1->index;
          exposure_update.src_status = herd1->status;
        }
      herd1_fragment = zones->membership[herd1->index];
      callback_data.herd1_fragment = herd1_fragment;

      /* Do direct contacts, then indirect contacts. */
      for (contact_type = DirectContact; contact_type <= IndirectContact; contact_type++)
        {
          contact_type_block = local_data->param_block[contact_type][herd1->production_type];
          if (contact_type_block == NULL)
            continue;

#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                 "generating %s exposures from unit \"%s\"",
                 EVT_contact_type_name[contact_type], herd1->official_id);
#endif

          /* If the herd is quarantined, it cannot be a source.  Recall that
           * quarantine affects only direct contacts. */
          if (herd1->quarantined)
            {
              if (contact_type == DirectContact)
                {
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                         "unit \"%s\" is quarantined, will not be source", herd1->official_id);
#endif
                  continue;
                }
              else
                {
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                         "quarantined unit \"%s\" will be a source of indirect contact",
                         herd1->official_id);
#endif
                  ;
                }
            }

          callback_data.contact_type = contact_type;
          if (NULL != guilib_record_exposure)
            {
              if (contact_type == DirectContact)
                exposure_update.exposure_method = "D";
              else
                exposure_update.exposure_method = "I";
            }

          /* Generate contacts for each recipient production type separately. */
          for (i = 0; i < nprod_types; i++)
            {
              param_block = contact_type_block[i];
              if (param_block == NULL)
                continue;

              /* Check whether the source herd can infect this recipient
               * production type. */
              if ((herd1->status == Latent && param_block->latent_units_can_infect == FALSE)
                  || (herd1->status == InfectiousSubclinical
                      && param_block->subclinical_units_can_infect == FALSE))
                continue;

#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                     "generating exposures to %s units",
                     (char *) g_ptr_array_index (local_data->production_types, i));
#endif
              callback_data.recipient_production_type = i;

              /* Compute a multiplier to reduce the rate of movement once the
               * community is aware of an outbreak. */
              if (!local_data->outbreak_known)
                disease_control_factors = REL_chart_lookup (0, param_block->movement_control);
              else if (ZON_same_zone (background_zone, herd1_fragment))
                disease_control_factors =
                  REL_chart_lookup (event->day - local_data->public_announcement_day,
                                    param_block->movement_control);
              else
                {
                  zone_index = ZON_level (herd1_fragment) - 1;
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "zone index = %u", zone_index);
                  control_chart =
                    local_data->movement_control[contact_type][zone_index][herd1->production_type];
                  if (control_chart == NULL)
                    {
#if DEBUG
                      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                             "unit is in \"%s\" zone, no special control chart for %s units in this zone",
                             herd1_fragment->parent->name, herd1->production_type_name);
#endif
                      control_chart = param_block->movement_control;
                    }
                  else
                    {
#if DEBUG
                      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                             "unit is in \"%s\" zone, using special control chart for %s units in this zone",
                             herd1_fragment->parent->name, herd1->production_type_name);
#endif
                      ;
                    }
                  disease_control_factors =
                    REL_chart_lookup (event->day - local_data->public_announcement_day,
                                      control_chart);
                }

              /* Pick the number of exposures from this source.
               * If a fixed number of movements is specified, use it.
               * Otherwise, pick a number from the Poisson distribution. */
              if (param_block->fixed_movement_rate > 0)
                {
                  rate = param_block->fixed_movement_rate * disease_control_factors;
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                         "contact rate = %g (fixed) * %g = %g",
                         param_block->fixed_movement_rate, disease_control_factors, rate);
#endif
                  nexposures = (int) ((event->day + 1) * rate) - (int) (event->day * rate);
                }
              else
                {
                  rate = param_block->movement_rate * disease_control_factors;
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                         "contact rate = %g * %g = %g",
                         param_block->movement_rate, disease_control_factors, rate);
#endif
                  poisson->u.poisson.mu = rate;
                  nexposures = (int) (PDF_random (poisson, rng));
                }
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "%i exposures from unit \"%s\"",
                     nexposures, herd1->official_id);
#endif

              /* For each exposure, pick a distance and use it to find a
               * recipient herd. */
              for (j = 0; j < nexposures; j++)
                {
                  callback_data.movement_distance = PDF_random (param_block->distance_dist, rng);
                  if (callback_data.movement_distance < 0)
                    callback_data.movement_distance = 0;

#if INFO
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "contact of %g km",
                         callback_data.movement_distance);
#endif

                  /* Find the potential recipient herd whose distance from the
                   * source herd is closest to the chosen distance.  If several
                   * potential recipients are the same distance from the source
                   * unit, choose among them randomly. */
                  callback_data.best_herd = NULL;
                  if (
#if defined(USE_RTREE) && USE_RTREE == 1
                       /* For debugging purposes, you can #define USE_RTREE to 0 to never
                        * use the spatial index, or 1 to always use it. */
                       TRUE ||
#endif
                       callback_data.movement_distance * 2 <= local_data->use_rtree_index)
                    {
                      distance = callback_data.movement_distance * 2 / GIS_DEGREE_DISTANCE;
                      mult = 1.0 / MIN (cos (DEG2RAD * (herd1->lat + distance)), cos(DEG2RAD * (herd1->lat - distance))); 
                      search_rect.boundary[0] = herd1->lon - (distance * mult);
                      search_rect.boundary[1] = herd1->lat - distance;
                      search_rect.boundary[2] = herd1->lon + (distance * mult);
                      search_rect.boundary[3] = herd1->lat + distance;
                      RTreeSearch (herds->spatial_index, &search_rect, callback, &callback_data);
                    }

                  /* If the R-tree search didn't find anything (or if we didn't
                   * use the R-tree), then do an exhaustive search through all
                   * herds. */
                  if (callback_data.best_herd == NULL || callback_data.best_herd_distance > (callback_data.movement_distance * 2))
                    for (herd2_index = 0; herd2_index < nherds; herd2_index++)
                      check_and_choose (&callback_data, HRD_herd_list_get (herds, herd2_index));

                  if (callback_data.best_herd == NULL)
                    {
#if INFO
                      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,
                             "no recipient can be found at ~%g km from unit \"%s\"",
                             callback_data.movement_distance, herd1->official_id);
#endif
                      continue;
                    }

                  /* An eligible recipient unit (correct production type, not
                   * Destroyed) was found. */
                  herd2 = callback_data.best_herd;
#if INFO
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,
                         "unit \"%s\" within %g km of %g",
                         herd2->official_id,
                         callback_data.best_herd_difference, callback_data.movement_distance);
#endif
                  /* Check whether contact with this unit is forbidden by the
                   * zone rules. */
                  herd2_fragment = zones->membership[herd2->index];
                  if (ZON_level (herd2_fragment) > ZON_level (herd1_fragment))
                    {
#if INFO
                      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                             "contact forbidden: contact from unit \"%s\" in \"%s\" zone, level %i to unit \"%s\" in \"%s\" zone, level %i would violate higher-to-lower rule",
                             herd1->official_id,
                             herd1_fragment->parent->name,
                             herd1_fragment->parent->level,
                             herd2->official_id,
                             herd2_fragment->parent->name, herd2_fragment->parent->level);
#endif
                      continue;
                    }
                  if ((ZON_level (herd2_fragment) == ZON_level (herd1_fragment))
                      && !ZON_same_fragment (herd2_fragment, herd1_fragment))
                    {
#if INFO
                      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                             "contact forbidden: contact from unit \"%s\" to unit \"%s\" in separate foci of \"%s\" zone would violate separate foci rule",
                             herd1->official_id, herd2->official_id, herd1_fragment->parent->name);
#endif
                      continue;
                    }

                  /* Announce the exposure. */
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                         "unit \"%s\" unit exposed", herd2->official_id);
#endif
                  exposure = EVT_new_exposure_event (herd1, herd2, event->day,
                                                     EVT_contact_type_name[contact_type], TRUE);
                  exposure->u.exposure.contact_type = contact_type;
                  shipping_delay = (int) round (PDF_random (param_block->shipping_delay, rng));
                  if (shipping_delay <= 0)
                    EVT_event_enqueue (queue, exposure);
                  else
                    {
                      exposure->u.exposure.day += shipping_delay;
                      if (shipping_delay > local_data->pending_infections->len)
                        ergadm_extend_rotating_array (local_data->pending_infections,
                                                      shipping_delay, local_data->rotating_index);
                      delay_index =
                        (local_data->rotating_index +
                         shipping_delay) % local_data->pending_infections->len;
                      q =
                        (GQueue *) g_ptr_array_index (local_data->pending_infections, delay_index);
                      g_queue_push_tail (q, exposure);
                      local_data->npending_infections++;
                    }

                  if (NULL != guilib_record_exposure)
                    {
                      exposure_update.dest_index = herd2->index;
                      exposure_update.dest_status = herd2->status;
                    }

                  /* Does the exposure result in infection? */
                  if (contact_type == DirectContact && herd1->prevalence_curve != NULL)
                    P = herd1->prevalence;
                  else
                    P = param_block->prob_infect;
                  r = RAN_num (rng);
                  if (r >= P)
                    {
#if INFO
                      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,
                             "r (%g) >= P (%g), unit \"%s\" not infected", r,
                             P, herd2->official_id);
#endif
                      if (NULL != guilib_record_exposure)
                        {
                          exposure_update.success = 0;
                          guilib_record_exposure (exposure_update);
                        }
                      continue;
                    }
#if INFO
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,
                         "r (%g) < P (%g), unit \"%s\" infected", r, P,
                         herd2->official_id);
#endif
                  infection = EVT_new_attempt_to_infect_event (herd1, herd2,
                                                               event->day,
                                                               EVT_contact_type_name[contact_type]);
                  if (shipping_delay <= 0)
                    EVT_event_enqueue (queue, infection);
                  else
                    {
                      infection->u.infection.day += shipping_delay;
                      /* The queue to add the delayed infection to was already
                       * found above. */
                      g_queue_push_tail (q, infection);
                      local_data->npending_infections++;
                    }

                  if (NULL != guilib_record_exposure)
                    {
                      exposure_update.success = -1;
                      guilib_record_exposure (exposure_update);
                    }

                }               /* end of loop over exposures */

            }                   /* end of loop over recipient production types */

        }                       /* end of loop over contact types */

    }                           /* end of loop over source herds */

  PDF_free_dist (poisson);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "EXIT handle_new_day_event %s", MODEL_NAME); 
    guilib_printf( guilog );
  }
}



/**
 * Records the day on which the outbreak is publically announced.  This is
 * important because the movement rate decreases with community awareness of an
 * outbreak.
 *
 * @param self the model.
 * @param event a public announcement event.
 */
void
handle_public_announcement_event (struct
                                  ergadm_model_t_ *self, EVT_public_announcement_event_t * event)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_public_announcement_event (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "ENTER handle_public_announcement_event %s", MODEL_NAME); 
    guilib_printf( guilog );
  }

  local_data = (local_data_t *) (self->model_data);
  if (local_data->outbreak_known == FALSE)
    {
      local_data->outbreak_known = TRUE;
      local_data->public_announcement_day = event->day;
#if INFO
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,
             "community is now aware of outbreak, movement will slow");
#endif
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_public_announcement_event (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "EXIT handle_public_announcement_event %s", MODEL_NAME); 
    guilib_printf( guilog );
  }
}



/**
 * Runs this model.
 *
 * Side effects: may change the state of one or more herds in list.
 *
 * @param self the model.
 * @param herds a list of herds.
 * @param zones a list of zones.
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
    guilib_printf( guilog );
  }

  switch (event->type)
    {
    case EVT_RequestForExposureCauses:
      handle_request_for_exposure_causes_event (self, queue);
      break;
    case EVT_RequestForInfectionCauses:
      handle_request_for_infection_causes_event (self, queue);
      break;
    case EVT_NewDay:
      handle_new_day_event (self, herds, zones, &(event->u.new_day), rng, queue);
      break;
    case EVT_PublicAnnouncement:
      handle_public_announcement_event (self, &(event->u.public_announcement));
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
    guilib_printf( guilog );
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
  unsigned int i;
  GQueue *q;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "ENTER reset %s", MODEL_NAME); 
    guilib_printf( guilog );
  }

  local_data = (local_data_t *) (self->model_data);
  local_data->outbreak_known = FALSE;
  local_data->public_announcement_day = 0;
  for (i = 0; i < local_data->pending_infections->len; i++)
    {
      q = (GQueue *) g_ptr_array_index (local_data->pending_infections, i);
      while (!g_queue_is_empty (q))
        EVT_free_event (g_queue_pop_head (q));
    }
  local_data->npending_infections = 0;
  local_data->rotating_index = 0;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT reset (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "EXIT reset %s", MODEL_NAME); 
    guilib_printf( guilog );
  }
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
  local_data_t *local_data;

  local_data = (local_data_t *) (self->model_data);
  return (local_data->npending_infections > 0);
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
  local_data_t *local_data;

  local_data = (local_data_t *) (self->model_data);
  return (local_data->npending_infections > 0);
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
  local_data_t *local_data;
  EVT_contact_type_t contact_type;
  param_block_t ***contact_type_block;
  REL_chart_t ***contact_type_chart;
  unsigned int nprod_types, nzones, i, j;
  param_block_t *param_block;
  REL_chart_t *chart;
  char *substring, *chararray;

  local_data = (local_data_t *) (self->model_data);
  s = g_string_new (NULL);
  g_string_append_printf (s, "<%s", MODEL_NAME);

  /* Add the parameter block for each to-from combination of production
   * types. */
  nprod_types = local_data->production_types->len;
  for (contact_type = DirectContact; contact_type <= IndirectContact; contact_type++)
    {
      contact_type_block = local_data->param_block[contact_type];
      for (i = 0; i < nprod_types; i++)
        if (contact_type_block[i] != NULL)
          for (j = 0; j < nprod_types; j++)
            if (contact_type_block[i][j] != NULL)
              {
                param_block = contact_type_block[i][j];
                g_string_append_printf (s, "\n  for %s -> %s (%s)",
                                        (char *) g_ptr_array_index (local_data->production_types,
                                                                    i),
                                        (char *) g_ptr_array_index (local_data->production_types,
                                                                    j),
                                        EVT_contact_type_name[contact_type]);
                if (param_block->fixed_movement_rate > 0)
                  g_string_append_printf (s, "\n    fixed-movement-rate=%g",
                                          param_block->fixed_movement_rate);
                else
                  g_string_append_printf (s, "\n    movement-rate=%g", param_block->movement_rate);

                substring = PDF_dist_to_string (param_block->distance_dist);
                g_string_append_printf (s, "\n    distance=%s", substring);
                free (substring);

                substring = PDF_dist_to_string (param_block->shipping_delay);
                g_string_append_printf (s, "\n    delay=%s", substring);
                free (substring);

                g_string_append_printf (s, "\n    prob-infect=%g", param_block->prob_infect);

                substring = REL_chart_to_string (param_block->movement_control);
                g_string_append_printf (s, "\n    movement-control=%s", substring);
                free (substring);

                g_string_append_printf (s, "\n    latent-units-can-infect=%s",
                                        param_block->latent_units_can_infect ? "true" : "false");
                g_string_append_printf (s, "\n    subclinical-units-can-infect=%s",
                                        param_block->
                                        subclinical_units_can_infect ? "true" : "false");
              }
    }

  /* Add the movement control chart for each production type-zone
   * combination. */
  nzones = ZON_zone_list_length (local_data->zones);
  for (contact_type = DirectContact; contact_type <= IndirectContact; contact_type++)
    {
      contact_type_chart = local_data->movement_control[contact_type];
      for (i = 0; i < nzones; i++)
        for (j = 0; j < nprod_types; j++)
          if (contact_type_chart[i][j] != NULL)
            {
              chart = contact_type_chart[i][j];
              g_string_append_printf (s, "\n  for %s in \"%s\" zone (%s)",
                                      (char *) g_ptr_array_index (local_data->production_types,
                                                                  j),
                                      ZON_zone_list_get (local_data->zones, i)->name,
                                      EVT_contact_type_name[contact_type]);
              substring = REL_chart_to_string (chart);
              g_string_append_printf (s, "\n    movement-control=%s", substring);
              free (substring);
            }
    }

  g_string_append_c (s, '>');

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

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER fprintf (%s)", MODEL_NAME);
#endif
  s = to_string (self);
  nchars_written = fprintf (stream, "%s", s);
  free (s);
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT fprintf (%s)", MODEL_NAME);
#endif
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
  int nchars_written;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER printf (%s)", MODEL_NAME);
#endif
  nchars_written = local_fprintf (stdout, self);
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT printf (%s)", MODEL_NAME);
#endif
  return nchars_written;
}



/**
 * Frees this model.  Does not free the production type names.
 *
 * @param self the model.
 */
void
local_free (struct ergadm_model_t_ *self)
{
  local_data_t *local_data;
  unsigned int nprod_types, nzones, i, j;
  EVT_contact_type_t contact_type;
  param_block_t ***contact_type_block;
  param_block_t *param_block;
  REL_chart_t ***contact_type_chart;
  GQueue *q;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER free (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Free each of the parameter blocks. */
  nprod_types = local_data->production_types->len;
  for (contact_type = DirectContact; contact_type <= IndirectContact; contact_type++)
    {
      contact_type_block = local_data->param_block[contact_type];
      for (i = 0; i < nprod_types; i++)
        if (contact_type_block[i] != NULL)
          {
            for (j = 0; j < nprod_types; j++)
              if (contact_type_block[i][j] != NULL)
                {
                  param_block = contact_type_block[i][j];
                  /* Free the dynamically-allocated parts of the parameter block. */
                  PDF_free_dist (param_block->distance_dist);
                  PDF_free_dist (param_block->shipping_delay);
                  REL_free_chart (param_block->movement_control);
                  /* Free the parameter block itself. */
                  g_free (param_block);
                }
            /* Free this row of the 2D array. */
            g_free (contact_type_block[i]);
          }
      /* Free the array of pointers to rows. */
      g_free (contact_type_block);
    }

  /* Free the movement control charts for units inside zones. */
  nzones = ZON_zone_list_length (local_data->zones);
  for (contact_type = DirectContact; contact_type <= IndirectContact; contact_type++)
    {
      contact_type_chart = local_data->movement_control[contact_type];
      for (i = 0; i < nzones; i++)
        {
          for (j = 0; j < nprod_types; j++)
            REL_free_chart (contact_type_chart[i][j]);

          /* Free this row of the 2D array. */
          g_free (contact_type_chart[i]);

        }
      /* Free the array of pointers to rows. */
      g_free (contact_type_chart);
    }

  /* Free any pending infections. */
  for (i = 0; i < local_data->pending_infections->len; i++)
    {
      q = (GQueue *) g_ptr_array_index (local_data->pending_infections, i);
      while (!g_queue_is_empty (q))
        EVT_free_event (g_queue_pop_head (q));
      g_queue_free (q);
    }
  g_ptr_array_free (local_data->pending_infections, TRUE);

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
 * Returns whether this model is a singleton or not.
 */
gboolean
is_singleton (void)
{
  return TRUE;
}



/**
 * Adds a set of parameters to a contact spread model.
 */
void
set_params (struct ergadm_model_t_ *self, scew_element * params)
{
  local_data_t *local_data;
  param_block_t t;
  scew_element const *e;
  gboolean success;
  scew_attribute *attr;
  XML_Char const *attr_text;
  EVT_contact_type_t contact_type;
  gboolean *from_production_type, *to_production_type;
  gboolean *zone;
  unsigned int nprod_types, nzones, i, j;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER set_params (%s)", MODEL_NAME);
#endif

  /* Make sure the right XML subtree was sent. */
  g_assert (strcmp (scew_element_name (params), MODEL_NAME) == 0);

  local_data = (local_data_t *) (self->model_data);

  /* Find out whether these parameters are for direct or indirect contact. */
  attr = scew_attribute_by_name (params, "contact-type");
  g_assert (attr != NULL);
  attr_text = scew_attribute_value (attr);
  if (strcmp (attr_text, "direct") == 0)
    contact_type = DirectContact;
  else if (strcmp (attr_text, "indirect") == 0)
    contact_type = IndirectContact;
  else
    g_assert_not_reached ();

  /* Read the parameters and store them in a temporary param_block_t
   * structure. */

  e = scew_element_by_name (params, "movement-rate");
  t.fixed_movement_rate = -1.0;
  t.movement_rate = 0;
  if (e != NULL)
    {
      /* There is a mean movement rate, so don't use the fixed rate */
      /* -1 seems like a reasonable default value for fixed_movement_rate. */
      /* If fixed_movement_rate >= 0, use it instead of the Poisson distribution */
      t.movement_rate = PAR_get_frequency (e, &success);
      if (success == FALSE)
        {
          g_warning ("setting movement rate to 0");
          t.movement_rate = 0;
        }
    }
  else
    {
      /* If there is no mean movement rate specified, look for a fixed movement rate */
      e = scew_element_by_name (params, "fixed-movement-rate");
      if (e != NULL)
        {
          t.fixed_movement_rate = PAR_get_frequency (e, &success);
          if (success == FALSE)
            {
              g_warning ("setting movement rate to 0");
              t.fixed_movement_rate = -1.0;
            }
        }
    }

  /* The movement rate cannot be negative. */
  if (t.movement_rate < 0)
    {
      g_warning ("movement rate cannot be negative, setting to 0");
      t.movement_rate = 0;
    }

  e = scew_element_by_name (params, "distance");
  if (e != NULL)
    {
      t.distance_dist = PAR_get_PDF (e);
      /* No part of the distance distribution can be negative. */
      if (!t.distance_dist->has_inf_lower_tail)
        {
          g_assert (PDF_cdf (-EPSILON, t.distance_dist) == 0);
        }
    }
  else
    {
      t.distance_dist = NULL;
    }

  e = scew_element_by_name (params, "delay");
  if (e != NULL)
    {
      t.shipping_delay = PAR_get_PDF (e);
    }
  else
    {
      t.shipping_delay = PDF_new_point_dist (0);
    }

  e = scew_element_by_name (params, "prob-infect");
  if (e != NULL)
    {
      t.prob_infect = PAR_get_probability (e, &success);
      if (success == FALSE)
        {
          g_warning ("setting probability of infection to 0");
          t.prob_infect = 0;
        }
    }
  else
    {
      t.prob_infect = 0;
    }

  e = scew_element_by_name (params, "movement-control");
  if (e != NULL)
    {
      t.movement_control = PAR_get_relationship_chart (e);
    }
  else
    {
      t.movement_control = REL_new_point_chart (1);
    }
  /* The movement rate multiplier cannot go negative. */
  g_assert (REL_chart_min (t.movement_control) >= 0);

  e = scew_element_by_name (params, "latent-units-can-infect");
  if (contact_type == DirectContact && e != NULL)
    {
      t.latent_units_can_infect = PAR_get_boolean (e, &success);
      if (success == FALSE)
        {
          g_warning ("latent units will be able to infect");
          t.latent_units_can_infect = TRUE;
        }
    }
  else
    t.latent_units_can_infect = (contact_type == DirectContact);

  e = scew_element_by_name (params, "subclinical-units-can-infect");
  if (e != NULL)
    {
      t.subclinical_units_can_infect = PAR_get_boolean (e, &success);
      if (success == FALSE)
        {
          g_warning ("subclinical units will be able to infect");
          t.subclinical_units_can_infect = TRUE;
        }
    }
  else
    t.subclinical_units_can_infect = TRUE;

  /* Find out which to-from production type combinations, or which production
   * type-zone combinations, these parameters apply to. */
  from_production_type =
    ergadm_read_prodtype_attribute (params, "from-production-type", local_data->production_types);
  to_production_type =
    ergadm_read_prodtype_attribute (params, "to-production-type", local_data->production_types);
  if (scew_attribute_by_name (params, "zone") != NULL)
    zone = ergadm_read_zone_attribute (params, local_data->zones);
  else
    zone = NULL;

  /* Copy the parameters to the appropriate place. */
  nprod_types = local_data->production_types->len;
  nzones = ZON_zone_list_length (local_data->zones);
  if (zone == NULL)
    {
      /* These parameters are by to-from production type. */

      param_block_t ***contact_type_block;
      param_block_t *param_block;

      contact_type_block = local_data->param_block[contact_type];
      for (i = 0; i < nprod_types; i++)
        {
          if (from_production_type[i] == FALSE)
            continue;

          /* If necessary, create a row in the 2D array for this from-
           * production type. */
          if (contact_type_block[i] == NULL)
            contact_type_block[i] = g_new0 (param_block_t *, nprod_types);

          for (j = 0; j < nprod_types; j++)
            {
              if (to_production_type[j] == FALSE)
                continue;

              /* Create a parameter block for this to-from production type
               * combination, or overwrite the existing one. */
              param_block = contact_type_block[i][j];
              if (param_block == NULL)
                {
                  param_block = g_new (param_block_t, 1);
                  contact_type_block[i][j] = param_block;
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                         "setting parameters for %s -> %s (%s)",
                         (char *) g_ptr_array_index (local_data->production_types, i),
                         (char *) g_ptr_array_index (local_data->production_types, j),
                         EVT_contact_type_name[contact_type]);
#endif
                }
              else
                {
                  g_warning ("overwriting previous parameters for %s -> %s (%s)",
                             (char *) g_ptr_array_index (local_data->production_types, i),
                             (char *) g_ptr_array_index (local_data->production_types, j),
                             EVT_contact_type_name[contact_type]);
                }

              param_block->movement_rate = t.movement_rate;
              param_block->fixed_movement_rate = t.fixed_movement_rate;
              param_block->movement_control = REL_clone_chart (t.movement_control);
              param_block->distance_dist = PDF_clone_dist (t.distance_dist);
              param_block->shipping_delay = PDF_clone_dist (t.shipping_delay);
              param_block->latent_units_can_infect = t.latent_units_can_infect;
              param_block->subclinical_units_can_infect = t.subclinical_units_can_infect;
              param_block->prob_infect = t.prob_infect;
            }
        }
    }
  else
    {
      /* These parameters are by production type-zone. */

      REL_chart_t ***contact_type_chart;

#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "here");
#endif

      contact_type_chart = local_data->movement_control[contact_type];
      for (i = 0; i < nzones; i++)
        {
          if (zone[i] == FALSE)
            continue;

          for (j = 0; j < nprod_types; j++)
            {
              if (from_production_type[j] == FALSE)
                continue;

              /* Create a relationship chart for this to-from production type
               * combination, or overwrite the existing one. */
              if (contact_type_chart[i][j] == NULL)
                {
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                         "setting movement control for %s in \"%s\" zone (%s)",
                         (char *) g_ptr_array_index (local_data->production_types, j),
                         ZON_zone_list_get (local_data->zones, i)->name,
                         EVT_contact_type_name[contact_type]);
#endif
                  ;
                }
              else
                {
                  REL_free_chart (contact_type_chart[i][j]);
                  g_warning ("overwriting previous movement control for %s in \"%s\" zone (%s)",
                             (char *) g_ptr_array_index (local_data->production_types, j),
                             ZON_zone_list_get (local_data->zones, i)->name,
                             EVT_contact_type_name[contact_type]);
                }
              contact_type_chart[i][j] = REL_clone_chart (t.movement_control);
            }
        }
    }

  g_free (from_production_type);
  g_free (to_production_type);
  if (zone != NULL)
    g_free (zone);
  PDF_free_dist (t.distance_dist);
  PDF_free_dist (t.shipping_delay);
  REL_free_chart (t.movement_control);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT set_params (%s)", MODEL_NAME);
#endif

  return;
}



/**
 * Returns a new contact spread model.
 */
ergadm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  ergadm_model_t *m;
  local_data_t *local_data;
  unsigned int nprod_types, nzones, i;
  char guilog[1024];

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
  m->outputs = g_ptr_array_new ();
  m->model_data = local_data;
  m->set_params = set_params;
  m->run = run;
  m->reset = reset;
  m->is_listening_for = is_listening_for;
  m->has_pending_actions = has_pending_actions;
  m->has_pending_infections = has_pending_infections;
  m->to_string = to_string;
  m->printf = local_printf;
  m->fprintf = local_fprintf;
  m->free = local_free;

  /* local_data->param_block holds two 2D arrays of parameter blocks, where
   * each block holds the parameters for one to-from combination of production
   * types.  The first level of indexing is by contact type, then by source
   * production type (rows), then by recipient production type (columns).
   * Initially, all row pointers are NULL.  Rows will be created as needed in
   * the set_params function. */
  local_data->production_types = herds->production_type_names;
  nprod_types = local_data->production_types->len;
  local_data->param_block[UnknownContact] = NULL;
  local_data->param_block[DirectContact] = g_new0 (param_block_t **, nprod_types);
  local_data->param_block[IndirectContact] = g_new0 (param_block_t **, nprod_types);

  /* local_data->movement_control holds movement control charts by zone and
   * production type.  The first level of indexing is by contact type, then by
   * zone (rows), then by source production type (columns).  Initially, the
   * rows contain all NULL pointers. */
  local_data->zones = zones;
  nzones = ZON_zone_list_length (zones);
  local_data->movement_control[UnknownContact] = NULL;
  local_data->movement_control[DirectContact] = g_new0 (REL_chart_t **, nzones);
  for (i = 0; i < nzones; i++)
    local_data->movement_control[DirectContact][i] = g_new0 (REL_chart_t *, nprod_types);
  local_data->movement_control[IndirectContact] = g_new0 (REL_chart_t **, nzones);
  for (i = 0; i < nzones; i++)
    local_data->movement_control[IndirectContact][i] = g_new0 (REL_chart_t *, nprod_types);

  /* No outbreak has been announced yet. */
  local_data->outbreak_known = FALSE;
  local_data->public_announcement_day = 0;

  /* Initialize an array for delayed contacts.  We don't know yet how long the
   * the array needs to be, since that will depend on values we sample from the
   * delay distribution, so we initialize it to length 1. */
  local_data->pending_infections = g_ptr_array_new ();
  g_ptr_array_add (local_data->pending_infections, g_queue_new ());
  local_data->npending_infections = 0;
  local_data->rotating_index = 0;

  /* Use the following heuristic to decide whether to use the R-tree index in
   * searches: if the ratio of the diameter of circle of maximum spread to the
   * short axis of the oriented bounding rectangle around the herds is <= 0.25,
   * use the R-tree. */
  if (herds->short_axis_length > 0)
    local_data->use_rtree_index = 0.25 / 2 * herds->short_axis_length;
  else
    local_data->use_rtree_index = 0;
#if defined(USE_RTREE) && USE_RTREE == 0
  /* For debugging purposes, you can #define USE_RTREE to 0 to never use the
   * spatial index, or 1 to always use it. */
  local_data->use_rtree_index = 0;
#endif

  /* Send the XML subtree to the init function to read the production type
   * combination specific parameters. */
  m->set_params (m, params);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}

char *
contact_spread_model_interface_version (void)
{
  return interface_version ();
}

ergadm_model_t *
contact_spread_model_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

gboolean
contact_spread_model_is_singleton (void)
{
  return is_singleton ();
}

/* end of file contact-spread-model.c */
