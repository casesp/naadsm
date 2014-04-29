/** @file model_loader.c
 * Functions for discovering and dynamically loading sub-models.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.93
 * @date March 2003
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

#include "model_loader.h"
#include "gis.h"
#include "reporting.h"

#if STDC_HEADERS
#  include <string.h>
#elif HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_UNISTD_H
#  include <unistd.h>
#endif

#if HAVE_ERRNO_H
#  include <errno.h>
#endif

#ifdef NO_MODEL_LIBS
#  include "airborne-spread-model.h"
#  include "airborne-spread-exponential-model.h"
#  include "basic-destruction-model.h"
#  include "basic-zone-focus-model.h"
#  include "conflict-resolver.h"
#  include "contact-spread-model.h"
#  include "destruction-monitor.h"
#  include "detection-model.h"
#  include "detection-monitor.h"
#  include "detection-on-given-day-model.h"
#  include "disease-model.h"
#  include "economic-model.h"
#  include "exposure-monitor.h"
#  include "infection-monitor.h"
#  include "quarantine-model.h"
#  include "resources-and-implementation-of-controls-model.h"
#  include "ring-destruction-model.h"
#  include "ring-vaccination-model.h"
#  include "trace-back-destruction-model.h"
#  include "trace-back-zone-focus-model.h"
#  include "vaccination-monitor.h"
#  include "vaccine-model.h"
#  include "zone-model.h"
#  include "zone-monitor.h"
#endif

#include "guilib.h"

#define DEFAULT_MODEL_DIR "models"

#define MODEL_INTERFACE_VERSION "0.93"


extern const char *RPT_frequency_name[];

#ifdef NO_MODEL_LIBS
struct model_load_info_t
{
  const char *model_name;
  ergadm_model_interface_version_t model_version_fn;
  ergadm_model_new_t model_instantiation_fn;
  ergadm_model_is_singleton_t model_singleton_fn;
};


struct model_load_info_t model_list[] = {
  {"airborne-spread-model", &airborne_spread_model_interface_version,
    (void*)&airborne_spread_model_new, airborne_spread_model_is_singleton},
  {"airborne-spread-exponential-model", &airborne_spread_exponential_model_interface_version,
    (void*)&airborne_spread_exponential_model_new, airborne_spread_exponential_model_is_singleton},
  {"basic-destruction-model", &basic_destruction_model_interface_version,
    (void*)&basic_destruction_model_new, NULL},
  {"basic-zone-focus-model", &basic_zone_focus_model_interface_version,
    (void*)&basic_zone_focus_model_new, NULL},     
  {"conflict-resolver", &conflict_resolver_interface_version, 
    (void*)&conflict_resolver_new, NULL},
  {"contact-spread-model", &contact_spread_model_interface_version, 
    (void*)&contact_spread_model_new, contact_spread_model_is_singleton},
  {"destruction-monitor", &destruction_monitor_interface_version, 
    (void*)&destruction_monitor_new, NULL},
  {"detection-model", &detection_model_interface_version, 
    (void*)&detection_model_new, detection_model_is_singleton},
  {"detection-monitor", &detection_monitor_interface_version, 
    (void*)&detection_monitor_new, NULL},
  {"detection-on-given-day-model", &detection_on_given_day_model_interface_version,
    (void*)&detection_on_given_day_model_new, NULL},
  {"disease-model", &disease_model_interface_version, 
    (void*)&disease_model_new, NULL},
  {"economic-model", &economic_model_interface_version, 
    (void*)&economic_model_new, NULL},
  {"exposure-monitor", &exposure_monitor_interface_version, 
    (void*)&exposure_monitor_new, NULL},
  {"infection-monitor", &infection_monitor_interface_version, 
    (void*)&infection_monitor_new, NULL},
  {"quarantine-model", &quarantine_model_interface_version, 
    (void*)&quarantine_model_new, NULL},
  {"resources-and-implementation-of-controls-model", &resources_and_implementation_of_controls_model_interface_version,
    (void*)&resources_and_implementation_of_controls_model_new, NULL},
  {"ring-destruction-model", &ring_destruction_model_interface_version, 
    (void*)&ring_destruction_model_new, NULL},
  {"ring-vaccination-model", &ring_vaccination_model_interface_version, 
    (void*)&ring_vaccination_model_new, NULL},
  {"trace-back-destruction-model", &trace_back_destruction_model_interface_version,
   &trace_back_destruction_model_new, NULL},
  {"trace-back-zone-focus-model", &trace_back_zone_focus_model_interface_version,
    (void*)&trace_back_zone_focus_model_new, NULL},
  {"vaccination-monitor", &vaccination_monitor_interface_version, 
    (void*)&vaccination_monitor_new, NULL},
  {"vaccine-model", &vaccine_model_interface_version, 
    (void*)&vaccine_model_new, NULL},
  {"zone-model", &zone_model_interface_version, 
    (void*)&zone_model_new, NULL},
  {"zone-monitor", &zone_monitor_interface_version, 
    (void*)&zone_monitor_new, NULL}    
};

int model_list_count = (sizeof (model_list) / sizeof (struct model_load_info_t));

int
model_name_cmp (const struct model_load_info_t *c1, const struct model_load_info_t *c2)
{
  return strcmp (c1->model_name, c2->model_name);
}


struct model_load_info_t *
find_model (const char *name)
{
  struct model_load_info_t target;
  target.model_name = name;

  return bsearch (&target, model_list, model_list_count, sizeof (struct model_load_info_t), model_name_cmp      /*AR  "warning: passing arg 5 of `bsearch' from incompatible pointer type" */
    );
}
#endif /* NO_MODEL_LIBS */



/**
 * Extracts the number of days the simulation is to last.
 *
 * @param e a "num-days" element from the simulation parameters.
 * @return the number of days.
 */
unsigned int
get_num_days (scew_element * e)
{
  long int tmp;

  tmp = strtol (scew_element_contents (e), NULL, 10);   /* base 10 */
  g_assert (errno != ERANGE && errno != EINVAL);
  return (unsigned int) tmp;
}



/**
 * Extracts the number of Monte Carlo runs for the simulation.
 *
 * @param e a "num-runs" element from the simulation parameters.
 * @return the number of runs.
 */
unsigned int
get_num_runs (scew_element * e)
{
  long int tmp;

  tmp = strtol (scew_element_contents (e), NULL, 10);   /* base 10 */
  g_assert (errno != ERANGE && errno != EINVAL);
  return (unsigned int) tmp;
}



/**
 * Instantiates a set of models based on information in a parameter file.
 *
 * @param parameter_file name of the parameter file.
 * @param herds a list of herds.
 * @param zones a list of zones.  This can be empty at first, as it may be
 *   populated while reading the parameters.
 * @param model_dir absolute or relative path to the model object files.  If
 *   NULL, DEFAULT_MODEL_DIR will be used.
 * @param ndays a location in which to store the number of days the simulation
 *   lasts.
 * @param nruns a location in which to store the number of Monte Carlo runs of
 *   the simulation.
 * @param models a location in which to store the address of the array of
 *   pointers to models.
 * @param outputs a list of output variables to report.  Their names should
 *   correspond to output elements in the parameter file.
 * @return the number of models loaded.
 */
int
ergadm_load_models (char *parameter_file,
                    HRD_herd_list_t * herds, ZON_zone_list_t * zones,
                    char *model_dir, unsigned int *ndays, unsigned int *nruns,
                    ergadm_model_t *** models, GPtrArray * outputs)
{
  scew_parser *parser;          /* to read the parameter file */
  scew_error err;               /* parser error code */
  scew_element *params;         /* root of the parameter tree */
  scew_element *e;              /* a subtree of the parameter tree */
  scew_element **ee;            /* a list of subtrees */
  scew_element *model_spec;     /* a subtree for a model */
  const char *model_name;       /* name of a model */
  const char *model_version;    /* interface version of a model */

#ifdef NO_MODEL_LIBS
  struct model_load_info_t *model_load_info;
#else
  lt_dlhandle handle;
  int dl_error;                 /* error code from dynamic loader */
  const char *dl_error_text;    /* error description from dynamic loader */
#endif

  ergadm_model_interface_version_t model_version_fn;
  ergadm_model_is_singleton_t model_is_singleton_fn;
  gboolean singleton;
  GHashTable *singletons;       /* stores the "singleton" modules (for which
                                   there can be only one instance).  Keys are
                                   model names (char *) and data are pointers
                                   to models. */
  ergadm_model_new_t model_instantiation_fn;
  ergadm_model_t *model;
  int nmodels;
  int nloaded = 0;
  int i, j;                     /* loop counters */
  unsigned int noutputs = 0;
  RPT_reporting_t *output;
  const XML_Char *variable_name;
  unsigned int nzones;
  char guilog[1024];
#if DEBUG
  char *s;
#endif

  if (NULL != guilib_debug)
    guilib_debug ("----- ENTER ergadm_load_models");

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER ergadm_load_models");
#endif

  parser = scew_parser_create ();
  if (parser == NULL)
    return 0;

  if (NULL != guilib_debug)
    {
      sprintf (guilog, "Loading SCEW parser with param file %s.", parameter_file);
      guilib_debug (guilog);
    }

  /* This test isn't foolproof because the file could be deleted in the split-
   * second before scew_parser_load_file tries to open it. */
  if (g_file_test (parameter_file, G_FILE_TEST_EXISTS) == FALSE)
    g_error ("parameter file \"%s\" not found", parameter_file);

  if (scew_parser_load_file (parser, parameter_file) != 1)
    {
      err = scew_error_code ();
      if (err == scew_error_expat)
        g_error ("parameter file \"%s\" could not be parsed: %s on line %i", parameter_file,
                 scew_error_expat_string (scew_error_expat_code (parser)),
                 scew_error_expat_line (parser));
      else
        g_error ("parameter file \"%s\" could not be parsed: %s", parameter_file,
                 scew_error_string (err));
    }

  if (NULL != guilib_debug)
    guilib_debug ("SCEW parser loaded.");

  params = scew_tree_root (scew_parser_tree (parser));

  g_assert (params != NULL);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "root has %u children", scew_element_count (params));
  for (i = 0; i < scew_element_count (params); i++)
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "child %i name=\"%s\"", i,
           scew_element_name (scew_element_by_index (params, i)));
#endif

  g_assert (scew_element_by_name (params, "num-days") != NULL);
  *ndays = get_num_days (scew_element_by_name (params, "num-days"));
  g_assert (scew_element_by_name (params, "num-runs") != NULL);
  *nruns = get_num_runs (scew_element_by_name (params, "num-runs"));

  /* Set the directory from which sub-models will be loaded. */
  if (model_dir == NULL)
    model_dir = DEFAULT_MODEL_DIR;

#if INFO
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "models in directory \"%s\"", model_dir);
#endif

#ifndef NO_MODEL_LIBS
  LTDL_SET_PRELOADED_SYMBOLS ();
  dl_error = lt_dlinit ();
  if (dl_error)
    g_error ("%s", lt_dlerror ());
  dl_error = lt_dlsetsearchpath (model_dir);
  if (dl_error)
    g_error ("%s", lt_dlerror ());
#endif

  /* Get the number of sub-models that will run in the simulation. */
  e = scew_element_by_name (params, "models");
  nmodels = (int) scew_element_count (e);

#if INFO
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "%i sub-models in parameters", nmodels);
#endif

  singletons = g_hash_table_new (g_str_hash, g_str_equal);

  /* Load and instantiate each model. */
#ifdef NO_MODEL_LIBS
  /*AR Just in case the order of model_list should get screwed up by a
   *  forgetful programmer (not that I know anyone like that)...  */
  qsort (model_list, model_list_count, sizeof (struct model_load_info_t), model_name_cmp);
  if (NULL != guilib_debug)
    guilib_debug ("Model list sorted.");
#endif
  *models = g_new (ergadm_model_t *, nmodels);
  for (i = 0; i < nmodels; i++)
    {
      model_spec = scew_element_by_index (e, (unsigned int) i);
      model_name = scew_element_name (model_spec);
#if INFO
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "loading model %i, \"%s\"", i + 1, model_name);
#endif

#ifdef NO_MODEL_LIBS
      /* Find the model in the array */
      model_load_info = find_model (model_name);

      sprintf (guilog, "Loading model %s\n", model_name);
      if (NULL != guilib_debug)
        {
          guilib_debug (guilog);
        }

      if (NULL == model_load_info)
        {
          g_warning ("Model %s not found in model list.", model_name);
          continue;
        }
#  if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "Model = <%p>", model_load_info);
#  endif
#else
      /* Use the dynamic loader */
      handle = lt_dlopenext (model_name);
      if (handle == NULL)
        {
          g_warning ("could not load %s because %s", model_name, lt_dlerror ());
          continue;
        }
#  if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "handle = <%p>", handle);
#  endif
#endif

      /* Check that the module conforms to the expected model interface
       * version. */
      model_version_fn = NULL;
#ifdef NO_MODEL_LIBS
      /* Use the model version function from the appropriate array element */
      if (NULL != guilib_debug)
        guilib_debug ("Loading model version function.");
      model_version_fn = model_load_info->model_version_fn;
#else
      /* Find the model's "interface_version" function using the dynamic loader */
      model_version_fn = (ergadm_model_interface_version_t) lt_dlsym (handle, "interface_version");
      if (model_version_fn == NULL)
        {
          g_warning ("could not find the interface version: not loading model %s", model_name);
          lt_dlclose (handle);
          continue;
        }
#endif
      model_version = model_version_fn ();
      if (strcmp (MODEL_INTERFACE_VERSION, model_version) != 0)
        {
          g_warning
            ("the interface version (%s) is different than expected (%s): not loading model %s",
             model_version, MODEL_INTERFACE_VERSION, model_name);
#ifndef NO_MODEL_LIBS
          lt_dlclose (handle);
#endif
          continue;
        }

      /* If this is a "singleton" module (only one instance of it can exist),
       * check whether there is already an instance.  If so, pass the
       * parameters to the existing instance.  If not, create a new instance. */
#ifdef NO_MODEL_LIBS
      model_is_singleton_fn = model_load_info->model_singleton_fn;
#else
      model_is_singleton_fn = (ergadm_model_is_singleton_t) lt_dlsym (handle, "is_singleton");
#endif

      singleton = (model_is_singleton_fn != NULL && model_is_singleton_fn () == TRUE);


#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "module %s a singleton", singleton ? "is" : "is not");
#endif
      model = NULL;
      if (singleton)
        model = (ergadm_model_t *) g_hash_table_lookup (singletons, model_name);

      if (model != NULL)
        {
          /* Send the additional parameters to the already-instantiated
           * model. */
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "adding parameters to existing instance");
#endif
          model->set_params (model, model_spec);
        }
      else
        {
          /* Get the module's "new" function (to instantiate a model object). */
#ifdef NO_MODEL_LIBS
          /* Use the model's "new" function from the appropriate array element */
          if (NULL != guilib_debug)
            guilib_debug ("Setting the model's 'new' function");
          model_instantiation_fn = model_load_info->model_instantiation_fn;
#else
          /* Find the model's "new" function using the dynamic loader  */
          model_instantiation_fn = NULL;
          model_instantiation_fn = (ergadm_model_new_t) lt_dlsym (handle, "new");
          /* The Autotools book says that comparing the returned function pointer
           * to NULL is a bad test and recommends comparing the string returned by
           * lt_dlerror to NULL instead.  But I get the string "unknown error"
           * from lt_dlerror when the symbol lookup *works*.  ?? */
          if (model_instantiation_fn == NULL)
            {
              g_warning ("could not instantiate model because %s", lt_dlerror ());
              lt_dlclose (handle);
              continue;
            }
#endif

#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "\"new\" function = <%p>",
                 model_instantiation_fn);
#endif

          if (NULL != guilib_debug)
            guilib_debug ("Calling the model's 'new' function");
          model = model_instantiation_fn (model_spec, herds, zones);

          if (NULL != guilib_debug)
            guilib_debug ("'New' function has returned");

          /* Add the model's output variables to the list of reporting variables. */
          for (j = 0; j < model->outputs->len; j++)
            g_ptr_array_add (outputs, g_ptr_array_index (model->outputs, j));

#ifdef NO_MODEL_LIBS
          /*AR Do nothing */
#else
          /* Store a copy of the loaded module's handle in the object: we will need
           * to dlclose it later. */
          model->handle = handle;
#endif

          (*models)[nloaded++] = model;

          if (singleton)
            g_hash_table_insert (singletons, model_name, model);

        }                       /* end of case where a new model instance is created */

#if DEBUG
      s = model->to_string (model);
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, s);
      free (s);
#endif
    }                           /* end of loop over models */

  /* We can free the hash table structure without freeing the keys (because the
   * keys are model names, which are static strings) or the values (because the
   * values are model instances, which persist after this function ends). */
  g_hash_table_destroy (singletons);

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
      for (j = 0; j < outputs->len; j++)
        {
          output = (RPT_reporting_t *) g_ptr_array_index (outputs, j);
          if (strcmp (output->name, variable_name) == 0)
            break;
        }
      if (j == outputs->len)
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

  /* Make sure the zones' surveillance level numbers start at 1 and are
   * consecutive, because we're going to use them as list indices later. */
  nzones = ZON_zone_list_length (zones);
  for (i = 0; i < nzones; i++)
    ZON_zone_list_get (zones, i)->level = i + 1;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "final number of zones = %u", nzones);
  s = ZON_zone_list_to_string (zones);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "final zone list =\n%s", s);
  g_free (s);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "zone operations %s use R-tree",
         zones->use_rtree_index ? "will" : "will not");
#endif

  /* Clean up. */
  scew_parser_free (parser);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT ergadm_load_models");
#endif

  return nloaded;
}



/**
 * Frees all memory and resources used by a set of models.
 *
 * @param nmodels the number of models.
 * @param models an array of models.
 */
void
ergadm_unload_models (int nmodels, ergadm_model_t ** models)
{
#ifdef NO_MODEL_LIBS
  /*AR Do nothing */
#else
  ergadm_model_t *model;
  int i;                        /* loop counter */
  void *handle;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER ergadm_unload_models");
#endif

  /* Free each model and close its associated dynamically-loaded module. */
  for (i = 0; i < nmodels; i++)
    {
      model = models[i];
      handle = model->handle;
      model->free (model);
      lt_dlclose (handle);
    }
  lt_dlexit ();

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT ergadm_unload_models");
#endif
#endif
}

/* end of file model_loader.c */
