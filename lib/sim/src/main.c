/** @file src/main.c
 * A simulator for animal disease outbreaks.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @author Aaron Reeves <Aaron.Reeves@colostate.edu><br>
 *   Animal Population Health Institute<br>
 *   Colorado State University<br>
 *   Fort Collins, CO 80523<br>
 *   USA
 * @author Shaun Case <ShaunCase@colostate.edu><br>
 *   Animal Population Health Institute<br>
 *   College of Veterinary Medicine and Biomedical Sciences<br>
 *   Colorado State University<br>
 *   Fort Collins, CO 80523<br>
 *   USA
 *
 * Copyright &copy; University of Guelph, and Colorado Statue University 2003-2010
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * @todo If herd size can change during a simulation, we will have to store the
 *   original sizes.
 * @todo Create something to auto-generate all the boilerplate code for a new
 *   sub-model.
 */

/** @mainpage
 * A simulator for animal disease outbreaks.  For background information,
 * please see the <a href="http://naadsm.org/">NAADSM web site</a>.
 *
 * Notes to maintainer:
 *
 * The best way to get an overview of this software is to read the summary
 * documentation for the following files:
 * <ul>
 *   <li>
 *     herd.h - Herds of animals are the basis of the simulation.  This file
 *     explains what information defines a herd and how herds may be modified.
 *   <li>
 *     model.h - Sub-models simulate natural processes or human actions that
 *     affect herds.  The simulator is modular: a "simulation" is simply the
 *     sum of the actions of whatever sub-models are running.  This file
 *     explains the interface that all sub-models must conform to.
 *   <li>
 *     event.h and event_manager.h - Explain the mechanism by which sub-models
 *     communicate.
 *   <li>
 *     zone.h - Zones are areas established around diseased herds.  They can
 *     affect the behaviour of models.  This file explains how zones work.
 * </ul>
 *
 * A few general notes on the implementation:
 * <ol>
 *   <li>
 *     The MPI implementation (for now) simply divides the desired number of
 *     (independent) Monte Carlo runs across the available processors.
 *   <li>
 *     The various <a href="annotated.html">data structures</a> should be
 *     treated as opaque and manipulated \em only through the functions
 *     defined in their header files.
 *     The functions will usually include
 *     <ul>
 *       <li>
 *         \a new, which returns a newly-allocated, initialized object, or NULL
 *         if there was not enough memory to create one.
 *       <li>
 *         \a free, which frees all memory used by an object.  If the function
 *         \em does \em not free all dynamically-allocated parts of the object,
 *         that is noted in the documentation.
 *       <li>
 *         \a to_string, \a printf, and \a fprintf, which exist to aid
 *         debugging.  The print functions \em always build a complete string in
 *         memory, then print it out, to prevent interleaving of output in a
 *         parallel environment.
 *     </ul>
 *   <li>
 *     Some objects (e.g.,
 *     <a href="prob__dist_8h.html">probability distributions</a>,
 *     <a href="event_8h.html">events</a>)
 *     are encapsulated in a supertype,
 *     implemented as a structure containing a type field and a union.
 *   <li>
 *     Basic data structures and message logging facilities from
 *     <a href="http://developer.gnome.org/doc/API/2.0/glib/">GLib</a>
 *     and special functions from the
 *     <a href="http://sources.redhat.com/gsl/ref/gsl-ref_toc.html">GNU Scientific Library</a>
 *     are used extensively.
 *
 *     The message logging facilities define severity levels (message, info,
 *     warning, error).  These are used to provide a "verbosity" level that the
 *     user can set, where
 *     <ul>
 *       <li>
 *         verbosity = 0 means just print the output of the simulation.
 *       <li>
 *         verbosity = 1 means also print detailed information about how
 *         calculations and decisions are being performed.
 *     </ul>
 *
 *     The verbosity is implemented by defining a "silent" log handler that
 *     simply ignores messages.  This means that when running with low
 *     verbosity, the program is wasting time building strings that are never
 *     displayed.  To eliminate this waste, specify "--disable-debug" when
 *     running the configure script.
 *   <li>
 *     Since the Spanish-language version of NAADSM became available, it is
 *     assumed that text in the input XML may contain accented characters.
 *     The text is converted to ISO-8859-1 inside the simulator, because the
 *     output filters cannot handle multi-byte character encodings.
 *   <li>
 *     Comments are marked up for the auto-documentation tool
 *     <a href="http://www.doxygen.org/">Doxygen</a>, whose lovely output you
 *     are reading right now.
 *   <li>
 *     The testing harness is written with
 *     <a href="http://www.gnu.org/software/dejagnu/">DejaGnu</a>.
 *     DejaGnu is written in <a href="http://expect.nist.gov/">Expect</a>,
 *     which in turn uses <a href="http://www.tcl.tk/">Tcl</a>,
 *     so individual tests are written in Tcl.
 * </ol>
 */

/** @page complexity Complexity
 * In computer science parlance, an algorithm's <i>complexity</i> is the answer
 * to the question, "If I double the size of the problem, does the time
 * required to solve it double?  Quadruple?  Go up by an order of magnitude?"
 *
 * We talk of algorithms having, for example, "<i>n</i><sup>2</sup> running
 * time."  <i>n</i><sup>2</sup> means that the worst-case running time
 * increases as the square of the problem size, e.g., twice as large a problem
 * will take four times as long to solve.
 *
 * Intuitively, we know that our model will be <i>n</i><sup>2</sup>, because
 * the spread models are based on interactions between pairs of units, and the
 * number of pairs of units is a function of the square of the number of units.
 *
 * Figure 1 confirms this.  It plots the relative running time of scenarios
 * between 500 and 50000 units.  In each case, the units were randomly
 * distributed in a circle with a density of 0.36 units/km<sup>2</sup> (the
 * radius of the circles ranged from 21 km to 210 km).  The mean unit size was
 * 33 animals.
 *
 * @image html complexity.png "Figure 1. Complexity of the simulation"
 *
 * Note that the scenario with uncontrolled fast spread shows the worst-case
 * behaviour, because of the large number of units that are infectious
 * simultaneously (Figure 2).
 *
 * @image html uncontrolled_example.png "Figure 2. Example simulation with uncontrolled fast spread."
 *
 * So we may conclude that, in the worst case, doubling the number of units
 * will quadruple the running time.  The true running time will also depend on
 * other factors, such as the density of the units and the distance over which
 * spread models can operate.
 *
 * It is worthwhile to ask what we gain by using a supercomputer.  Ideally,
 * using <i>N</i> processors would yield an <i>N</i> x speedup.  However,
 * factors such as communication among the processors and repetition of
 * "startup" work (e.g., reading in the locations of units) usually make the
 * true speedup lower than the ideal.
 *
 * Figure 3 shows this effect.  The speedup is farthest from the ideal when the
 * number of Monte Carlo trials run per processor drops to trivial values.
 *
 * @image html speedup.png "Figure 3. Speedup from using a supercomputer."
 *
 * It is clear that a strategy for handling larger simulations is needed.
 * Using a supercomputer is, by itself, <em>not</em> an adequate strategy: as
 * we saw, the best speedup one can expect by adding more computers is
 * <em>linear</em>, but the growth of computing time needed as the problems get
 * bigger is <em>squared</em>.
 *
 * I suggest that we use R-trees to cope with large simulations.  R-trees are
 * commonly used in geographical databases.  Like an index in a book that lets
 * you quickly locate a particular topic, an R-tree is a spatial index that
 * lets you quickly find objects in a particular rectangular area.  Recall that
 * the simulation is <i>n</i><sup>2</sup> because all pairs of units can
 * potentially interact.  If we can <i>quickly</i> locate the subset of units
 * that are close enough for a unit to interact with, we can potentially reduce
 * the number of interactions in medium- and large-scale simulations.
 *
 * We can measure the usefulness of a spatial index by running the following
 * test with and without one:
 * <ol>
 *   <li>Choose a unit at random.
 *   <li>Find all other units of the same production type within <i>d</i> km.
 *   <li>Repeat many times.
 * </ol>
 * Figure 4 shows the results.  The speedup is tremendous when <i>d</i> is
 * small compared to the entire study area, but when <i>d</i> is larger, using
 * the index can hurt performance.  In the figure, the notion of "size compared
 * to the entire study area" is quantified as the ratio of the diameter of the
 * search area to the short side of a minimum-area rectangle drawn around the
 * units (figure 5).
 *
 * @image html rtree_benefit.png "Figure 4. Speedup from using a spatial index."
 *
 * @image html ontario_tiled.png "Figure 5. An example of a minimum-area rectangle around a set of units."
 *
 * This result means that the program should take a "hybrid" approach, using an
 * R-tree index whenever a spatial search over a small area is needed and a
 * na&iuml;ve search through all units when a search over a large area is
 * needed.
 *
 * The uncontrolled fast spread scenarios mentioned earlier experience a
 * considerable speedup with this approach: 4&times; faster with 5000 units,
 * 7&times; with 10000 units, and 22&times; with 50000 units.
 *
 * <b>How narrow a search?</b>
 *
 * There are 4 modules that work with spatial relationships in the model:
 * contact spread, airborne spread, ring vaccination, and ring destruction.
 * The last three work easily with an R-tree search because they have a
 * definite boundary past which they cannot have an effect: the distance at
 * which the probability of airborne spread drops to zero, or the radius of the
 * vaccination ring or the destruction ring.
 *
 * Contact spread lacks a definite boundary.  The model chooses a contact
 * distance <i>d</i>, and the contact goes to the unit whose distance from the
 * source is closest to <i>d</i>.  However, the closest potential recipient can
 * be arbitrarily far away.  So we use the R-tree to quickly search for a
 * recipient close to the source first, and if we do not find one, we fall back
 * to na&iuml;ve search.
 *
 * Figure 6 illustrates another concern when using a limited-area search for
 * contact spread.  If we search only out to distance <i>d</i>, we can miss the
 * correct recipient.  We avoid this problem by searching out to distance
 * 2<i>d</i>.  Figure 7 shows why this works.  View the scenario as a number
 * line where A is at position 0.  We know that <i>m</i> &lt; <i>d</i> (if
 * <i>d</i> &lt;= <i>m</i>, unit B is the correct choice for recipient and we
 * don't have a problem).  Because B is at the same location as A or to the
 * right of A, it follows that <i>x</i> &lt; <i>d</i>.  Given that <i>m</i>
 * &lt; <i>d</i> and <i>x</i> &lt; <i>d</i>, we can conclude that C = <i>m</i>
 * + <i>x</i> &lt; 2<i>d</i>.
 *
 * @image html contact_problem.png "Figure 6. Finding the recipient unit (B or C) that is closest to the distance d from unit A.  The dashed line is midway between B and C.  If we search only out to distance d, we will choose B as the recipient.  We must search further to find the correct recipient, C."
 *
 * @image html contact_proof.png "Figure 7. Searching out to distance 2d.  m is midway between B and C; x is the distance from B to m and from m to C.  See text for a proof that C will always be to the left of 2d."
 */

/** @page licenses Licenses of libraries and components
 * The table below lists terms and conditions on supporting libraries used by
 * this software.
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>GLib</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>generic data structures such as text and lists</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>The GTK+ Team (Peter Matthis et al.)</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       GNU Library General Public License (GNU LGPL)
 *       [<a href="http://www.gnu.org/copyleft/lgpl.html">read text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://www.gtk.org/">http://www.gtk.org/</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>GNU Scientific Library (GSL)</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>probability calculations</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>M. Galassi et al.</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       GNU General Public License (GNU GPL)
 *       [<a href="http://www.gnu.org/copyleft/gpl.html">read text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://www.gnu.org/software/gsl/">http://www.gnu.org/software/gsl/</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>Scalable Parallel Random Number Generators (SPRNG)</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>high-quality random numbers</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>Michael Mascagni et al.</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       source code distributed freely from website; no license terms apparent
 *       on website or in software
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://sprng.cs.fsu.edu/">http://sprng.cs.fsu.edu/</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>GNU Multiple Precision Arithmetic Library (GMP)</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>needed by SPRNG</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>Torbjorn Granlund et al.</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       GNU Library General Public License (GNU LGPL)
 *       [<a href="http://www.gnu.org/copyleft/lgpl.html">read text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://www.swox.com/gmp/">http://www.swox.com/gmp/</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>Expat</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>reading XML files</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>James Clark</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       "Permission is hereby granted, free of charge, to ... deal in the
 *       Software without restriction, including without limitation the rights
 *       to use, copy, modify, merge, publish, distribute, sublicense, and/or
 *       sell copies of the Software"
 *       [<a href="expat-COPYING.txt">read full text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://expat.sourceforge.net/">http://expat.sourceforge.net/</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>Simple C Expat Wrapper (SCEW)</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>reading XML files</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>Aleix Conchillo Flaque</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       GNU Lesser General Public License (GNU LGPL)
 *       [<a href="http://www.gnu.org/copyleft/lgpl.html">read text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://www.nongnu.org/scew/">http://www.nongnu.org/scew/</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>R-tree library</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>spatial indexing</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>Antonin Guttman and Daniel Green</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       "This code is placed in the public domain."
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://www.superliminal.com/sources/sources.htm">http://www.superliminal.com/sources/sources.htm</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>Wild Magic Library</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>geometry calculations and zones</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>David H. Eberly</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       "The Software may be used, edited, modified, copied, and distributed
 *       by you for commercial products provided that such products are not
 *       intended to wrap The Software solely for the purposes of selling it as
 *       if it were your own product.  The intent of this clause is that you
 *       use The Software, in part or in whole, to assist you in building your
 *       own original products."
 *       [<a href="http://www.magic-software.com/License/WildMagic.pdf">read full text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://www.magic-software.com/SourceCode.html">http://www.magic-software.com/SourceCode.html</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>planar convex hull code</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>automatic tiling</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>Ken Clarkson</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       "Permission to use, copy, modify, and distribute this software for any
 *       purpose without fee is hereby granted"
 *       [<a href="2dch.txt">read full text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://cm.bell-labs.com/who/clarkson/">http://cm.bell-labs.com/who/clarkson/</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>Shapefile C Library</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>writing ArcView files</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>Frank Warmerdam</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       Library GNU Public License (LGPL)
 *       [<a href="http://www.gnu.org/copyleft/lesser.html">read text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://shapelib.maptools.org/">http://shapelib.maptools.org/</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>GD graphics library</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>quick rendering of herd maps</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>Thomas Boutell et al.</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       "Permission has been granted to copy, distribute and modify gd in any
 *       context without fee, including a commercial application"
 *       [<a href="http://www.boutell.com/gd/manual2.0.33.html#notice">read full text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://www.boutell.com/gd/">http://www.boutell.com/gd/</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>General Polygon Clipper (gpc)</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>geometry calculations</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>Alan Murta</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>"free for non-commercial use"</td>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://www.cs.man.ac.uk/aig/staff/alan/software/">http://www.cs.man.ac.uk/aig/staff/alan/software/</a>
 *     </td>
 *   </tr>
 * </table>
 *
 * <table>
 *   <tr>
 *     <td><b>Component:</b></td> <td>PROJ.4 Cartographic Projections library</td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>converting lat/long locations into x-y locations</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>Gerald Evenden et al.</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       "Permission is hereby granted, free of charge, to any person obtaining
 *       a copy of this software and associated documentation files ... to deal
 *       in the Software without restriction, including without limitation the
 *       rights to use, copy, modify, merge, publish, distribute, sublicense,
 *       and/or sell copies of the Software"
 *       [<a href="http://trac.osgeo.org/proj/wiki/WikiStart#License">read full text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://trac.osgeo.org/proj/">http://trac.osgeo.org/proj/</a>
 *     </td>
 *   </tr>
 * </table>
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif
/* #include <sys/times.h> */
#include <time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include "herd.h"
#include "model_loader.h"
#include "event_manager.h"
#include "reporting.h"
#include "rng.h"

#include "herd-randomizer.h"
#if HAVE_MPI && !CANCEL_MPI
#  include "mpix.h"
#endif

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#if HAVE_CTYPE_H
#  include <ctype.h>
#endif

#if HAVE_ERRNO_H
#  include <errno.h>
#endif

/*
main.c needs access to the functions defined in naadsm.h,
even when compiled as a *nix executable (in which case,
the functions defined will all be NULL).
*/
#include "naadsm.h"

#include "general.h"

#define G_LOG_ALL_LEVELS (G_LOG_LEVEL_CRITICAL | \
                    G_LOG_LEVEL_DEBUG | \
                    G_LOG_LEVEL_ERROR | \
                    G_LOG_LEVEL_INFO | \
                    G_LOG_LEVEL_WARNING )
#define	G_LOG_ALERT_LEVELS (G_LOG_LEVEL_CRITICAL | \
                      G_LOG_LEVEL_ERROR | \
                      G_LOG_LEVEL_WARNING)

extern const char *RPT_frequency_name[];

extern gboolean use_fixed_poisson;
extern double poisson_fix;



/**
 * A log handler that simply discards messages.  "Info" and "debug" level
 * messages are directed to this at low verbosity levels.
 */
void
silent_log_handler (const gchar * log_domain, GLogLevelFlags log_level,
                    const gchar * message, gpointer user_data)
{
  ;
}



/**
 * Pull apart the program's version number, available in the constant
 * PACKAGE_VERSION, into major, minor, and release numbers.
 */
void
split_version (const char *version, RPT_reporting_t *split)
{
  gchar **tokens;
  gchar **iter;
  guint i;
  long num;
  
  tokens = g_strsplit (version, ".", 3);
  for (iter = tokens, i = 0; *iter != NULL; iter++, i++)
    {
      num = strtol (*iter, NULL, 10);
      if (i == 0)
        RPT_reporting_set_integer1 (split, num, "major");
      else if (i == 1)
        RPT_reporting_set_integer1 (split, num, "minor");
      else
        RPT_reporting_set_integer1 (split, num, "release");
    }
  g_strfreev (tokens);

  return;	
}

 

/**
 * Create a default map projection to use if no preferred one is supplied.
 *
 * Side effects: after this function runs, the herd list will have a bounding
 * box defined.  The box will be an unoriented rectangle.
 *
 * @image html albers.gif "Albers equal area conic projection.  Public domain image from USGS."
 *
 * @param herds the herd list.
 * @return a newly-allocated projPJ object.  projPJ is actually a pointer data
 *   type, but the fact that it is a pointer is "hidden" by a typedef.
 */
projPJ
default_projection (HRD_herd_list_t * herds)
{
  unsigned int nherds, i;
  HRD_herd_t *herd;
  double min_lat, min_lon, max_lat, max_lon;
  double lat_range, center_lon, sp1, sp2;
  projPJ projection;
  char *projection_args;
#if DEBUG
  char *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER default_projection");
#endif

  /* Get the lat and lon limits, which we will use to set up the projection. */
  nherds = HRD_herd_list_length (herds);
  if (nherds == 0)
    min_lat = max_lat = min_lon = max_lon = 0;
  else
    {
      /* Initialize with the position of the first herd. */
      herd = HRD_herd_list_get (herds, 0);
      min_lat = max_lat = herd->latitude;
      min_lon = max_lon = herd->longitude;

      for (i = 1; i < nherds; i++)
        {
          herd = HRD_herd_list_get (herds, i);
          if (herd->latitude < min_lat)
            min_lat = herd->latitude;
          else if (herd->latitude > max_lat)
            max_lat = herd->latitude;

          if (herd->longitude < min_lon)
            min_lon = herd->longitude;
          else if (herd->longitude > max_lon)
            max_lon = herd->longitude;
        }
    }
  center_lon = (min_lon + max_lon) / 2.0;

  /* If the latitude range is very close to the equator or contains the
   * equator, use a cylindrical equal area projection.  (The Albers equal area
   * conic projection becomes the cylindrical equal area when its parallels are
   * at the equator.) */
  if ((min_lat > -1 && max_lat < 1) || (min_lat * max_lat < 0))
    {
#if DEBUG
      g_debug ("study area near equator, using cylindrical equal area projection");
#endif
      projection_args = g_strdup_printf ("+ellps=WGS84 +units=km +lon_0=%g +lat_0=%g +proj=cea", center_lon, min_lat);
      projection = pj_init_plus (projection_args);
    }
  else
    {
#if DEBUG
      g_debug ("using Albers equal area conic projection");
#endif
      lat_range = max_lat - min_lat;
      sp1 = min_lat + lat_range / 6.0;
      sp2 = max_lat - lat_range / 6.0;
      projection_args =
        g_strdup_printf ("+ellps=WGS84 +units=km +lon_0=%g +proj=aea +lat_0=%g +lat_1=%g +lat_2=%g",
                         center_lon, min_lat, sp1, sp2);
      projection = pj_init_plus (projection_args);
      if (!projection)
        {
          g_error ("could not create map projection object: %s", pj_strerrno(pj_errno));
        }
    }
  g_free (projection_args);
#if DEBUG
  s = pj_get_def (projection, 0);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "projection = %s", s);
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT default_projection");
#endif

  return projection;
}



/* WARNING: If the parameters to this function change, then a change is
 * also required in the Delphi user interface! */
DLL_API void
run_sim_main (char *herd_file,
              char *parameter_file,
              double fixed_rng_value, int verbosity, int seed)
{
  unsigned int ndays, nruns, day, run;
  gboolean randomize_herds;
  int initial_state_numbers[HRD_NSTATES];
  RPT_reporting_t *last_day_of_outbreak;
  RPT_reporting_t *clock_time;
  RPT_reporting_t *version;
  GPtrArray *reporting_vars;
  int nmodels = 0;
  naadsm_model_t **models = NULL;
  naadsm_event_manager_t *manager;
  unsigned int nherds;
  HRD_herd_list_t *herds;
  HRD_herd_t *herd;
  RAN_gen_t *rng;
  unsigned int nzones;
  ZON_zone_list_t *zones;
  ZON_zone_t *zone;
  int i;                     /* loop counter */
  gboolean active_infections_yesterday, active_infections_today,
    pending_actions, disease_end_recorded,
    stop_on_disease_end, early_exit;
  time_t start_time;
  guint exit_conditions = 0;
  double m_total_time, total_processor_time;
  unsigned long total_runs;
  char *summary;
  m_total_time = total_processor_time = 0.0;

  for( i = 0; i < HRD_NSTATES; ++i )
    initial_state_numbers[i] = 0;

  /* This line prints a Byte Order Mark (BOM) that indicates that the output is
   * in UTF-8.  Not currently used. */
  /*
  if (NULL == naadsm_printf)
    g_print ("%s", "\xEF\xBB\xBF");
  */

  /* Set the verbosity level. */
  if (verbosity < 1)
    {
      g_log_set_handler (NULL, G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("herd", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("prob_dist", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("rel_chart", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("reporting", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("zone", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("gis", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
    }

  #ifdef WIN_DLL
    #if DEBUG
      /* During debugging, the items listed here can be changed, 
       * depending on which units are being debugged. */
      g_log_set_handler (NULL, G_LOG_ALL_LEVELS, naadsm_log_handler, NULL);
      g_log_set_handler ("herd", G_LOG_ALERT_LEVELS, silent_log_handler, NULL);
    #else
      /* For routine use, only the more serious alerts should be sent to the GUI. */
      g_log_set_handler (NULL, G_LOG_ALERT_LEVELS, naadsm_log_handler, NULL);
      g_log_set_handler ("herd", G_LOG_ALERT_LEVELS, naadsm_log_handler, NULL);
      g_log_set_handler ("prob_dist", G_LOG_ALERT_LEVELS, naadsm_log_handler, NULL);
      g_log_set_handler ("rel_chart", G_LOG_ALERT_LEVELS, naadsm_log_handler, NULL);
      g_log_set_handler ("reporting", G_LOG_ALERT_LEVELS, naadsm_log_handler, NULL);
      g_log_set_handler ("zone", G_LOG_ALERT_LEVELS, naadsm_log_handler, NULL);
      g_log_set_handler ("gis", G_LOG_ALERT_LEVELS, naadsm_log_handler, NULL);
    #endif
  #endif
  
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "verbosity = %i", verbosity);
#endif

  /* Get the list of herds. */
  if (herd_file)
    {
      herds = HRD_load_herd_list ( herd_file );
      nherds = HRD_herd_list_length (herds);
    }
  else
    {
      herds = NULL;
      nherds = 0;
    }

#if DEBUG
  g_debug ("%i units read", nherds);
#endif
  if (nherds == 0)
    {
      g_error ("no units in file %s", herd_file);
    }

  /* Project the herd locations onto a flat map, if they aren't already. */
  if (herds->projection == NULL)
    {
      herds->projection = default_projection (herds);
      HRD_herd_list_project (herds, herds->projection);
    }
  /* Build a spatial index around the herd locations. */
  herds->spatial_index = new_spatial_search ();
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      spatial_search_add_point (herds->spatial_index, herd->x, herd->y);
    }
  spatial_search_prepare (herds->spatial_index);

  /* Initialize the reporting variables, and bundle them together so they can
   * easily be sent to a function for initialization. */
  last_day_of_outbreak =
    RPT_new_reporting ("outbreakDuration", RPT_integer, RPT_never);
  clock_time = RPT_new_reporting ("clock-time", RPT_real, RPT_never);
  version = RPT_new_reporting ("version", RPT_group, RPT_never);
  split_version (PACKAGE_VERSION, version);
  reporting_vars = g_ptr_array_new ();
  g_ptr_array_add (reporting_vars, last_day_of_outbreak);
  g_ptr_array_add (reporting_vars, clock_time);
  g_ptr_array_add (reporting_vars, version);

  /* Pre-create a "background" zone. */
  zones = ZON_new_zone_list (nherds);
  zone = ZON_new_zone ("", -1, 0.0);

  ZON_zone_list_append (zones, zone);

  /* Get the simulation parameters and sub-models. */
  nmodels =
    naadsm_load_models (parameter_file, herds, herds->projection, zones,
                        &ndays, &nruns, &randomize_herds, initial_state_numbers,
                        &models, reporting_vars, &exit_conditions );
  nzones = ZON_zone_list_length (zones);

  /* The clock time reporting variable is special -- it can only be reported
   * once (at the end of each simulation) or never. */
  if (clock_time->frequency != RPT_never && clock_time->frequency != RPT_once)
    {
      g_warning ("clock-time cannot be reported %s; it will reported at the end of each simulation",
                 RPT_frequency_name[clock_time->frequency]);
      RPT_reporting_set_frequency (clock_time, RPT_once);
    }

#if HAVE_MPI && !CANCEL_MPI
  /* Increase the number of runs to divide evenly by the number of processors,
   * if necessary. */
  if (nruns % me.np != 0)
    nruns += (me.np - nruns % me.np);
  nruns /= me.np;               /* because it's parallel, wheee! */
  _scenario.nruns = nruns;
#endif

#if DEBUG
  g_debug ("simulation %u days x %u runs", ndays, nruns);
#endif

  /* Initialize the pseudo-random number generator. */
  rng = RAN_new_generator (seed);

  if (fixed_rng_value >= 0 && fixed_rng_value < 1)
    {
      RAN_fix (rng, fixed_rng_value);
    }

  manager = naadsm_new_event_manager (models, nmodels);

  /* Determine whether each iteration should end when the active disease phase ends. */
  stop_on_disease_end = (0 != get_stop_on_disease_end( exit_conditions ) );

  m_total_time = total_processor_time = 0.0;
  total_runs = 0;

  if (NULL != naadsm_sim_start)
    naadsm_sim_start ();

  naadsm_create_event (manager, EVT_new_declaration_of_outputs_event(reporting_vars), herds, zones, rng);

  /* Begin the loop over the specified number of iterations. */
  naadsm_create_event (manager, EVT_new_before_any_simulations_event(), herds, zones, rng);
  for (run = 0; run < nruns; run++)
    {

#if defined( USE_MPI ) && !CANCEL_MPI
      double m_start_time, m_end_time;
      m_start_time = m_end_time = 0.0;
      m_start_time = MPI_Wtime();
#endif

    _iteration.zoneFociCreated = FALSE;
    _iteration.diseaseEndDay = -1;
    _iteration.outbreakEndDay = -1;
    _iteration.first_detection = FALSE;

      /* Does the GUI user want to stop a simulation in progress? */
      if (NULL != naadsm_simulation_stop)
        {
          if (0 != naadsm_simulation_stop ())
            break;
        }

#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "resetting everything before start of simulation");
#endif

      if (NULL != naadsm_iteration_start)
        naadsm_iteration_start (run);

      if ( _iteration.infectious_herds != NULL )
      {
        g_hash_table_destroy( _iteration.infectious_herds );
        _iteration.infectious_herds = NULL;
      };
      _iteration.infectious_herds = g_hash_table_new( g_direct_hash, g_direct_equal );

      /* Reset reporting variables. */
      RPT_reporting_set_null (last_day_of_outbreak, NULL);

      /* Reset all models. */
      for (i = 0; i < nmodels; i++)
        models[i]->reset (models[i]);

      /* Reset all zones. */
      ZON_zone_list_reset (zones);

      /* Reset initial disease states, if necessary. */
      if( randomize_herds )
        randomize_initial_states( herds, HRD_NSTATES, initial_state_numbers, rng );
        
      /* Reset some iteration variables. */
      active_infections_yesterday = TRUE;
      pending_actions = TRUE;
      disease_end_recorded = FALSE;
      early_exit = FALSE;

      naadsm_create_event (manager, EVT_new_before_each_simulation_event(), herds, zones, rng);

      /* Run the iteration. */
      start_time = time (NULL);

      /* Begin the loop over the days in an iteration. */
      for (day = 1; (day <= ndays) && (!early_exit); day++)
        {
#if defined( USE_MPI ) && !CANCEL_MPI
          double m_day_start_time = MPI_Wtime();
          double m_day_end_time = m_day_start_time;
#endif
          /* Does the GUI user want to stop a simulation in progress? */
          if (NULL != naadsm_simulation_stop)
            {
              /* This check may break the day loop.
               * If necessary, Another check (see above) will break the iteration loop.*/
              if (0 != naadsm_simulation_stop ())
                break;
            }

          _iteration.current_day = day;

          if (NULL != naadsm_day_start)
            naadsm_day_start (day);

#if DEBUG && defined( USE_MPI )
          double m_start_day_time = MPI_Wtime();
#endif
          /* Process changes made to the herds and zones on the previous day. */
          naadsm_create_event (manager, EVT_new_midnight_event (day), herds, zones, rng);

          active_infections_today = FALSE;
          for (i = 0; i < nherds; i++)
            {
              herd = HRD_herd_list_get (herds, i);
              if (HRD_is_infected (herd))
                {
                  active_infections_today = TRUE;
                  break;
                }
            }

          /* Run the models to get today's changes. */
          naadsm_create_event (manager, EVT_new_new_day_event (day), herds, zones, rng);

          /* Check if the outbreak is over, and if so, whether we can exit this
           * Monte Carlo trial early. */

          /* Check first for active infections... */
          if (active_infections_yesterday)
            {
              if (!active_infections_today)
                {
                  ;
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "no more active infections");
#endif
                }
            }
          else /* there were no active infections yesterday */
            {
              if (active_infections_today)
                {
                  ;
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "active infections again");
#endif
                }
            }

          /* Should the end of the disease phase be recorded? */
          if (!disease_end_recorded && !active_infections_today)
            {
              if (NULL != naadsm_disease_end)
                naadsm_disease_end (day);

              disease_end_recorded = TRUE;
            }


          /* Check the early exit conditions.  If the user wants to exit when
           * the active disease phase ends, then active_infections must be
           * false to exit early.
           *
           * Otherwise, active_infections and pending_actions must both be false
           * to exit early.
           */
          if (stop_on_disease_end)
            {
              if (!active_infections_today)
                {
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "can exit early on end of disease phase");
#endif
                  early_exit = TRUE;
                }
            }
          else
            {
              if (!active_infections_today && !pending_actions)
                {
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "can exit early on end of outbreak");
#endif
                  if (NULL != naadsm_outbreak_end)
                    naadsm_outbreak_end (day);
                  RPT_reporting_set_integer (last_day_of_outbreak, day - 1, NULL);
                  early_exit = TRUE;
                }
            }
          active_infections_yesterday = active_infections_today;

          /* Should the iteration end due to first detection? */
          if ( _iteration.first_detection && (0 != get_stop_on_first_detection( exit_conditions )) )
            early_exit = TRUE;

          naadsm_create_event (manager, EVT_new_end_of_day_event (day, early_exit), herds, zones, rng);

          /* And finally, check for pending actions. */
          pending_actions = FALSE;
          for (i = 0; i < nmodels; i++)
            {
              if (models[i]->has_pending_actions (models[i]))
                {

#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s has pending actions",
                         models[i]->name);
#endif

                  pending_actions = TRUE;
                  break;
                }
            }

          if (NULL != naadsm_show_all_prevalences)
            {
              summary = HRD_herd_list_prevalence_to_string (herds, day);
              naadsm_show_all_prevalences (summary);
              free (summary);
            }

          if (NULL != naadsm_show_all_states)
            {
              summary = HRD_herd_list_summary_to_string (herds);
              naadsm_show_all_states (summary);
              free (summary);
            }

          if (NULL != naadsm_set_zone_perimeters)
            naadsm_set_zone_perimeters (zones);

          if (NULL != naadsm_day_complete)
            naadsm_day_complete (day);

#if defined( USE_MPI ) && !CANCEL_MPI
  m_day_end_time = MPI_Wtime();
  g_debug( "%i: Iteration: %i, Day: %i, Day_Time: %g\n", me.rank, run, day, (double)(m_day_end_time - m_day_start_time ) );
#endif
        } /* end loop over days of one Monte Carlo trial */

      if (NULL != naadsm_iteration_complete)
        naadsm_iteration_complete (run);

#if defined( USE_MPI ) && !CANCEL_MPI
  m_end_time = MPI_Wtime();
  m_total_time = (double)((((double)m_end_time - (double)m_start_time)) + (double)m_total_time);
  #ifdef DEBUG
      g_debug("%i - Run: %d timing: startCount %g, endCount %g, totalCount %g MPI Timer Seconds\n", me.rank, me.rank * _scenario.nruns + run + 1, m_start_time, m_end_time, (double)((double)m_end_time - (double)m_start_time));
  #endif
#endif
    }                           /* loop over all Monte Carlo trials */

  /* Inform the GUI that the simulation has ended */
  if (NULL != naadsm_sim_complete)
    {
      if (-1 == naadsm_simulation_stop ())
        {
          /* simulation was interrupted by the user and did not complete. */
          naadsm_sim_complete (0);
        }
      else
        {
          /* Simulation ran to completion. */
          naadsm_sim_complete (-1);
        }
    }

  /* Clean up. */
  RPT_free_reporting (last_day_of_outbreak);
  RPT_free_reporting (clock_time);
  RPT_free_reporting (version);
  g_ptr_array_free (reporting_vars, TRUE);
  naadsm_free_event_manager (manager);
  naadsm_unload_models (nmodels, models);
  RAN_free_generator (rng);
  ZON_free_zone_list (zones);
  HRD_free_herd_list (herds);

  return;
}

/* end of file main.c */
