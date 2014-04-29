/** @file src/main.c
 * A simulator for animal disease outbreaks.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @author Aaron Reeves <Aaron.Reeves@colostate.edu><br>
 *   Animal Population Health Institute<br>
 *   Colorado State University<br>
 *   Fort Collins, CO 80526-8117<br>
 *   USA
 * @version 0.1
 * @date January 2003
 *
 * Copyright &copy; University of Guelph, 2003-2008
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
 * A simulator for animal disease outbreaks,
 * written to take advantage of a parallel computer that uses the
 * <A href="http://www.mpi-forum.org/">Message Passing Interface</A> (MPI).
 *
 * For background information, please see the
 * <A href="http://hebb.cis.uoguelph.ca/~dastacey/Grid/ERG_ADM">Emergency
 * Response Grid for Animal Disease Modelling</A> website.
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
 *         verbosity = 1 means also print information about events of interest
 *         while setting up and running the simulation.
 *       <li>
 *         verbosity = 2 means also print detailed information about how
 *         calculations and decisions are being performed.
 *     </ul>
 *
 *     The verbosity is implemented by defining a "silent" log handler that
 *     simply ignores messages.  This means that when running with low
 *     verbosity, the program is wasting time building strings that are never
 *     displayed.  To eliminate this waste, specify "--disable-debug" and
 *     "--disable-info" when running the configure script.
 *   <li>
 *     Since the Spanish-language version of NAADSM became available, it is
 *     assumed that text in the input XML may contain accented characters.
 *     The text is converted to ISO-8859-1 inside the simulator, because the
 *     output filters cannot handle multi-byte character encodings.
 *   <li>
 *     Some functions are broken out into libraries when you might not expect
 *     them to be.  This is done for portability.
 *
 *     For example, disease-model.c calls HRD_infect() in herd.c.  Like all the
 *     sub-models, disease-model.c is compiled to a library, which the main
 *     application loads if needed at runtime.
 *
 *     The tricky part is, how does the disease model find HRD_infect()?
 *
 *     Most Unix variants allow <i>back linking</i>: unknown symbols in a
 *     dynamically-loaded library are resolved by looking back into the
 *     application that loaded the library.  In Linux, herd.c could be compiled
 *     right into the application:
 *
 *     @image html back_linking.png
 *     @image latex back_linking.eps width=3in
 *
 *     However, Windows and AIX lack back linking.  For their sake herd.c must
 *     be compiled to a library that can be linked by both the main
 *     application and the sub-model:
 *
 *     @image html no_back_linking.png
 *     @image latex no_back_linking.eps width=2.6in
 *
 *     See chapter 17 of "GNU Autoconf, Automake, and Libtool" for more
 *     information.
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
 *   <li>
 *     <a href="http://kcachegrind.sourceforge.net/cgi-bin/show.cgi">KCachegrind</a>
 *     was used to profile the code without compiling in any extras.
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
 * @image latex complexity.eps "Figure 1. Complexity of the simulation" width=3in
 *
 * Note that the scenario with uncontrolled fast spread shows the worst-case
 * behaviour, because of the large number of units that are infectious
 * simultaneously (Figure 2).
 *
 * @image html uncontrolled_example.png "Figure 2. Example simulation with uncontrolled fast spread."
 * @image latex uncontrolled_example.eps "Figure 2. Example simulation with uncontrolled fast spread." width=3in
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
 * @image latex speedup.eps "Figure 3. Speedup from using a supercomputer." width=3in
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
 * @image latex rtree_benefit.eps "Figure 4. Speedup from using a spatial index." width=3in
 *
 * @image html ontario_tiled.png "Figure 5. An example of a minimum-area rectangle around a set of units."
 * @image latex ontario_tiled.eps "Figure 5. An example of a minimum-area rectangle around a set of units." width=3in
 *
 * This result means that the program should take a "hybrid" approach, using an
 * R-tree index whenever a spatial search over a small area is needed and a
 * \htmlonly na&iuml;ve \endhtmlonly \latexonly na\"ive \endlatexonly search
 * through all units when a search over a large area is needed.  The program's
 * testing must ensure that both mechanisms work.  To this end, the use of
 * R-trees can be shut off entirely by adding -DUSE RTREE=0 to the CFLAGS
 * environment variable when the program is configured.  If CFLAGS includes
 * -DUSE RTREE=1, R-trees will <i>always</i> be used for every spatial search.
 *
 * The uncontrolled fast spread scenarios mentioned earlier experience a
 * considerable speedup with this approach: \htmlonly 4&times; \endhtmlonly
 * \latexonly $4 \times$ \endlatexonly faster with 5000 units, \htmlonly
 * 7&times; \endhtmlonly \latexonly $7 \times$ \endlatexonly with 10000
 * units, and \htmlonly 22&times; \endhtmlonly \latexonly $22 \times$
 * \endlatexonly with 50000 units.
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
 * to \htmlonly na&iuml;ve \endhtmlonly \latexonly na\"ive \endlatexonly
 * search.
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
 * @image latex contact_problem.eps "Figure 6. Finding the recipient unit (B or C) that is closest to the distance d from unit A.  The dashed line is midway between B and C.  If we search only out to distance d, we will choose B as the recipient.  We must search further to find the correct recipient, C." width=3in
 *
 * @image html contact_proof.png "Figure 7. Searching out to distance 2d.  m is midway between B and C; x is the distance from B to m and from m to C.  See text for a proof that C will always be to the left of 2d."
 * @image latex contact_proof.eps "Figure 7. Searching out to distance 2d.  m is midway between B and C; x is the distance from B to m and from m to C.  See text for a proof that C will always be to the left of 2d." width=3in
 */

/** @page bnf BNF grammar for simulator output
 * The simulator's output is not intended to be human-friendly.  Instead, it is
 * intended to be compact and easy to process or "filter" into any number of
 * human-friendly formats.
 *
 * Below is a grammar for the output in Backus-Naur form (BNF).  This grammar
 * can be used with a parser generator like YACC to construct programs to
 * process the simulator output.  (In fact, the filter programs included with
 * the simulator, like full_table.c and exposures_table.c, do exactly that.)
 *
 * output_lines ::= output_line { output_line }
 *
 * output_line ::= tracking_line data_line
 *
 * tracking_line ::= <b>node</b> <b>integer</b> <b>run</b> <b>integer</b>
 *
 * data_line ::= state_codes vars | state_codes | vars | @
 *
 * state_codes ::= <b>integer</b> { <b>integer</b> }
 *
 * vars ::= var { var }
 *
 * var ::= <b>varname</b> "=" value
 *
 * value ::= <b>integer</b> | <b>float</b> | <b>string</b> | <b>polygon</b> "(" ")" | <b>polygon</b> "(" contours ")" | "{" "}" | "{" subvars "}"
 *
 * subvars ::= subvar { "," subvar }
 *
 * subvar ::= <b>string</b> ":" value
 *
 * contours ::= contour { contour }
 *
 * contour ::= "(" coords ")"
 *
 * coords ::= coord { "," coord }
 *
 * coord ::= <b>float</b> <b>float</b>
 *
 * <b>varname</b>, <b>float</b>, <b>integer</b>, and <b>string</b> are
 * terminals in the rules above because they are more convenient to write as
 * regular expressions.  (And more useful in that form too, if you write a
 * YACC parser with Lex scanner to process this grammar.)  Those symbols are
 * defined as:
 *
 * <b>varname</b> = [A-Za-z][A-Za-z0-9_-]*
 *
 * <b>float</b> = [+-]?[0-9]+(\.[0-9]+)([eE][+-][0-9]+)?
 *
 * <b>integer</b> = [+-]?[0-9]+
 *
 * <b>string</b> = '[^']*'
 *
 * The decimal point in the float is "escaped" with a backslash to indicate
 * that it is a literal period, not a regular expression metacharacter.  Note
 * that there is no allowance for a single-quote inside a string as defined
 * here.
 *
 * The state codes are:
 * <table>
 *   <tr><td>0</td><td>Susceptible</td></tr>
 *   <tr><td>1</td><td>Latent</td></tr>
 *   <tr><td>2</td><td>Infectious subclinical</td></tr>
 *   <tr><td>3</td><td>Infectious clinical</td></tr>
 *   <tr><td>4</td><td>Naturally immune</td></tr>
 *   <tr><td>5</td><td>Vaccine immune</td></tr>
 *   <tr><td>6</td><td>Destroyed</td></tr>
 * </table>
 *
 * \section example Example output
 *
 * Below is an example of the simulator output.  It consists of one Monte Carlo
 * trial (run 0) run on one CPU (node 0).  There are 3 units; 2 are initially
 * infected, and on day 2 they infect the unit in the middle.  (This test
 * checks that when 2 units both infect a third, the infection is not
 * double-counted.)
 *
 * \verbatim
node 0 run 0
1 0 1 num-units-infected={'airborne spread':0,'initially infected':2}
node 0 run 0
3 0 3 num-units-infected={'airborne spread':1,'initially infected':0}
node 0 run 0
4 1 4 num-units-infected={'airborne spread':0,'initially infected':0} \endverbatim
 *
 * @sa <a href="filters.html">Output filters</a>
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
 *     <td><b>Component:</b></td> <td>popt</a></td>
 *   </tr>
 *   <tr>
 *     <td><b>Used for:</b></td> <td>handling "command-line options"</td>
 *   </tr>
 *   <tr>
 *     <td><b>Author:</b></td> <td>Michael K. Johnson and Erik W. Troan</td>
 *   </tr>
 *   <tr>
 *     <td><b>License:</b></td>
 *     <td>
 *       "Permission is hereby granted, free of charge, to ...  deal in the
 *       Software without restriction, including without limitation the rights
 *       to use, copy, modify, merge, publish, distribute, sublicense, and/or
 *       sell copies of the Software"
 *       [<a href="popt-COPYING.txt">read full text</a>]
 *     </td>
 *   </tr>
 *   <tr>
 *     <td><b>Website:</b></td>
 *     <td>
 *       <a href="http://freshmeat.net/projects/popt/">http://freshmeat.net/projects/popt/</a>
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
 */


#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <unistd.h>
#include <stdio.h>
#include <popt.h>
#include <time.h>
#include "herd.h"
#include "model_loader.h"
#include "event_manager.h"
#include "herd_zone_updater.h"
#include "reporting.h"
#include "rng.h"

#if HAVE_MPI && !CANCEL_MPI
#  include "mpix.h"
#endif

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_CTYPE_H
#  include <ctype.h>
#endif

#if HAVE_ERRNO_H
#  include <errno.h>
#endif

/*
main.c needs access to the functions defined in guilib.h,
even when compiled as a *nix executable (in which case,
the functions defined will all be NULL).
*/
#include "guilib.h"

extern const char *HRD_status_name[];
extern const char *RPT_frequency_name[];

extern gboolean use_fixed_poisson;
extern double poisson_fix;


/**
 * Global variable for a file output stream.  Needed for redirecting messages
 * sent through g_print().
 */
FILE *output_stream;



/**
 * A print handler that outputs to an open file pointer.
 */
void
file_gprint (const gchar * string)
{
  fprintf (output_stream, "%s", string);
}



/**
 * Modify a provided output file name.  If the program is compiled without MPI
 * support, this just returns a string copy of <i>filename</i>.  If the program
 * is compiled with MPI support, this inserts the node number just before the
 * file extension, or at the end of the filename if there is no file extension.
 */
char *
make_expanded_filename (const char *filename)
{
#if HAVE_MPI && !CANCEL_MPI
  GString *s;
  char *last_dot;
  char *chararray;

  s = g_string_new (NULL);
  last_dot = rindex (filename, '.');
  if (last_dot == NULL)
    {
      /* No file extension; just append the MPI node number. */
      g_string_printf (s, "%s%i", filename, me.rank);
    }
  else
    {
      /* Insert the MPI node number just before the extension. */
      g_string_insert_len (s, -1, filename, last_dot - filename);
      g_string_append_printf (s, "%i", me.rank);
      g_string_insert_len (s, -1, last_dot, strlen (filename) - (last_dot - filename));
    }

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
#else
  return g_strdup (filename);
#endif
}



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
 * Returns of a list of herds that are Latent, Infectious Subclinical,
 * Infectious Clinical, or Naturally Immune.
 */
unsigned int
get_initially_infected_herds (HRD_herd_list_t * herds, HRD_herd_t *** list)
{
  HRD_herd_t **partial_list;
  unsigned int n;
  GArray *array;
  HRD_status_t state;

  /* Concatenate the lists of herds for each diseased state. */
  array = g_array_new (FALSE, FALSE, sizeof (HRD_herd_t *));

  for (state = Latent; state <= NaturallyImmune; state++)
    {
      n = HRD_herd_list_get_by_status (herds, state, &partial_list);
      g_array_append_vals (array, partial_list, n);
      g_free (partial_list);
    }

  /* Don't return the wrapper object. */
  n = array->len;
  *list = (HRD_herd_t **) (array->data);
  g_array_free (array, FALSE);
  return n;
}



/**
 * Returns of a list of herds that are Vaccine Immune.
 */
unsigned int
get_initially_immune_herds (HRD_herd_list_t * herds, HRD_herd_t *** list)
{
  HRD_herd_t **partial_list;
  unsigned int n;
  GArray *array;

  array = g_array_new (FALSE, FALSE, sizeof (HRD_herd_t *));
  n = HRD_herd_list_get_by_status (herds, VaccineImmune, &partial_list);
  g_array_append_vals (array, partial_list, n);
  g_free (partial_list);

  /* Don't return the wrapper object. */
  n = array->len;
  *list = (HRD_herd_t **) (array->data);
  g_array_free (array, FALSE);
  return n;
}



/**
 * Returns of a list of herds that are Destroyed.
 */
unsigned int
get_initially_destroyed_herds (HRD_herd_list_t * herds, HRD_herd_t *** list)
{
  HRD_herd_t **partial_list;
  unsigned int n;
  GArray *array;

  array = g_array_new (FALSE, FALSE, sizeof (HRD_herd_t *));
  n = HRD_herd_list_get_by_status (herds, Destroyed, &partial_list);
  g_array_append_vals (array, partial_list, n);
  g_free (partial_list);

  /* Don't return the wrapper object. */
  n = array->len;
  *list = (HRD_herd_t **) (array->data);
  g_array_free (array, FALSE);
  return n;
}



/**
 * A structure for use with the function build_report, below.
 */
typedef struct
{
  GString *string;
  unsigned int day;
  gboolean include_all;
}
build_report_args_t;



/**
 * This function is meant to be used with the foreach function of a GLib
 * Pointer Array, specifically, the Pointer Array used to store output
 * variables.  It appends strings of the form " variable=value" to a GString.
 *
 * @param data an output variable, cast to a gpointer.
 * @param user_data a pointer to a build_report_t structure, cast to a
 *   gpointer.
 */
void
build_report (gpointer data, gpointer user_data)
{
  RPT_reporting_t *reporting;
  build_report_args_t *build_report_args;
  char *substring;

  reporting = (RPT_reporting_t *) data;
  build_report_args = (build_report_args_t *) user_data;
  if (RPT_reporting_due (reporting, build_report_args->day)
      || (build_report_args->include_all && reporting->frequency != RPT_never))
    {
      substring = RPT_reporting_value_to_string (reporting, NULL);
      g_string_append_printf (build_report_args->string, " %s=%s", reporting->name, substring);
      free (substring);
      if (!reporting->cumulative)
        RPT_reporting_zero (reporting);
    }
}



DLL_API void
run_sim_main (char *herd_file,
              char *parameter_file,
              char *output_file, char *model_dir, double fixed_rng_value, int verbosity, int seed)
{
  unsigned int ndays, nruns, day, run;
  double prevalence_num, prevalence_denom;
  RPT_reporting_t *show_unit_states;
  RPT_reporting_t *num_units_in_state;
  RPT_reporting_t *num_units_in_state_by_prodtype;
  RPT_reporting_t *num_animals_in_state;
  RPT_reporting_t *num_animals_in_state_by_prodtype;
  RPT_reporting_t *avg_prevalence;
  RPT_reporting_t *last_day_of_outbreak;
  RPT_reporting_t *clock_time;
  RPT_reporting_t *version;
  GPtrArray *reporting_vars;
  int nmodels = 0;
  ergadm_model_t **models = NULL;
  ergadm_event_manager_t *manager;
  unsigned int nherds;
  HRD_herd_list_t *herds;
  HRD_herd_t *herd;
  RAN_gen_t *rng;
  unsigned int ninitially_infected_herds, ninitially_immune_herds, ninitially_destroyed_herds;
  HRD_herd_t **initially_infected_herds, **initially_immune_herds, **initially_destroyed_herds;
  unsigned int nzones;
  ZON_zone_list_t *zones;
  ZON_zone_t *zone;
  int i, j;                     /* loop counters */
  char *drill_down_list[3] = { NULL, NULL, NULL };
  gboolean active_infections, pending_actions, pending_infections, disease_end_recorded,
    stop_on_disease_end, early_exit;
  time_t start_time, finish_time;
  build_report_args_t build_report_args;
  char *summary;
  GString *s;
  char *prev_summary;
  char guilog[1024];

  if (NULL != guilib_printf)
    {
      sprintf (guilog, "Running sim with params = %s, herds = %s, outputs = %s, verbosity = %d",
               parameter_file, herd_file, output_file, verbosity);
      guilib_printf (guilog);
    }

  /* Open a file for output, if specified; if not, use stdout. */
  if (output_file)
    {
      output_file = make_expanded_filename (output_file);
      output_stream = fopen (output_file, "w");
      if (output_stream == NULL)
        {
          /* FIXME: use errno to provide a more helpful message. */
          g_error ("Could not open file \"%s\" for writing.", output_file);
        }
      g_set_print_handler (file_gprint);
    }

  /* This line prints a Byte Order Mark (BOM) that indicates that the output is
   * in UTF-8.  Not currently used. */
  /*
  if (NULL == guilib_printf)
    g_print ("%s", "\xEF\xBB\xBF");
  */

  /* Set the verbosity level. */
  if (verbosity < 2)
    {
      g_log_set_handler (NULL, G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("herd", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("prob_dist", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("rel_chart", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("reporting", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("zone", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("gis", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
    }
  if (verbosity < 1)
    {
      g_log_set_handler (NULL, G_LOG_LEVEL_INFO, silent_log_handler, NULL);
      g_log_set_handler ("herd", G_LOG_LEVEL_INFO, silent_log_handler, NULL);
      g_log_set_handler ("prob_dist", G_LOG_LEVEL_INFO, silent_log_handler, NULL);
      g_log_set_handler ("rel_chart", G_LOG_LEVEL_INFO, silent_log_handler, NULL);
      g_log_set_handler ("reporting", G_LOG_LEVEL_INFO, silent_log_handler, NULL);
      g_log_set_handler ("zone", G_LOG_LEVEL_INFO, silent_log_handler, NULL);
      g_log_set_handler ("gis", G_LOG_LEVEL_INFO, silent_log_handler, NULL);
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "verbosity = %i", verbosity);
#endif

  if (NULL != guilib_debug)
    guilib_debug ("Loading herds list...");

  /* Get the list of herds. */
  if (herd_file)
    {
      herds = HRD_load_herd_list (herd_file);
      nherds = HRD_herd_list_length (herds);
    }
  else
    {
      herds = NULL;
      nherds = 0;
    }

  if (NULL != guilib_debug)
    guilib_debug ("Herds list loaded.");

#if INFO
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "%i units read", nherds);
#endif
  if (nherds == 0)
    g_error ("no units in file %s", herd_file);

#ifdef FIX_ME                   // FIXME: this block causes a crash on Windows
#if DEBUG
  summary = HRD_herd_list_to_string (herds);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "\n%s", summary);
  free (summary);
#endif
#endif

  /* Record the initially infected herds. */
  ninitially_infected_herds = get_initially_infected_herds (herds, &initially_infected_herds);
  if (ninitially_infected_herds == 0)
    g_warning ("no units initially infected");

  /* Record the initially immune herds. */
  ninitially_immune_herds = get_initially_immune_herds (herds, &initially_immune_herds);

  /* Record the initially destroyed herds. */
  ninitially_destroyed_herds = get_initially_destroyed_herds (herds, &initially_destroyed_herds);

  s = g_string_new (NULL);

#ifdef FIX_ME                   // FIXME: this block causes a crash on Windows
#if INFO
  g_string_printf (s, "%u units initially infected:", ninitially_infected_herds);
  for (i = 0; i < ninitially_infected_herds; i++)
    g_string_append_printf (s, " %s", initially_infected_herds[i]->official_id);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, s->str);
#endif
#endif

  if (NULL != guilib_debug)
    guilib_debug ("initializing reporting variables...");


  /* Initialize the reporting variables, and bundle them together so they can
   * easily be sent to a function for initialization. */
  show_unit_states = RPT_new_reporting ("all-units-states", NULL, RPT_integer, RPT_never, FALSE);
  num_units_in_state =
    RPT_new_reporting ("num-units-in-each-state", NULL, RPT_group, RPT_never, FALSE);
  num_units_in_state_by_prodtype =
    RPT_new_reporting ("num-units-in-each-state-by-production-type", NULL, RPT_group, RPT_never,
                       FALSE);
  num_animals_in_state =
    RPT_new_reporting ("num-animals-in-each-state", NULL, RPT_group, RPT_never, FALSE);
  num_animals_in_state_by_prodtype =
    RPT_new_reporting ("num-animals-in-each-state-by-production-type", NULL, RPT_group, RPT_never,
                       FALSE);
  for (i = 0; i < HRD_NSTATES; i++)
    {
      RPT_reporting_set_integer1 (num_units_in_state, 0, HRD_status_name[i]);
      RPT_reporting_set_integer1 (num_animals_in_state, 0, HRD_status_name[i]);
      drill_down_list[1] = HRD_status_name[i];
      for (j = 0; j < herds->production_type_names->len; j++)
        {
          drill_down_list[0] = (char *) g_ptr_array_index (herds->production_type_names, j);
          RPT_reporting_set_integer (num_units_in_state_by_prodtype, 0, drill_down_list);
          RPT_reporting_set_integer (num_animals_in_state_by_prodtype, 0, drill_down_list);
        }
    }
  avg_prevalence = RPT_new_reporting ("average-prevalence", NULL, RPT_real, RPT_never, TRUE);
  last_day_of_outbreak =
    RPT_new_reporting ("time-to-end-of-outbreak", NULL, RPT_integer, RPT_never, TRUE);
  clock_time = RPT_new_reporting ("clock-time", NULL, RPT_real, RPT_never, TRUE);
  version = RPT_new_reporting ("version", NULL, RPT_text, RPT_never, TRUE);
  RPT_reporting_set_text (version, PACKAGE_VERSION, NULL);
  reporting_vars = g_ptr_array_new ();
  g_ptr_array_add (reporting_vars, show_unit_states);
  g_ptr_array_add (reporting_vars, num_units_in_state);
  g_ptr_array_add (reporting_vars, num_units_in_state_by_prodtype);
  g_ptr_array_add (reporting_vars, num_animals_in_state);
  g_ptr_array_add (reporting_vars, num_animals_in_state_by_prodtype);
  g_ptr_array_add (reporting_vars, avg_prevalence);
  g_ptr_array_add (reporting_vars, last_day_of_outbreak);
  g_ptr_array_add (reporting_vars, clock_time);
  g_ptr_array_add (reporting_vars, version);

  if (NULL != guilib_debug)
    guilib_debug ("Reporting variables initialized.");

  /* Pre-create a "background" zone. */
  zones = ZON_new_zone_list (nherds);
  zone = ZON_new_zone ("", -1, 0.0);
  ZON_zone_list_append (zones, zone);

  /* Get the simulation parameters and sub-models. */
  if (NULL != guilib_debug)
    guilib_debug ("Loading models...");

  nmodels =
    ergadm_load_models (parameter_file, herds, zones, model_dir, &ndays, &nruns, &models,
                        reporting_vars);
  nzones = ZON_zone_list_length (zones);

  if (NULL != guilib_debug)
    guilib_debug ("Models loaded.");

  /* The clock time reporting variable is special -- it can only be reported
   * once (at the end of each simulation) or never. */
  if (clock_time->frequency != RPT_never && clock_time->frequency != RPT_once)
    {
      g_warning ("clock-time cannot be reported %s; it will reported at the end of each simulation",
                 RPT_frequency_name[clock_time->frequency]);
      RPT_reporting_set_frequency (clock_time, RPT_once);
    }

  /* Now that the reporting frequency of show_unit_states has been set from the
   * simulation parameters, remove that variable from the list of reporting
   * variables, because it is treated specially. */
  g_ptr_array_remove (reporting_vars, show_unit_states);

#if HAVE_MPI && !CANCEL_MPI
  /* Increase the number of runs to divide evenly by the number of processors,
   * if necessary. */
  if (nruns % me.np != 0)
    nruns += (me.np - nruns % me.np);
  nruns /= me.np;               /* because it's parallel, wheee! */
#endif

#if INFO
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "simulation %u days x %u runs", ndays, nruns);
#endif


  /* Initialize the pseudo-random number generator. */
  rng = RAN_new_generator (seed);
  if (fixed_rng_value >= 0 && fixed_rng_value < 1)
    {
      if (NULL != guilib_printf)
        {
          sprintf (guilog, "rng set to fixed value %f", fixed_rng_value);
          guilib_printf (guilog);
        }
      RAN_fix (rng, fixed_rng_value);
    }

  manager = ergadm_new_event_manager (models, nmodels);

  build_report_args.string = s;

  /* Determine whether each iteration should end when the active disease phase ends. */
  if (NULL != guilib_stop_on_disease_end)
    stop_on_disease_end = (0 != guilib_stop_on_disease_end ());
  else
    stop_on_disease_end = FALSE;

  if (NULL != guilib_sim_start)
    guilib_sim_start ();

  /* Begin the loop over the specified number of iterations. */
  for (run = 0; run < nruns; run++)
    {
      /* Does the GUI user want to stop a simulation in progress? */
      if (NULL != guilib_simulation_stop)
        {
          if (0 != guilib_simulation_stop ())
            break;
        }

      if (NULL != guilib_printf)
        {
          sprintf (guilog, "RUN NUMBER %d...", run);
          guilib_printf (guilog);
        }

#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "resetting everything before start of simulation");
#endif

      if (NULL != guilib_iteration_start)
        guilib_iteration_start (run);

      /* Reset all herds to Susceptible. */
      for (i = 0; i < nherds; i++)
        {
          HRD_reset (HRD_herd_list_get (herds, i));
        }

      /* Reset reporting variables. */
      RPT_reporting_reset (last_day_of_outbreak);

      /* Reset all models. */
      for (i = 0; i < nmodels; i++)
        models[i]->reset (models[i]);

      /* Reset all zones. */
      ZON_zone_list_reset (zones);

      /* Set the latent period, contagious period, etc. for initially infected
       * herds. */
      day = 0;
      for (i = 0; i < ninitially_infected_herds; i++)
        {
          herd = initially_infected_herds[i];
          ergadm_create_event (manager,
                               EVT_new_inprogress_infection_event (NULL, herd, day,
                                                                   "initially infected",
                                                                   herd->initial_status,
                                                                   herd->
                                                                   days_left_in_initial_status),
                               herds, zones, rng);
        }
      for (i = 0; i < ninitially_immune_herds; i++)
        {
          herd = initially_immune_herds[i];
          ergadm_create_event (manager,
                               EVT_new_inprogress_immunity_event (herd, day, "initially immune",
                                                                  herd->initial_status,
                                                                  herd->
                                                                  days_left_in_initial_status),
                               herds, zones, rng);
        }
      for (i = 0; i < ninitially_destroyed_herds; i++)
        {
          herd = initially_destroyed_herds[i];
          ergadm_create_event (manager,
                               EVT_new_attempt_to_destroy_event (herd, day, "initially destroyed"),
                               herds, zones, rng);
        }
      ergadm_create_event (manager, EVT_new_end_of_day_event (0), herds, zones, rng);
      active_infections = (ninitially_infected_herds > 0);
      pending_actions = TRUE;
      pending_infections = TRUE;
      disease_end_recorded = FALSE;
      early_exit = FALSE;

      if (NULL != guilib_reset_detection_end)
        guilib_reset_detection_end ();

      /* Run the iteration. */
      start_time = time (NULL);

      /* Begin the loop over the days in an iteration. */
      for (day = 1; (day <= ndays) && (!early_exit); day++)
        {
          /* Does the GUI user want to stop a simulation in progress? */
          if (NULL != guilib_simulation_stop)
            {
              /* This check may break the day loop.
               * If necessary, Another check (see above) will break the iteration loop.*/
              if (0 != guilib_simulation_stop ())
                break;
            }

          /* Should the iteration end due to first detection? */
          if (NULL != guilib_stop_on_detection)
            {
              /* This check may break the day loop, but the iteration loop should always continue. */
              if (0 != guilib_stop_on_detection ())
                break;
            }

          if (NULL != guilib_day_start)
            guilib_day_start (day);

          /* Process changes made to the herds on the previous day.  At the
           * same time, count the number of herds and animals infected,
           * vaccinated, and destroyed, and the number of herds and animals in
           * each state. */
          RPT_reporting_zero (num_units_in_state);
          RPT_reporting_zero (num_animals_in_state);
          prevalence_num = prevalence_denom = 0;
          for (i = 0; i < nherds; i++)
            {
              herd = HRD_herd_list_get (herds, i);
              HRD_step (herd);

              RPT_reporting_add_integer1 (num_units_in_state, 1, HRD_status_name[herd->status]);
              RPT_reporting_add_integer1 (num_animals_in_state, herd->size,
                                          HRD_status_name[herd->status]);
              drill_down_list[0] = herd->production_type_name;
              drill_down_list[1] = HRD_status_name[herd->status];
              RPT_reporting_add_integer (num_units_in_state_by_prodtype, 1, drill_down_list);
              RPT_reporting_add_integer (num_animals_in_state_by_prodtype, herd->size,
                                         drill_down_list);

              if (herd->status >= Latent && herd->status <= InfectiousClinical)
                {
                  prevalence_num += herd->size * herd->prevalence;
                  prevalence_denom += herd->size;
                }
              RPT_reporting_set_real (avg_prevalence, (prevalence_denom > 0) ?
                                      prevalence_num / prevalence_denom : 0, NULL);
            }                   /* end loop over herds */

          /* Process changes made to the zones on the previous day, that is,
           * update the zones with any new foci.  At the same time, update the
           * records of which herd is in which zone. */
          ergadm_update_herd_zones (herds, zones);

          /* Run the models to get today's changes. */
          ergadm_create_event (manager, EVT_new_new_day_event (day), herds, zones, rng);
          ergadm_create_event (manager, EVT_new_end_of_day_event (day), herds, zones, rng);

          /* Check if the outbreak is over, and if so, whether we can exit this
           * Monte Carlo trial early. */

          /* Check first for active infections... */
          if (active_infections)
            {
              if (RPT_reporting_get_integer1 (num_units_in_state, HRD_status_name[Latent]) == 0
                  && RPT_reporting_get_integer1 (num_units_in_state,
                                                 HRD_status_name[InfectiousSubclinical]) == 0
                  && RPT_reporting_get_integer1 (num_units_in_state,
                                                 HRD_status_name[InfectiousClinical]) == 0)
                {
                  active_infections = FALSE;
                  RPT_reporting_set_integer (last_day_of_outbreak, day - 1, NULL);
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "no more active infections");
#endif
                  if (NULL != guilib_printf)
                    guilib_printf ("No more active infections");

                }
            }
          else
            {
              if (RPT_reporting_get_integer1 (num_units_in_state, HRD_status_name[Latent]) > 0
                  || RPT_reporting_get_integer1 (num_units_in_state,
                                                 HRD_status_name[InfectiousSubclinical]) > 0
                  || RPT_reporting_get_integer1 (num_units_in_state,
                                                 HRD_status_name[InfectiousClinical]) > 0)
                {
                  active_infections = TRUE;
                  RPT_reporting_set_integer (last_day_of_outbreak, 0, NULL);
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "active infections again");
#endif
                  if (NULL != guilib_printf)
                    guilib_printf ("Active infections again");
                }
            }


          /* Should the end of the disease phase be recorded? */
          if (!disease_end_recorded && !active_infections && !pending_infections)
            {
              if (NULL != guilib_printf)
                {
                  sprintf (guilog, "Disease phase is over as of day %d.", day);
                  guilib_printf (guilog);
                }

              if (NULL != guilib_disease_end)
                guilib_disease_end (day);

              disease_end_recorded = TRUE;
            }


          /* Check the early exit conditions.  If the user wants to exit when
           * the active disease phase ends, then active_infections and pending_infections
           * must both be false to exit early.
           *
           * Otherwise, active_infections and pending_actions must both be false
           * to exit early.
           */
          if (stop_on_disease_end)
            {
              if (!active_infections && !pending_infections)
                {
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "can exit early on end of disease phase");
#endif
                  if (NULL != guilib_printf)
                    guilib_printf ("can exit early on end of disease phase");

                  early_exit = TRUE;
                }
            }
          else
            {
              if (!active_infections && !pending_actions)
                {
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "can exit early on end of outbreak");
#endif
                  if (NULL != guilib_printf)
                    guilib_printf ("can exit early on end of outbreak");

                  if (NULL != guilib_outbreak_end)
                    guilib_outbreak_end (day);

                  early_exit = TRUE;
                }
            }


          /* Next, check for pending actions... */
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


          /* And finally, check for pending infections. */
          pending_infections = FALSE;
          for (i = 0; i < nmodels; i++)
            {
              if (models[i]->has_pending_infections (models[i]))
                {

#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s has pending infections",
                         models[i]->name);
#endif
                  if (NULL != guilib_printf)
                    {
                      sprintf (guilog, "%s has pending infections", models[i]->name);
                      guilib_printf (guilog);
                    }
                  pending_infections = TRUE;
                  break;
                }
            }


          /* Build the daily output string.  Start with the special case
           * variable that tells whether to output the state of every unit. */
          if (RPT_reporting_due (show_unit_states, day - 1)
              || (early_exit && show_unit_states->frequency != RPT_never))            
            {
              summary = HRD_herd_list_summary_to_string (herds);
              g_string_printf (s, "%s", summary);
              free (summary);
            }
          else
            g_string_truncate (s, 0);

          /* For the other output variables, append text in the format
           * variable-name=value to the output string. */
          build_report_args.day = day - 1;
          build_report_args.include_all = (early_exit || day == ndays);
          if (build_report_args.include_all)
            {
              finish_time = time (NULL);
              RPT_reporting_set_real (clock_time, (double) (finish_time - start_time), NULL);
              ergadm_create_event (manager, EVT_new_last_day_event (day), herds, zones, rng);
            }
          g_ptr_array_foreach (reporting_vars, build_report, &build_report_args);

/* The DLL shouldn't output anything directly to the console.  Strange things happen... */
#ifndef WIN_DLL
#if HAVE_MPI && !CANCEL_MPI
          g_print ("node %i run %u\n%s\n", me.rank, run, s->str);
#else
          g_print ("node 0 run %u\n%s\n", run, s->str);
#endif
#endif

          if (NULL != guilib_show_all_prevalences) 
            {
              prev_summary = HRD_herd_list_prevalence_to_string (herds, day);
              guilib_show_all_prevalences (prev_summary);
              free (prev_summary);     
            }

          if (NULL != guilib_show_all_states)
            guilib_show_all_states (s->str);

          if ( NULL != guilib_set_zone_perimeters )
            guilib_set_zone_perimeters ( zones );
            
          if (NULL != guilib_day_complete)
            guilib_day_complete (day);
        }                       /* end loop over days of one Monte Carlo trial */


      if (NULL != guilib_iteration_complete)
        guilib_iteration_complete (run);

    }                           /* loop over all Monte Carlo trials */


  /* Inform the GUI that the simulation has ended */
  if (NULL != guilib_sim_complete)
    {
      if (-1 == guilib_simulation_stop ())
        {
          /* simulation was interrupted by the user and did not complete. */
          guilib_sim_complete (0);
        }
      else
        {
          /* Simulation ran to completion. */
          guilib_sim_complete (-1);
        }
    }

  /* Clean up. */
  RPT_free_reporting (show_unit_states, TRUE);
  RPT_free_reporting (num_units_in_state, TRUE);
  RPT_free_reporting (num_units_in_state_by_prodtype, TRUE);
  RPT_free_reporting (num_animals_in_state, TRUE);
  RPT_free_reporting (num_animals_in_state_by_prodtype, TRUE);
  RPT_free_reporting (avg_prevalence, TRUE);
  RPT_free_reporting (last_day_of_outbreak, TRUE);
  RPT_free_reporting (clock_time, TRUE);
  RPT_free_reporting (version, TRUE);
  g_ptr_array_free (reporting_vars, TRUE);
  g_string_free (s, TRUE);
  ergadm_free_event_manager (manager);
  ergadm_unload_models (nmodels, models);
  g_free (initially_infected_herds);
  g_free (initially_immune_herds);
  g_free (initially_destroyed_herds);
  RAN_free_generator (rng);
  ZON_free_zone_list (zones);
  HRD_free_herd_list (herds);
  if (output_stream != NULL)
    fclose (output_stream);

  return;
}


int
main (int argc, char *argv[])
{
  poptContext option;
  char *model_dir = NULL;
  int verbosity = 0;
  const char *parameter_file = NULL;
  const char *herd_file = NULL;
  const char *output_file = NULL;
  double fixed_rng_value = -1;
  struct poptOption options[6];

#if HAVE_MPI && !CANCEL_MPI
  /* Initialize MPI. */
  if (MPIx_Init (&argc, &argv) != MPI_SUCCESS)
    g_error ("Couldn't initialize MPI.");
#endif

  clear_guilib_fns ();
  clear_rng_fns ();

  options[0].longName = "herd-file";
  options[0].shortName = 'h';
  options[0].argInfo = POPT_ARG_STRING;
  options[0].arg = &herd_file;
  options[0].val = 0;
  options[0].descrip = "Herd file";
  options[0].argDescrip = "herd-file";

  options[1].longName = "model-dir";
  options[1].shortName = '\0';
  options[1].argInfo = POPT_ARG_STRING;
  options[1].arg = &model_dir;
  options[1].val = 0;
  options[1].descrip = "Directory containing sub-models";
  options[1].argDescrip = "model-dir";

  options[2].longName = "verbosity";
  options[2].shortName = 'V';
  options[2].argInfo = POPT_ARG_INT;
  options[2].arg = &verbosity;
  options[2].val = 0;
  options[2].descrip =
    "Message verbosity level (0 = simulation output only, 1 = + informational messages, 2 = + all debugging output)";
  options[2].argDescrip = "verbosity";

  options[3].longName = "output-file";
  options[3].shortName = 'o';
  options[3].argInfo = POPT_ARG_STRING;
  options[3].arg = &output_file;
  options[3].val = 0;
  options[3].descrip = "Output file";
  options[3].argDescrip = "output-file";

  options[4].longName = "fixed-random-value";
  options[4].shortName = 'r';
  options[4].argInfo = POPT_ARG_DOUBLE;
  options[4].arg = &fixed_rng_value;
  options[4].val = 0;
  options[4].descrip = "Fixed number to use instead of random numbers";
  options[4].argDescrip = "fixed-random-value";

  options[5].longName = NULL;
  options[5].shortName = '\0';
  options[5].argInfo = 0;
  options[5].arg = NULL;
  options[5].val = 0;
  options[5].descrip = NULL;
  options[5].argDescrip = NULL;

  option = poptGetContext (NULL, argc, (const char **) argv, options, 0);
  poptGetNextOpt (option);

  parameter_file = poptGetArg (option);
  poptFreeContext (option);

  run_sim_main (herd_file, parameter_file, output_file, model_dir, fixed_rng_value, verbosity, -1);

#if HAVE_MPI && !CANCEL_MPI
  MPI_Finalize ();
#endif

  return EXIT_SUCCESS;
}



/* end of file main.c */
