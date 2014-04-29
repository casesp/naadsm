#!/usr/bin/env python
"""This script converts an XML description of test cases into a C comment
formatted for Doxygen.

Copyright (C) University of Guelph, 2003-2008

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version."""

__author__ = "Neil Harvey <neilharvey@gmail.com>"
__date__ = "December 2003"

import re
import sys
import time
import xml.dom.minidom
from sets import Set
from string import upper
from warnings import warn

OUTPUT_ENCODING = "utf-8" # Doxygen's preferred encoding



def getText (element):
	"""Returns the contents of all text children of the given element,
	concatenated, as a string."""
	text = ""
	for node in element.childNodes:
		if node.nodeType == node.TEXT_NODE:
			text += node.data

	return text.strip()



def main ():
	doc = xml.dom.minidom.parse (sys.stdin)
	print """\
/** @file
 *
 * A dummy C file containing only comments.  This file causes the
 * <a href="testsuite.html">model test suite pages</a> to be built.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date %s
 *
 * Copyright &copy; University of Guelph, 2003-%s
 */
""" % (time.strftime ("%B %Y"), time.strftime ("%Y"))

	print """/** @page testsuite Model test suite
 *
 * Some thoughts about testing:	
 *
 * "The first principle is that you must not fool yourself
 * \htmlonly &ndash; \endhtmlonly \latexonly -- \endlatexonly
 * and you are the easiest person to fool.
 * I'm talking about a specific, extra type of integrity ...
 * bending over backwards to show how you're maybe wrong."<br>
 * \htmlonly &ndash; \endhtmlonly \latexonly -- \endlatexonly Richard Feynman,
 * in a Caltech commencement address given in 1974,
 * also in <i>Surely You're Joking, Mr. Feynman!</i>
 *
 * "Trust Jesus"<br>
 * \htmlonly &ndash; \endhtmlonly \latexonly -- \endlatexonly graffiti on a
 * sidewalk near Portland State U<br>
 * "but cut the cards"<br>
 * \htmlonly &ndash; \endhtmlonly \latexonly -- \endlatexonly added below
 *"""

	# Find the categories.  We will sort the output by these.
	categories = Set ([getText (element) for element in doc.getElementsByTagName ("category")])
	categories = list (categories)
	categories.sort()

	# Create "display" names for each category by changing dashes to spaces and
	# capitalizing the first letter.
	for i in range(len(categories)):
		displayName = categories[i].replace("-", " ")
		displayName = re.sub ("^[a-z]", lambda m: upper(m.group(0)), displayName)
		categories[i] = (categories[i], displayName)

	tests = filter (
	  lambda node:node.nodeType == node.ELEMENT_NODE
	    and node.localName in ("deterministic-test", "stochastic-test", "variable-test", "stochastic-variable-test"),
	  doc.getElementsByTagName ("tests")[0].childNodes
	)

	# Count how many tests there are in each category.
	testcount = {}
	for category, displayName in categories:
		testcount[category] = 0
	for test in tests:
		category = getText (test.getElementsByTagName("category")[0])
		testcount[category] += 1

	# Print an index at the top.
	print ' * <ul>'
	for category, displayName in categories:
		print ' *   <li><a href="testsuite-%s.html">%s</a> (%i tests)' % \
		  (category, displayName, testcount[category])
		num = 0
		print ' *     <ul>'
		for test in tests:	
			if getText (test.getElementsByTagName ("category")[0]) != category:
				continue
			num += 1
			print ' *       <li><a href="testsuite-%s.html#%s%i">%s</a></li>' \
			  % (category, category, num, getText (test.getElementsByTagName ("short-name")[0]))
		print ' *     </ul>'
	print ' * </ul>'
	print ' *'
	print ' * Total %i tests' % sum ([testcount[category] for category in testcount.keys()])
	print ' */'

	# Print a page of detailed entries for each category.
	for category, displayName in categories:
		print
		print '/** @page testsuite-%s %s tests\n *' % (category, displayName)

		# Print an index at the top.
		print ' * <ul>'
		num = 0
		for test in tests:	
			if getText (test.getElementsByTagName ("category")[0]) != category:
				continue
			num += 1
			print ' *   <li><a href="#%s%i">%s</a></li>' \
			  % (category, num, getText (test.getElementsByTagName ("short-name")[0]))
		print ' * </ul>'
		print ' *'
		print ' * Total %i tests' % testcount[category]
		print ' *'
		print ' * <a href="testsuite.html"><b>Back</b></a> to master list of tests.\n *'
		print ' *'

		num = 0
		for test in tests:
			if getText (test.getElementsByTagName ("category")[0]) != category:
				continue
			num += 1
			shortName = getText (test.getElementsByTagName ("short-name")[0])
			print ' * @section %s%i %s\n *' % (category, num, shortName)

			testtype = test.tagName

			description = getText (test.getElementsByTagName ("description")[0])
			for line in description.split('\n'):
				print ' * %s' % line.strip()
			print ' *'

			try:
				author = getText (test.getElementsByTagName ("author")[0])
				print ' * Author: %s' % author.encode(OUTPUT_ENCODING, "replace")
				print ' *'
			except IndexError:
				warn ("Missing author for test %s/%s" % (category, shortName))

			try:
				creation_date = getText (test.getElementsByTagName ("creation-date")[0])
				print ' * Created: %s' % creation_date
				print ' *'
			except IndexError:
				warn ("Missing creation date for test %s/%s" % (category, shortName))

			try:
				model_version = getText (test.getElementsByTagName ("model-version")[0])
				print ' * Applies to model specification %s' % model_version
				print ' *'
			except IndexError:
				warn ("Missing model spec version for test %s/%s" % (category, shortName))

			parameters = getText (test.getElementsByTagName ("parameter-description")[0])
			for line in parameters.split('\n'):
				print ' * %s' % line.strip().encode(OUTPUT_ENCODING, "replace")
			print ' *'

			paramFileName = getText (test.getElementsByTagName ("parameter-file")[0])
			print ' * See the <a href="test-xml-%s-%s.html">parameters as XML</a>.' \
			  % (category, paramFileName)
			print ' *'

			herdFileName = getText (test.getElementsByTagName ("herd-file")[0])
			diagrams = test.getElementsByTagName ("diagram")
			if len (diagrams) >= 1:
				for element in diagrams:
					diagram = getText (element)
					print ' * @image html %s.png Using %s.xml' % (diagram, herdFileName)
					print ' * @image latex %s.eps "Using %s.xml" width=3in' % (diagram, herdFileName)
			else:
				print ' * @image html %s.png Using %s.xml' % (herdFileName, herdFileName)
				print ' * @image latex %s.eps "Using %s.xml" width=3in' % (herdFileName, herdFileName)
			print ' *'

			noutcomes = 0
			tables = test.getElementsByTagName ("output")
			for table in tables:
				probability = table.getAttribute ("probability")
				if probability != "":
					noutcomes += 1
					print ' * Possible outcome #%i (expected frequency %s):' % (noutcomes, probability)
					print ' *'
				print ' * <table>'
				print ' *   <tr>'
				print ' *     <th>Day</th>',
				rows = table.getElementsByTagName ("tr")
				if testtype == "deterministic-test" or testtype == "stochastic-test":
					# Find out how many units there are.
					nherds = len (rows[0].getElementsByTagName ("td"))
					print '<th>Unit 0</th>',
					for i in range (1, nherds):
						print ('<th>%i</th>' % i),
				elif testtype == "variable-test" or testtype == "stochastic-variable-test":
					# Find out how many output variables there are.
					for cell in rows[0].getElementsByTagName ("td"):
						print ('<th>%s</th>' % getText(cell).encode(OUTPUT_ENCODING, "replace")),
					del rows[0]
				print '\n *   </tr>'

				day = 0
				for row in rows:
					print ' *   <tr>'
					day += 1
					print (' *     <td>%i</td>' % day),
					for cell in row.getElementsByTagName ("td"):
						state = getText (cell)
						print ('<td class="%s">%s</td>' % (state, state)),
					print '\n *   </tr>'
				print ' * </table>\n *'

			print ' * <a href="#testsuite-%s"><b>Back</b></a> to list of %s tests.\n *' \
			  % (category, displayName)
			print ' * <a href="testsuite.html"><b>Back</b></a> to master list of tests.\n *'
		print ' */'

	# Create pages showing the actual XML parameter files.
	for category, displayName in categories:
		for test in tests:
			if getText (test.getElementsByTagName ("category")[0]) != category:
				continue

			paramFileName = getText (test.getElementsByTagName ("parameter-file")[0])

			print
			print "/** @page test-xml-%s-%s %s/%s.xml File Reference" \
			  % (category, paramFileName, category, paramFileName)
			print " * @verbinclude model.%s/%s.xml" % (category, paramFileName)
			print " */"

	print
	print "/* end of file */"


if __name__ == "__main__":
	main()
