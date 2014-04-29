#!/usr/bin/env python
"""This script plots one or more output variables from a simulation.  It reads
a table of outputs in comma-separated value format from standard input.

It will plot specific requested variables or, by default, will try to plot the
number of units in each state on each day.  It will plot results from specific
requested simulations on specific requested nodes or, by default, will plot the
variables from all simulations."""

__author__ = "Neil Harvey <neilharvey@canada.com>"
__date__ = "August 2004"

import getopt
import os
import string
import sys
from line import Line

STATE_NAMES = ["Susceptible", "Latent", "Infectious Subclinical",
  "Infectious Clinical", "Naturally Immune", "Vaccine Immune", "Destroyed"]




def usage ():
	"""Prints a usage message."""
	print """\
Usage: python graph.py2 [OPTIONS] "variablename1,variablename2,..." < TABLE-FILE

Options are:
-n, --node INT              only show results from given node (default = all)
-r, --run INT               only show results from given run (default = all)
-x, --xlabel TEXT           set a label for the x-axis
-y, --ylabel TEXT           set a label for the y-axis
-e, --exclude TEXT          exclude matching variable names
--lw N                      set the gnuplot linewidth
--lt N                      set the gnuplot linetype
--eps, --epsfile FILENAME   output to an encapsulated postscript file
--png, --pngfile FILENAME   output to a portable network graphics file"""


def main ():
	# Set defaults for the command-line options.
	desired_node = None
	desired_run = None
	eps_filename = None
	png_filename = None
	xlabel = "Day"
	xrange = ('', '')
	ylabel = ""
	yrange = ('', '')
	linewidth = 3
	linetype = 1
	excluded_varnames = []

	# Get any command-line arguments.
	try:
		opts, args = getopt.getopt (sys.argv[1:], "o:y:x:n:r:e:h", ["eps=",
		  "epsfile=", "outfile=", "png=", "pngfile=", "ylabel=", "xlabel=",
		  "yrange=", "xrange=", "node=", "run=", "lw=", "lt=", "exclude=",
		  "help"])
	except getopt.GetoptError, details:
		print details
		sys.exit()
		
	for o, a in opts:
		if o in ("-h", "--help"):
			usage()
			sys.exit()
		elif o in ("--eps", "--epsfile", "-o", "--outfile"):
			eps_filename = a
		elif o in ("--png", "--pngfile"):
			png_filename = a
		elif o in ("-y", "--ylabel"):
			ylabel = a
		elif o in ("-x", "--xlabel"):
			xlabel = a
		elif o == "--yrange":
			yrange = tuple (a.split(':'))
		elif o == "--xrange":
			xrange = tuple (a.split(':'))
		elif o in ("-n", "--node"):
			desired_node = int(a)
		elif o in ("-r", "--run"):
			desired_run = int(a)
		elif o == "--lw":
			linewidth = int(a)
		elif o == "--lt":
			linetype = int(a)
		elif o in ("-e", "--exclude"):
			excluded_varnames = a.split(",")

	if len (args) >= 1:
		desired_varnames = args[0].split(",")
	else:
		desired_varnames = ["num-units-in-each-state:" + statename for statename in STATE_NAMES]

	# Read the header line.
	header = sys.stdin.readline().strip()
	varnames = header.split(",")[2:]

	# Find the indices of the desired variables.
	indices = []
	for i in range (len (varnames)):
		keep = False
		for desired_varname in desired_varnames:
			if varnames[i].find (desired_varname) >= 0:
				keep = True
		for excluded_varname in excluded_varnames:
			if varnames[i].find (excluded_varname) >= 0:
				keep = False
		if keep:
			indices.append (i)

	# We're going to build a set of Line objects.  They will be stored in a
	# dictionary, keyed by output variable name.
	lines = {}
	for i in indices:
		lines[varnames[i]] = Line (varnames[i])

	# Go through the results table.
	for line in sys.stdin:
		# Pick off the run # and day.  The remainder of the line is output
		# variable values.
		dummy, day, fields = line.strip().split(",", 2)
		day = int (day)

		j = 0
		for i in indices:
			# Advance to the ith output variable value.  NB: don't try to use a
			# "split" here to split the entire input line at once, you'll get a
			# "maximum recursion limit exceeded" error for big files.
			while j <= i:
				# If the line begins with a quoted string, remove the quoted
				# string (because it might have commas inside it that will mess
				# us up).  NB: don't try to use a regular expression match
				# here, you'll get a "maximum recursion limit exceeded" error
				# for big files.
				if fields.startswith ('"'):
					end = fields.find ('"', 1)
					fields = fields[end + 1:]
				varvalue, fields = fields.split(",", 1)
				j += 1
			varname = varnames[i]
			if varvalue != "":
				try:
					lines[varname].append ((day, float (varvalue)))
				except ValueError:
					lines[varname].append ((day, 0))

	# Check whether the graph actually contains any points.
	no_data = (len(lines) == 0)
	if no_data:
		xrange = ("0","1")
		yrange = ("0","1")

	#plot = sys.stdout
	plot = os.popen ("gnuplot -persist", "w")
	plot.write ("set xlabel '%s'\n" % xlabel)
	plot.write ("set xrange [%s:%s]\n" % xrange)
	plot.write ("set ylabel '%s'\n" % ylabel)
	plot.write ("set yrange [%s:%s]\n" % yrange)
	if no_data:
		plot.write ("set noxtics\n")
		plot.write ("set noytics\n")
	if eps_filename != None:
		plot.write ("set terminal postscript eps color 16\n")
		plot.write ("set output '%s'\n" % eps_filename)
	if png_filename != None:
		plot.write ("set terminal png large\n")
		plot.write ("set output '%s'\n" % png_filename)

	# Gnuplot requires that you plot each line twice: once for the line and
	# once for the errorbars.
	cmd = []
	for i in indices:
		varname = varnames[i]
		cmd.append ("'-' title '%s' w lines lt %i lw %i" % (lines[varname].title, i+linetype, linewidth))
		cmd.append ("'-' notitle w errorbars lt %i" % (i+linetype))
	if len(cmd) > 0:
		plot.write ("plot " + string.join (cmd, ",") + "\n")

	for i in indices:
		line = lines[varnames[i]]
		if len (line) > 0:
			line.medianx (quantiles=True)
			# Sort the points in the line by x-coordinate.
			line.sort()
			for i in range (2):
				for point in line:
					plot.write ("%g %g %g %g\n" % point)
				plot.write ("e\n")
		else:
			for i in range (2):
				plot.write ("0 0\n")
				plot.write ("e\n")		

	if no_data:
		plot.write ("set label 'No output variable found to create this plot' at 0.5,0.5 center\n")
		plot.write ("plot 1 notitle lt -1\n")

	plot.close()



if __name__ == "__main__":
	main()
